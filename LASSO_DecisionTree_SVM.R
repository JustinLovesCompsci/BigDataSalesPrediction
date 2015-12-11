require(dplyr)
require(magrittr)
require(lubridate)
require(ggplot2)
require(randomForest)
require(kernlab)
require(glmnet)

train <- read.csv("train.csv")
test <- read.csv("test.csv")
store <- read.csv("store.csv")
joined <- left_join(train,store) %>% mutate(Year = year(Date),Month = month(Date),Day = day(Date)) #left join test and store

for (i in c("DayOfWeek","Open","Promo","StateHoliday","SchoolHoliday","StoreType","Assortment","Promo2","Year","Month","Day")){
  joined[,i] <- as.factor(joined[,i])
}

#Plot of sales#
#Input: the store number
#Output: a plot of the store sales over time
plot_sales <- function(store){
  data <- joined %>% group_by(Store) %>% arrange(Year,Month,Day)  #group by store and order by date
  store_data <- subset(data,Store==store) #subset the data for the input store
  ggplot(store_data,aes(rownames(store_data),y=Sales,group=1))+geom_area()+xlab("Time")  #plot the sales as a function of time
}

###################################Regression########################################
#Exploratory plots#
ggplot(joined,aes(x=log(Sales),fill="red"))+geom_histogram(colour="black",binwidth=0.1)
ggplot(joined,aes(x=log(CompetitionDistance),fill="red"))+geom_histogram(colour="black",binwidth=0.5)

#Lasso
joined <- na.omit(joined)
x <- model.matrix(~DayOfWeek+Month+Promo+StateHoliday+SchoolHoliday+StoreType+Assortment+CompetitionDistance,joined)
x <- x[,-1] #remove intercept 
y <- as.matrix(log(joined$Sales+1))
lasso <- cv.glmnet(x = x, y = y,alpha = 1,standardize=TRUE)
plot(lasso) #plot lasso
lambda_min <- lasso$lambda.min #find the lambda that  minimizes the cv error
coef <- coef(lasso, s = "lambda.min") #find the coefficients under lambda min
pred <- predict(lasso,x,s = "lambda.min")
RMSE <- (sum((estimated-log(joined_sample$Sales+1))^2)/length(log(joined_sample$Sales+1)))^(1/2)
plot(log(joined$Sales+1),pred,pch=19,xlab="Actual Values",ylab="LASSO Estimates",main="Estimates vs. Actual Values")


#####################################Classification#####################################
#Classify stores by sales
summary <- summary(joined$Sales)
third_quantile <- summary[2]
first_quantile <- summary[4]
joined$SalesType <- matrix(NA,dim(joined)[1],1)
joined[which(joined$Sales < third_quantile),"SalesType"] = "Third"
joined[which(joined$Sales > first_quantile),"SalesType"] = "First"
joined[which(joined$Sales >= third_quantile & joined$Sales <= first_quantile),"SalesType"] = "Second"

#Linear support vector machine
sample <- sample(seq(1,dim(joined)[1]),20000,replace=FALSE) #sample 20000 
joined_sample <- joined[sample,] #sample 20000 from the training data
#train the support vector machine model using the sample data
svm <- ksvm(SalesType~DayOfWeek+Month+Promo+StateHoliday+SchoolHoliday+StoreType+Assortment+log(CompetitionDistance)+Promo2,data = joined_sample)
pred <- predict(svm,joined) #prediction using the svm
length(which(pred != joined$SalesType))/dim(joined)[1] #calculate the error rate - 27.6%

#Nonlinear support vector machine (reduces the error rate to 22.6%)
nonlinear <- ksvm(SalesType~DayOfWeek+Month+Promo+StateHoliday+SchoolHoliday+StoreType+Assortment+log(CompetitionDistance)+Promo2,data = joined_sample, type="C-svc",kernel="rbf",kpar=list(sigma=1),C=1)
nonlinear_pred <- predict(nonlinear,joined)
length(which(nonlinear_pred != joined$SalesType))/dim(joined)[1] #calculate the error rate - 28.0%

#####################################Tree##############################################
tree <- randomForest(log(Sales+1)~DayOfWeek+Month+Promo+StateHoliday+SchoolHoliday+StoreType+Assortment+CompetitionDistance+Promo2, data=joined_sample)
estimated <- predict(tree,data=joined_sample)
RMSE <- (sum((estimated-log(joined_sample$Sales+1))^2)/length(log(joined_sample$Sales+1)))^(1/2)
qqnorm(estimated-log(joined_sample$Sales+1))#/sd(estimated-log(joined_sample$Sales)))
qqline(estimated-log(joined_sample$Sales+1))#/sd(estimated-log(joined_sample$Sales)))
plot(log(joined_sample$Sales+1),estimated-log(joined_sample$Sales+1),xlab="Actual Values",ylab="Residuals",main="Residuals vs. Competition Distance",pch=19)
plot(log(joined_sample$Sales+1),estimated,pch=19,xlab="Actual Values",ylab="Random Forest Estimates",main="Estimates vs. Actual Values")



