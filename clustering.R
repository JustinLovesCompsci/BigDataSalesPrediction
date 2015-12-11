## Intro ##

#install.packages("aod")
#install.packages("ggplot2")
#install.packages("glmnet")
#install.packages("mixtools")
#install.packages("dplyr")
#install.packages("magrittr")
#install.packages("lubridate")
#install.packages("randomForest")
#install.packages("kernlab")

library(aod)
library(ggplot2)
library(glmnet)
library(mixtools)
library(dplyr)
library(magrittr)
library(lubridate)
library(randomForest)
library(kernlab)
library(cluster)

joined <- left_join(train,store) %>% mutate(Year = year(Date),Month = month(Date),Day = day(Date))
joined <- na.omit(joined)
# Left join test and store

## Exploratory Data Analysis ##

# Input: the store number
# Output: a plot of the store sales over time
plot_sales <- function(store){
  data <- joined %>% group_by(Store) %>% arrange(Year,Month,Day)  #group by store and order by date
  store_data <- subset(data,Store==store) #subset the data for the input store
  ggplot(store_data,aes(rownames(store_data),y=Sales,group=1))+geom_area()+xlab("Time")  #plot the sales as a function of time
}

ggplot(joined,aes(x=log(Sales),fill="red"))+geom_histogram(colour="black",binwidth=0.1)
ggplot(joined,aes(x=log(CompetitionDistance),fill="red"))+geom_histogram(colour="black",binwidth=0.5)

## Sparse Regression ##

joined <- na.omit(joined)
x <- model.matrix(~DayOfWeek+Month+Promo+StateHoliday+SchoolHoliday+StoreType+Assortment+CompetitionDistance+Promo2,joined)
x <- x[,-1]
# Remove intercept 
y <- as.matrix(log(joined$Sales+1))
lasso <- cv.glmnet(x, y)
plot(lasso)
# Plot lasso
lambda_1se <- lasso$lambda.1se
# Find the lambda that  minimizes the cv error within 1 se
coef <- coef(lasso, s = "lambda.1se")
# Find the coefficients under lambda lse
pred <- predict(lasso, x, s = "lambda.1se")
residual <- log(y/pred)
plot(residual,y)

## Clustering ##

dataC <- cbind(log(joined$Sales+1),joined$DayOfWeek,joined$Month,joined$Promo,joined$StateHoliday,joined$SchoolHoliday,joined$StoreType,joined$Assortment,joined$CompetitionDistance,joined$Promo2)
dataC <- as.matrix(dataC)
clusters <- kmeans(dataC, 3)
clusters$centers
#plot(dataC, col = clusters$cluster)
#points(clusters$centers, col = 1:4, pch = 8, cex = 2)
#clusplot(dataC, clusters$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

## Mixed Model ##

statCalc <- function(value, list1, list2){
  temp <- NULL
  for (i in 1:length(list1)){
    if (list1[i] == value){
      temp <- c(temp, list2[i])
    }
  }
  vector <- c(mean(temp), sd(temp))
  return(vector)
}

v1 <- statCalc("a", joined$StoreType, joined$Sales)
v3 <- statCalc("c", joined$StoreType, joined$Sales)
v4 <- statCalc("d", joined$StoreType, joined$Sales)

m1 <- v1[1]
m3 <- v3[1]
m4 <- v4[1]

s1 <- v1[2]
s3 <- v3[2]
s4 <- v4[2]

regMM <- normalmixEM(dataC, mu = c(log(m1), log(m3), log(m4)), sigma = c(log(s1), log(s3), log(s4)))

v5 <- statCalc("a", joined$Assortment, joined$Sales)
v6 <- statCalc("c", joined$Assortment, joined$Sales)

m5 <- v5[1]
m6 <- v6[1]

s5 <- v5[2]
s6 <- v6[2]

regMMAssort <- normalmixEM(dataC, mu = c(log(m5), log(m6)), sigma = c(log(s5), log(s6)))

