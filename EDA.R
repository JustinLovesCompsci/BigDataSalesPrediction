#install.packages("dplyr")
#install.packages("magrittr")
#install.packages("lubridate")
#install.packages("ggplot2")
#install.packages("randomForest")
#install.packages("kernlab")

require(dplyr)
require(magrittr)
require(lubridate)
require(ggplot2)
require(randomForest)
require(kernlab)

train <- read.csv("train.csv")
test <- read.csv("test.csv")
store <- read.csv("store.csv")
joined <- left_join(train,store) %>% mutate(Year = year(Date),Month = month(Date),Day = day(Date)) #left join test and store
attach(joined)

#Plot of sales#
#Input: the store number
#Output: a plot of the store sales over time
plot_sales <- function(store){
  data <- joined %>% group_by(Store) %>% arrange(Year,Month,Day)  #group by store and order by date
  store_data <- subset(data,Store==store) #subset the data for the input store
  ggplot(store_data,aes(rownames(store_data),y=Sales,group=1))+geom_area()+xlab("Time")  #plot the sales as a function of time
}

##### EDA #####
#Cross Tabulation
table(SchoolHoliday, Promo)
table(SchoolHoliday, Promo2)
table(StateHoliday, Promo)
table(StateHoliday, Promo2)
table(Open, SchoolHoliday)
table(Open, StateHoliday)

#Boxplots
boxplot(Sales~SchoolHoliday, main="Sales by School Holiday") #does not vary
boxplot(Sales~StateHoliday, main="Sales by State Holiday") #vary significantly
boxplot(Sales~Assortment, main="Sales by Assortment") #vary significantly

#Correlation between Sales and Customers
cor(Sales, Customers) #high correlation = 0.895
logSales <- log(Sales)
logCustomers <- log(Customers)
logDistance <- log(CompetitionDistance)
plot(y=logSales, x=logCustomers, xlab = "Log of Customers", ylab = "Log of Sales") #roughly linear

#Histograms of Log Transformed Continuous Variables#
ggplot(joined,aes(x=log(Sales),fill="red"))+geom_histogram(colour="black",binwidth=0.1)+xlab("Log of Sales") #normally distributed
ggplot(joined,aes(x=log(CompetitionDistance),fill="red"))+geom_histogram(colour="black",binwidth=0.5)+xlab("Log of Competitor's Distance") #normally distributed
