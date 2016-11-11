setwd("C:/Users/ENVY X360/Desktop/R/RossmanSales")

train=read.csv("train.csv",header=T)
test=read.csv("test.csv",header=T)
store=read.csv("store.csv",header=T)

# merge the data set
train=merge(train,store)
test=merge(test,store)

# Analysing the data
str(train)
head(train)

summary(train)

# replacing NA's by the mean value

 munge_data <- function(dt){
  
  dt$CompetitionDistance[is.na(dt$CompetitionDistance)] = round(mean(dt$CompetitionDistance, na.rm = T))
  dt$CompetitionOpenSinceMonth[is.na(dt$CompetitionOpenSinceMonth)] = round(mean(dt$CompetitionOpenSinceMonth, na.rm = T))
  dt$CompetitionOpenSinceYear[is.na(dt$CompetitionOpenSinceYear)] = round(mean(dt$CompetitionOpenSinceYear, na.rm = T))
  dt$Promo2SinceWeek[is.na(dt$Promo2SinceWeek)] = round(mean(dt$Promo2SinceWeek, na.rm = T))
  dt$Promo2SinceYear[is.na(dt$Promo2SinceYear)] = round(mean(dt$Promo2SinceYear, na.rm = T))
  dt$Open[is.na(dt$Open)] = round(mean(dt$Open, na.rm = T))
  
  # converting to numeric
  dt$StateHoliday = as.numeric(dt$StateHoliday)
  dt$StoreType = as.numeric(dt$StoreType)
  dt$Assortment = as.numeric(dt$Assortment)
  dt$PromoInterval = as.numeric(dt$PromoInterval)
  
  # seperating out the elements of the date column for the dt set
  dt$Date = as.Date(dt$Date, format = "%Y-%m-%d")
  dt$month <- as.integer(format(dt$Date, "%m"))
  dt$year <- as.integer(format(dt$Date, "%y"))
  dt$day <- as.integer(format(dt$Date, "%d"))
  
  # removing the date columns (Date not used) (Customers affect simetry) (CompetitionOpenSinceYear not relevant/correlated)
  dt$Date = NULL
  dt$Customers = NULL
  dt$CompetitionOpenSinceYear = NULL
  
  return(dt)
}
train = munge_data(train);train
test = munge_data(test);test

cor(train)

#Splitting training and test data set
set.seed(100)
split_data=sample(1:nrow(train), 0.70*nrow(train), replace = F)
split_train=train[split_data,]
split_test=test[-split_data,]


mod = lm(Sales ~DayOfWeek+Open+Promo+StateHoliday+Assortment+CompetitionDistance+PromoInterval+year, data = split_train);mod


summary(mod)


#Analyzing p values

tapply(split_train$Sales,split_train$StoreType, mean)
tapply(split_train$Sales,split_train$CompetitionDistance, mean)
tapply(split_train$Sales,split_train$CompetitionOpenSinceMonth, mean)
tapply(split_train$Sales,split_train$Promo2, mean)
tapply(split_train$Sales,split_train$StateHoliday, mean)

prediction<-predict(mod, newdata=split_test)



actuals_predict <- data.frame(cbind(actuals=split_test$Sales, predict_new=prediction))

correlation_accuracy <- cor(actuals_predict)

min_max_accuracy <- mean(apply(actuals_predict, 1, min) / apply(actuals_predict, 1, max))

mape <- mean(abs((actuals_predict$predict_new - actuals_predict$actuals))/actuals_predict$actuals)

y=predict(mod,newdata=test)

submission=data.frame(Id=test$Id,sales=y)

