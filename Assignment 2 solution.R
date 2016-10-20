data=read.csv("C:/Users/ENVY X360/Desktop/R/forestfires.csv")

# 1.	Compute the square of each data point in the X column and store the result in a new column called "X_square"
X=data["X"]
x_square=X^2


# 2.	Compute the sum, mean, median, standard deviation of the following columns - 

# a.FMCC

FFMC=data$FFMC
sum_FFMC=sum(FFMC)
mean_FFMC=mean(FFMC)
std_FFMC=sd(FFMC)

# b.	DMC

DMC=data$DMC
sum_DMC=sum(DMC)
mean_DMC=mean(DMC)
std_DMC=sd(DMC)

# c.	DC

DC=data$DC
sum_DC=sum(DC)
mean_DC=mean(DC)
std_DC=sd(DC)

#3.	Create another column called "Month", which has full values of month, i.e "aug" becomes "August", "sep" becomes "September" and so on

data["month"]

month=data[data$month %in% c("aug","sep","oct"),]
month

#4.	Create another Column Day_Num where day will be from 1 to 7 - 1 being Sunday, 2 being Monday, 3 being Tuesday and so on
data["day"]
Day_Num=data[data$day %in% c("sun","mon","tue","wed","thu","fri","sat"),]

# 5.	Find the correlation coefficient between X and Y 
x=data$X
y=data$Y
correlation=cor(x,y);correlation

# 6.	Find the total rain,wind  for each month 
library("plyr")
sum_rain=ddply(data,.(month),summarize,tot=sum(rain))

sum_wind=ddply(data,.(month),summarize,tot=sum(wind))


#7.	Find the mean rain,wind  for each month 
library("plyr")
mean_rain = ddply( data, .(month), function(x) mean(x$rain))

mean_wind = ddply( data, .(month), function(x) mean(x$wind))

# 8.	Find the number of records present for each month

data.frame(table(data$month))

# 9.	Find the number of records for each month-day combo
as.data.frame(table(data[,c("month","day")]))
