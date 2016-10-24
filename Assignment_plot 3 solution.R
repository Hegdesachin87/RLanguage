# Scatterplot matrix of DMC,DC,wind,rain,temp

data_1 = read.csv("C:/Users/ENVY X360/Desktop/R/forestfires.csv")


pairs(~DMC+DC+wind+rain+temp,data=data_1, 
      main="Scatterplot Matrix")
attach(data_1)
dev.copy(pdf,"scatterplot.pdf")
dev.off()

dev.copy(png,"scatterplot.png")
dev.off()

# 3D Scatterplot of wind,rain,area
library(scatterplot3d)

scatterplot3d(wind,rain,area,main="3D Scatterplot")
dev.copy(pdf,"3D_scatterplot.pdf")
dev.off()

# Interactive 3D Scatterplot of wind,rain,area
library(rgl)
plot3d(wind,rain,area, col="red", size=3)
dev.copy(pdf,"3D_interactiveplot.pdf")
dev.off()

# Boxplot of X and Y
boxplot(X~Y,data=data_1, main="Boxplot", 
        xlab="X", ylab="Y")
dev.copy(pdf,"Boxplot.pdf")
dev.off()

# Simple bar plot of temp, wind, rain [horizontal and vertical]
  #Horizantal
counts =table(data_1$temp)
barplot(counts, main="Temperature Distribution", horiz=TRUE)

counts =table(data_1$wind)
barplot(counts, main="Temperature Distribution", horiz=TRUE)

counts =table(data_1$rain)
barplot(counts, main="Temperature Distribution", horiz=TRUE)

   #Vertical
counts =table(data_1$temp)
barplot(counts, main="Temperature Distribution", 
        xlab="Temp")

counts =table(data_1$wind)
barplot(counts, main="Temperature Distribution", 
        xlab="Temp")

counts =table(data_1$rain)
barplot(counts, main="Temperature Distribution", 
        xlab="Temp")

# Grouped bar plot of X and Y

counts <- table(data_1$X, data_1$Y)
barplot(counts, main="Distribution by X and Y",
        xlab="X", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
dev.copy(pdf,"groundbar_plot.pdf")
dev.off()

dev.copy(png,"groundbar_plot.png")
dev.off()

# Histogram of probability distribution of X, Y, wind, temp, area along with line density
hist(data_1$X, 
     main="Histogram for X", 
     xlab="X", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=5, 
     prob = TRUE)
lines(density(data_1$X))

hist(data_1$Y, 
     main="Histogram for Y", 
     xlab="Y", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=5, 
     prob = TRUE)
lines(density(data_1$Y))

hist(data_1$wind, 
     main="Histogram for wind", 
     xlab="wind", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=5, 
     prob = TRUE)
lines(density(data_1$wind))

hist(data_1$temp, 
     main="Histogram for temp", 
     xlab="temp", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=5, 
     prob = TRUE)
lines(density(data_1$temp))

hist(data_1$area, 
     main="Histogram for area", 
     xlab="area", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=5, 
     prob = TRUE)
lines(density(data_1$area))

# Histogram of frequency distribution of X, Y, wind, temp, area
hist(data_1$X,main="Histogram for X",xlab="X",col = "Blue")
hist(data_1$Y,main="Histogram for Y",xlab="Y",col = "Blue")
hist(data_1$wind,main="Histogram for wind",xlab="wind",col = "Blue")
hist(data_1$temp,main="Histogram for temp",xlab="temp",col = "Blue")

# Pie Chart of area, wind, rain, temp by month
data_1$month

data_1_pivot <- summarise(group_by(data_1,month),area=sum(area))
slices <- data_1_pivot[["area"]]
pie(slices, labels=data_1[["month"]], main="Pie Chart of area")


data_1_pivot <- summarise(group_by(data_1,month),wind=sum(wind))
slices <- data_1_pivot[["wind"]]
pie(slices, labels=data_1[["month"]], main="Pie Chart of wind")

data_1_pivot <- summarise(group_by(data_1,month),rain=sum(rain))
slices <- data_1_pivot[["rain"]]
pie(slices, labels=data_1[["month"]], main="Pie Chart of rain")

data_1_pivot <- summarise(group_by(data_1,month),temp=sum(temp))
slices <- data_1_pivot[["temp"]]
pie(slices, labels=data_1[["month"]], main="Pie Chart of temp")


# Pie Chart of area, wind, rain, temp by day
library(dplyr)
data_1_pivot <- summarize(group_by(data_1,day),area=sum(area))
slices <- data_1_pivot[["area"]] 
pie(slices, labels=data_1[["day"]], main="Pie Chart of area")


data_1_pivot <- summarize(group_by(data_1,day),wind=sum(wind))
slices <- data_1_pivot[["wind"]] 
pie(slices, labels=data_1[["day"]], main="Pie Chart of wind")

data_1_pivot <- summarize(group_by(data_1,day),rain=sum(rain))
slices <- data_1_pivot[["rain"]] 
pie(slices, labels=data_1[["day"]], main="Pie Chart of rain")

data_1_pivot <- summarize(group_by(data_1,day),temp=sum(temp))
slices <- data_1_pivot[["temp"]] 
pie(slices, labels=data_1[["day"]], main="Pie Chart of temp")

# Map Plot of sourceAirportID

airports <- read.csv("C:/Users/ENVY X360/Desktop/R/airports.dat")
head(airports)
colnames(airports) <- c("ID", "name", "city", "country", "IATA_FAA", "ICAO", "lat", "lon", "altitude", "timezone", "DST")
head(airports)

routes <- read.csv("C:/Users/ENVY X360/Desktop/R/routes.dat")
colnames(routes) <- c("airline", "airlineID", "sourceAirport", "sourceAirportID", "destinationAirport", "destinationAirportID", "codeshare", "stops", "equipment")
head(routes)

library(plyr)
departures <- ddply(routes, .(sourceAirportID), "nrow")
names(departures)[2] <- "flights"
arrivals <- ddply(routes, .(destinationAirportID), "nrow")
names(arrivals)[2] <- "flights"

airportA <- merge(airports, departures, by.x = "ID", by.y = "sourceAirportID")

# install.packages("ggmap")
library(ggmap)
map <- get_map(location = 'World', zoom = 4)

mapPoints <- ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = sqrt(flights)), data = airportA, alpha = .5)


# Map Plot of destinationAirportID
airportB <- merge(airports, arrivals, by.x = "ID", by.y = "destinationAirportID")

library(ggmap)
map <- get_map(location = 'World', zoom = 4)

mapPoints <- ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = sqrt(flights)), data = airportB, alpha = .5)

mapPoints
dev.copy(pdf,"map.pdf")
dev.off()
