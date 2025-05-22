#install package to read excel
install.packages("readxl")
#call library
library(readxl)

#read the excel file
data <- read_excel("C:\\College Work\\Level 4\\Numerical Computing\\Lab5\\rocket.xlsx",)

# create two vectors and add the values from excel file 
xVec <- data$`t-sec`
yVec <- data$`d-meter`

#plot orignal
plot(xVec,yVec, type = "l", col = "green", xlab = "Time(s)", 
     ylab= "Distance(km)",main = "Time vs distance")
grid()

# CDD method function
CDD <- function(xVec, yVec) {
  l <- length(xVec)
  firstDev <- numeric(l)
  
  for (i in 2:(l-1)) {
    firstDev[i] <- (yVec[i+1] - yVec[i-1]) / (xVec[i+1] - xVec[i-1])  
  }
  return (firstDev)
}
#Accelarition func
secDEV <- function(xVec, yVec) {
  l <- length(xVec)
  secondDev <- numeric(l)
  
  for (i in 2:(l-1)) {
    secondDev[i] <- (yVec[i+1] - 2*yVec[i] + yVec[i-1]) / (xVec[i+1] - xVec[i])^2  
  }
  return (secondDev)
}
#Calculate velocity
vel <- CDD(xVec,yVec)
#convert to km/s
vel_final <- vel/1000

#plot velocity graph
plot(xVec,vel_final,type = "l",col = "red", xlab = "Time(s)", 
     ylab = "Velocity(km/s)", main = "Time vs Veclocity")
grid()

#plot second derivitve
secondDev <- secDEV(xVec,yVec)
#convert to km/s
secondDev_final <- secondDev / 1000

#plot accelaration graph
plot(xVec, secondDev_final, type = "l",col = "blue", xlab = "Time(s)", 
     ylab = "Accelaration(km/s^2)", main = "Time vs Accelaration")
grid()

