# Task 1: CSV Files
CerealsDF <- read.csv("C:/College Work/Level 4/Numerical Computing/assignment1.csv",sep = ";")

str(CerealsDF)
#print first 10 rows
head(CerealsDF,10)

#delete the second row
CerealsDF <- CerealsDF[-2, ]

#print number of rows
num_rows <- nrow(CerealsDF)
#print number of columns
num_columns <- ncol(CerealsDF)

cat("Numbers of rows: ",num_rows, "\n")
cat("Number of columns: ",num_columns)

#convert each type to numeric use gsub because of NAs errors
CerealsDF$carbo <- as.numeric(gsub("[^0-9.-1]","",CerealsDF$carbo))
CerealsDF$sugars <-as.numeric(gsub("[^0-9.-1]","",CerealsDF$sugars))
#add them to new row added  
CerealsDF$totalCarbo <- as.numeric(CerealsDF$carbo) + as.numeric(CerealsDF$sugars)

#find the number of hot cereals
hot <- subset(CerealsDF, type == "H")
hot_cereal_num <- nrow(hot)
print(hot_cereal_num)

#find unique manufacturarers
unique_mfr <- unique(CerealsDF$mfr)
num_unique_mfr <- length(unique_mfr)
print(num_unique_mfr)
#extract cereals manufactured by kellogs
cereal_k <- subset(CerealsDF,mfr=="K")
print(cereal_k)

#extract cereal based on calories
extract_cereal <- subset(CerealsDF, calories <=90 & fat > 2)
write.csv(extract_cereal, file = "C:/College Work/Level 4/Numerical Computing/newDataFrame.csv",row.names = FALSE)

# interpolation function
LagrangeBasic<- function(x,i,x_val) {
  L_i <- 1
  for (j in 1:length(x_val)) {
    if (i != j) {
      L_i <- L_i * (x-x_val[j]) / (x_val[i] - x_val[j])
    }
  }
  return(L_i)
}
MyIntCal<- function(x, x_val, f_val) {
  n <- length(x_val)
  result <- 0
  for(i in 1:n) {
    L_i <- LagrangeBasic(x, i, x_val)
    result <- result + f_val[i] * L_i
  }
  return(result)
}

x_val <- c(pi, 6.678, 3*pi, 12.961, 5*pi, 19.244, 7*pi)
f_val <- c(0, -1.921, 0, 3.584, 0, -6.718, 0)

pdf("MyIntFig.pdf")
par(mfrow = c(4,2))

x_plot <- seq(min(x_val),max(x_val), length.out=400)

for(i in 1:length(x_val)) {
  y_plot <- sapply(x_plot, function(x) LagrangeBasic(x,i,x_val))
    plot(x_plot,y_plot, type= "l")
}

pf_x <- poly.calc(x_val, f_val)


y_final <- predict(pf_x, x_plot)
plot(x_plot,y_final, type = "l")

dev.off()

#using MyIntCal
check1 <- 15
check2 <- 24

if (check1>= 0 && check1 <= 7*pi) {
  f_MyInt_15 <- MyIntCal(check1, x_val, f_val)
  f_pf_15 <- predict(pf_x, check1)
  cat("MyIntCal result: f(15) = ",f_MyInt_15, "\n")
  cat("pf_x result: f(15) = ",f_pf_15, "\n")
} else {
  print("f(15) is beyond the range for interpolation")
}

if (check2 >= 0 && check2 <=  7*pi) {
  f_MyInt_24 <- MyIntCal(check2, x_val, f_val)
  f_pf_24 <- predict(pf_x, check2)
  cat("MyIntCal result: f(24) = ",f_MyInt_24, "\n")
  cat("pf_x result: f(24) = ",f_pf_24, "\n")
} else {
  print("f(24) is beyond the range x[0,7*pi] for interpolation")
}

