# install package
install.packages("polynom")
require(polynom)

x<- polynomial(c(0,1))

p <- x^3 - 3*x^2 - 2*x + 7

print(p)

p_class <- class(p)
cat("The class of variable p is: ",p_class,"\n")

cofficent <- coef(p)
cat("cofficent's are: ",cofficent)

# y^2 +2*y

y <- polynomial(c(0,1))
q <- y^2 +2*y
print(q)

y_class <- class(y)
q_class <- class(q)

cat("class of variable y is: ",y_class,"\n")
cat("The class of variable q is: ",q_class,"\n")

p_plus_q <- p + q
p_minus_q <- p - q
p_times_q <- p * q

cat("p + q = ",p_plus_q)
cat("p - q = ",p_minus_q)
cat("p * q = ",p_times_q)

  dpdx <- deriv(p)
  dqdy<- deriv(q)
  
  cat("derivate of p is: ",dpdx)
  cat("derivate of q is: ",dqdy)
    
  #comvert to function so the curve function can take it as an argument
  p_func <- as.function(p)
  dpdx_func <- as.function(dpdx)
  
  #plot function
  curve(p_func, from = -2, to =3,col="blue",ylab="p(x), dpdx",main="Plot of p(x) and its deravitive")
  curve(dpdx_func, from =-2, to = 3, col ="red", add = TRUE)
  
  abline(h=0)
  
  #Part 2
  
  library(datasets)
  my_df <- airquality
  
  str(my_df)
  head(my_df)
  
  names(my_df)
  # install pacjage to use the select method so its possible to make data frame
  install.packages("dplyr")
  library(dplyr)
  my_df_temp <- select(my_df, Temp)
  
  
  june_temp <- filter(my_df, Month == 6)$Temp
  july_temp <- filter(my_df, Month == 7)$Temp
  august_temp <- filter(my_df, Month == 8)$Temp
  
  mean(june_temp)
  median(june_temp)
  sd(june_temp)
  
  all_temp <- my_df$Temp
  mean_temp <- mean(all_temp, na.rm = TRUE)
  sd_temp <- sd(all_temp, na.rm = TRUE)
  
  #probality of temp < 70
  pnorm(70, mean = mean_temp, sd = sd_temp)
  #probality of temp > 85
  1 - pnorm(85, mean = mean_temp, sd = sd_temp)
  #probality of 90 > temp < 75
  pnorm(90, mean=mean_temp, sd = sd_temp) - pnorm(75, mean=mean_temp, sd = sd_temp)
