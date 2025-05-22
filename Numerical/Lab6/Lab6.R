y0 <- 1.241
x0 <- 0
t_max <- 6

dydt <- function(x,y) {
  -y*cos(x)
}

eulerMethod <- function(dydt, y0, x, h) {
  y <- numeric(length(x))
  y[1] <- y0
  for (i in 2:length(x)) {
    y[i] <- y[i-1] + h * dydt(x[i-1],y[i-1])
  }
    return(y)
}


h <- 0.1
x<- seq(0, t_max, by= h)
y_estimate <- eulerMethod(dydt, y0, x, h)

plot(x,y_estimate, type ="l",col = "green", xlab = "Time(t)", ylab = "Displacement (y)", 
     main = "Euler method with h=(0.1, 0.25, 0.5)",ylim = c(-2,4))
grid()
h <- 0.25
x<- seq(0, t_max, by= h)
y_estimate <- eulerMethod(dydt, y0, x, h)
lines(x,y_estimate,col = "red")

h <- 0.5
x<- seq(0, t_max, by= h)
y_estimate <- eulerMethod(dydt, y0, x, h)
lines(x,y_estimate,col = "blue")

# analytical solution
h <- 0.5
sol<- function(x) {
  return (0.5 * exp(sin(2 * t)) * exp(-sin(t)))
}

x_val <-seq(0, t_max, by= 0.5)
y_analytical <- sol(x_val)
lines(x_val,y_analytical, col = "purple")
y_est <- eulerMethod(dydt, y0, x, 0.5)
  
#calculate errors
abs_err <- abs(y_est - y_analytical)
rel_err <- (abs_err / abs(y_analytical)) * 100
error_df <-data.frame(Absolute_Error = abs_err, Relative_Error = rel_err)
print(error_df)

