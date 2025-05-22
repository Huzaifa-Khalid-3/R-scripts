f <- function(x) { # log function thats given
  return(x^2 * log(1+x)) 
}

# MacLaurin version of the function
McLaurin <- function(x, n = 10) {
  result <- 0     # store mcLaurin sum
  for (n in 1:n) {    #loop through
    term <- ((-1)^(n+1) * x^(n+2)) / n #do the calculation for mcLauruin
    result <- result + term # store answer in result
  }
  return (result)
}

#create vectors to store errors
abs_errors <- numeric(10)
rel_errors <- numeric(10)
MacLaurin_value <- 0

x <- as.numeric(readline(prompt = "Enter a number: ")) # get user number

real_val <- f(x) #calculate actual value (not maclaurin)

for (n in 1:10) {
  MacLaurin_value <- McLaurin(x,n)
  
  abs_errors[n] <- abs(real_val - MacLaurin_value)
  rel_errors[n] <- (abs_errors[n] / abs(real_val)) * 100
}

result <- data.frame(
  Absolute_Error = abs_errors,
  Relative_error = rel_errors
)
print(result)

curve(f, from =0, to = 1.5, col = "blue", lwd =4, ylab = "y", xlab="x")
curve(McLaurin(x,10), from =0, to = 1.5, add = TRUE, col = "red", lwd =2)




