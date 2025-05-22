#change the working directory to the correct one
a0_best <- 0
a1_best <- 0
model <- ""
bestFit <- function() {
library(readxl)
  cat("Menu \n 1. Best Fit \n 2. Quit\n")
  Menu1_Choice <-as.numeric(readline(prompt = "enter choice: "))
  
  while (is.na(Menu1_Choice) || Menu1_Choice < 1 || Menu1_Choice > 2) {
    cat("Error, enter 1 or 2\n")
    cat("\nMenu \n 1. Best Fit \n 2. Quit\n")
    Menu1_Choice <-as.numeric(readline(prompt = "enter Choice: "))
    
  }
  if (Menu1_Choice == 2) {
    return("....Exiting....")
  } else {
    # get user to input file name and check the correct conditions
    fileName <- readline(prompt = "Please enter the name of the file to open: ")
      while(!file.exists(fileName)) {
        cat("File doesn't exist,  please enter the name of the file to open:\n")
        fileName <- readline(prompt = "Enter the file name: ")
      }
    #read
    rocket_data <- read_excel(fileName)
    print.data.frame(rocket_data)
    Task1(rocket_data)
  }
}

Task1 <-function(rocket_data) {
  #initialized data frame values
  time <- rocket_data$`t-sec`
  distance <- rocket_data$`d-meter`
  #extra
  n_terms <- 21 # num of terms in data frame
  #power model math
  log_time <- log(time)
  log_distance <- log(distance)
  log_mul <- log_time * log_distance
  exponent <- log_time^2
  #intitalize sum values
  time_sum <- 0
  distance_sum <- 0
  mul_sum <- 0
  exponent_sum <- 0
  
  time_sum <- sum(log_time)
  distance_sum <- sum(log_distance)
  mul_sum <- sum(log_time * log_distance)
  exponent_sum <- sum(log_time^2)
    #power model exponents y = a*x^b
  a1_pow <- (n_terms * mul_sum - time_sum * distance_sum) / (n_terms * exponent_sum - time_sum^2)
  a0_pow <- exp((distance_sum / n_terms) - a1_pow*(time_sum / n_terms))
  
  #residual calculation power y = a*x^b
  d_pow <- a0_pow * time^a1_pow
  residual_pow <- distance - d_pow
  SR_pow <- sum(residual_pow^2)

  #Exponential model exponents y = a*e^bx
  time_sum2 <- sum(time)
  exponent_sum2 <- sum(time^2)
  a1_exp <- (n_terms * mul_sum - time_sum2 * distance_sum) / (n_terms * exponent_sum2 - time_sum2^2)
  a0_exp <- exp((distance_sum / n_terms) - a1_exp*(time_sum2 / n_terms))
  
  #residual calculation power y = a*e^bx
  d_exp <- a0_exp * exp(a1_exp * time)
  residual_exp <- distance - d_exp
  SR_exp <- sum(residual_exp^2)
  
  cat("\nPower Model: \n")
  cat("d = a * t^b\n")
  cat("SR = ", SR_pow, "\n")
  
  cat("Exponential Model: \n")
  cat("D = a * e^(b*t)\n")
  cat("SR = ", SR_exp,  "\n")
  
  if (SR_pow > SR_exp) {
    a0_best <<- a0_exp
    a1_best <<- a1_exp
    model <<- "exponential"
    print("The best fit model is the exponential model")
  } else{
    a0_best <<- a0_pow
    a1_best <<- a1_pow
    model <<- "power"
    print("The best fit model is the power model")
  }
  cat("Menu \n 1. Extrapolation \n 2. Main Menu\n")
  Menu2_Choice <-as.numeric(readline(prompt = "enter choice: "))
  
  while (is.na(Menu2_Choice) || Menu2_Choice < 1 || Menu2_Choice > 2) {
    cat("Error, enter 1 or 2\n")
    cat("Menu \n 1. Extrapolation \n 2. Main Menu\n")
    Menu2_Choice <-as.numeric(readline(prompt = "enter Choice: "))
  }
  if (Menu2_Choice == 2 ) {
    bestFit()
  } else {
    value <- as.numeric(readline(prompt = "Enter value to extrapolate: " ))
    while (is.na(value)) {
      cat("Error, enter a number\n")
      value <- as.numeric(readline(prompt = "Enter value to extrapolate: " ))
    }
    if (model == "power") {
      final_val <- a0_best * value^a1_best
    } else if(model == "exponential") {
      final_val <- a0_best * exp(a1_best * value)
    }
    cat("Extrapolated value is ",final_val, "\n")
    
    plot(time,distance, xlab="Time(sec)", ylab="Distance(meter)", pch =19,col = "green")
    grid()
    if (model == "power") {
      fit <- a0_best * time^a1_best
      lines(time,fit, col = "blue", lwd =2)
    } else if(model == "exponential") {
      fit <- a0_best * exp(time*a1_best)
      lines(time,fit, col = "blue", lwd =2)
    }
    pdf("best_fit.pdf")
    plot(time,distance, xlab="Time(sec)", ylab="Distance(meter)", pch =19,col = "green")
    grid()
    if (model == "power") {
      fit <- a0_best * time^a1_best
      lines(time,fit, col = "blue", lwd =2)
    } else if(model == "exponential") {
      fit <- a0_best * exp(time*a1_best)
      lines(time,fit, col = "blue", lwd =2)
    }
    dev.off()
  }
}
#Main method call at the end
bestFit()

  

