#* code section to run before script
euler_method <- function(h,theta_0,t_max) {
  t_vals <- seq(0,t_max, by= h)
  
  theta_vals <- numeric(length(t_vals))
  theta_vals[1] <- theta_0
  
  for (i in 2:length(t_vals)) {
    t <- t_vals[i-1]
    theta_2 <- theta_vals[i-1]
    dtheta_dt <- cos(4*t) -2 * theta_2
    theta_vals[i] <- theta_2 + h * dtheta_dt
  }
  return (data.frame(t=t_vals,theta=theta_vals))
}
runge_kutta <- function(h,theta_0,t_max) {
  t_vals <- seq(0,t_max, by= h)
  
  theta_vals <- numeric(length(t_vals))
  theta_vals[1] <- theta_0
  
  for (i in 2:length(t_vals)) {
    t <- t_vals[i-1]
    theta_2 <-theta_vals[i-1]
    
    k1 <- h * (cos(4*t) - 2 * theta_2)
    k2 <- h * (cos(4 * (t + h /2)) -2 * (theta_2 + k1 / 2))
    k3 <- h * (cos(4 * (t + h /2)) -2 * (theta_2 + k2 /2))
    k4 <- h * (cos(4 * (t + h)) -2 * (theta_2 + k3))
    theta_vals[i] <- theta_2 + (1/6) * (k1 + 2 * k2 + 2 * k3 + k4)
  }
  return (data.frame(t=t_vals,theta=theta_vals))
}

exact <- function(t) {
  return(0.1 * cos(4*t) +0.2 *sin(4*t) + 2.9 * exp(-2*t))
}

rel_err <- function(exact,estimate) {
  return(abs(exact - estimate) / abs(exact))
}
ODEsolver <- function() {
  cat("Choose a method:\n1 Euler's method\n2.Runge-Kutta 4th order\n")
  choice <- as.numeric(readline(prompt="Enter choice (1-2): "))
  
  while (is.na(choice) || choice < 1 || choice >2) {
    cat("Error, enter 1 or 2\n")
    cat("Choose a method:\n1 Euler's method\n2. Runge-Kutta 4th order\n")
    choice <- as.numeric(readline(prompt="Enter choice (1-2): "))
  } 
  
  cat("Step size(s):\n1. h = 0.8\n2. h = 0.2\n3. h = 0.05\n")
  choice_2 <- as.numeric(readline(prompt="Enter step size (0.8,0.2,0.05): "))
  
  while (is.na(choice_2) || (choice_2 != 0.8 && choice_2 != 0.2 && choice_2 != 0.05)) {
    choice_2 <- as.numeric(readline(prompt="Error, enter (0.8, 0.2, or 0.05): "))
  }
  if (choice_2 == 0.8) {
    h <- 0.8
  }
  if (choice_2 == 0.2) {
    h <- 0.2
  }
  if (choice_2 == 0.05) {
    h <- 0.05
  }
  
  theta_0 <- 3
  t_max <- 2
  
  if(choice == 1) {
    final <- euler_method(h, theta_0, t_max)
  }
  if (choice == 2) {
    final <- runge_kutta(h, theta_0, t_max)
  }
  
  exact_v <- exact(final$t)
  rel_eror <- rel_err(exact_v,final$theta)
  rel_error <- rel_eror * 100
  final_table <- data.frame(
    Time = final$t,
    "Exact_Temp(C)" = round(exact_v,3),
    "Estimated_Temp(C)" = round(final$theta,3),
    "Relative_error" = round(rel_error,2)
    )
  print(final_table)
}

#Main method call at the end
ODEsolver()
