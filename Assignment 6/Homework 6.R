#4.
  #x = the data
  #starting_value = a starting value for the location parameter
  #tolerance_level = a stopping criterion
    #Arbitrarily picked at 1e-10
  mle <- function(x, starting_value, tolerance_level=1e-10) {
    theta_0 <- starting_value
    d_log_likelihood <- function(x,theta_0) {
      return(sum((2*(x-theta_0)) / ((x-theta_0)^2 + 1)))
    }
    d2_log_likelihood <- function(x, theta_0) {
      return(sum((2*((x-theta_0)^2 - 1)) / ((x-theta_0)^2 +1)^2))
    }
    theta_1 <- theta_0 - d_log_likelihood(x,theta_0)/d2_log_likelihood(x,theta_0)
    iteration_counter <- c(0) #Just out of curiosity
    while(abs(d_log_likelihood(x,theta_0)) > tolerance_level) {
      theta_0 <- theta_1
      theta_1 <- theta_0 - d_log_likelihood(x,theta_0)/d2_log_likelihood(x,theta_0)
      iteration_counter <- iteration_counter + 1 
    }
    result <- list("Iteration Counter" = iteration_counter, "ML Estimate" = theta_1)
    return(result)
  }
  
#5.
  x <- c(-2.09, -2.68, -1.92, -1.76, -2.12, 2.21, 1.97, 1.61, 1.99, 2.18)
  cat("The ML estimate when the starting value is -2 is", mle(x,-2)$"ML Estimate")
  cat("The ML estimate when the starting value is -1 is", mle(x,-1)$"ML Estimate")
  cat("The ML estimate when the starting value is 0 is", mle(x,0)$"ML Estimate")
  cat("The ML estimate when the starting value is 1 is", mle(x,1)$"ML Estimate")
  cat("The ML estimate when the starting value is 2 is", mle(x,2)$"ML Estimate")
  
#6.
  one_step_mle <- function(x) {
    theta_0 <- median(x)
    d_log_likelihood <- sum((2*(x-theta_0)) / ((x-theta_0)^2 + 1))
    d2_log_likelihood <- sum((2*((x-theta_0)^2 - 1)) / ((x-theta_0)^2 +1)^2)
    theta_1 <- theta_0 - d_log_likelihood/d2_log_likelihood
    return(theta_1)
  }   
  
#7.
  
  #Modifying function from #4
  mle <- function(x, tolerance_level=1e-10) {
    theta_0 <- median(x)
    d_log_likelihood <- function(x,theta_0) {
      return(sum((2*(x-theta_0)) / ((x-theta_0)^2 + 1)))
    }
    d2_log_likelihood <- function(x, theta_0) {
      return(sum((2*((x-theta_0)^2 - 1)) / ((x-theta_0)^2 +1)^2))
    }
    theta_1 <- theta_0 - d_log_likelihood(x,theta_0)/d2_log_likelihood(x,theta_0)
    iteration_counter <- c(0) 
    while(abs(d_log_likelihood(x,theta_0)) > tolerance_level) {
      theta_0 <- theta_1
      theta_1 <- theta_0 - d_log_likelihood(x,theta_0)/d2_log_likelihood(x,theta_0)
      iteration_counter <- iteration_counter + 1
      #In case of infinite loop:
      if (iteration_counter > 10000) {
        break
      }
    }
    return(theta_1)
  }
  
  #n=10
  n_10 <- list()
  for (i in 1:1000) {
    draw <- rcauchy(10, location=0, scale=1)
    n_10[[i]] <- draw
  }
  mle_variance_n_10 <- var(sapply(n_10, mle))
  cat("The variance for the maximum likelihood estimate when n=10 is", mle_variance_n_10)
  one_step_mle_variance_n_10 <- var(sapply(n_10, one_step_mle))
  cat("The variance for the one-step maximum likelihood estimate when n=10 is", one_step_mle_variance_n_10)
  
  #n=100
  n_100 <- list()
  for (i in 1:1000) {
    draw <- rcauchy(100, location=0, scale=1)
    n_100[[i]] <- draw
  }
  mle_variance_n_100 <- var(sapply(n_100, mle))
  cat("The variance for the maximum likelihood estimate when n=100 is", mle_variance_n_100)
  one_step_mle_variance_n_100 <- var(sapply(n_100, one_step_mle))
  cat("The variance for the one-step maximum likelihood estimate when n=100 is", one_step_mle_variance_n_100)
  
  #n=1000
  n_1000 <- list()
  for (i in 1:1000) {
    draw <- rcauchy(1000, location=0, scale=1)
    n_1000[[i]] <- draw
  }
  mle_variance_n_1000 <- var(sapply(n_1000, mle))
  cat("The variance for the maximum likelihood estimate when n=1000 is", mle_variance_n_1000)
  one_step_mle_variance_n_1000 <- var(sapply(n_1000, one_step_mle))
  cat("The variance for the one-step maximum likelihood estimate when n=1000 is", one_step_mle_variance_n_1000)
  
  