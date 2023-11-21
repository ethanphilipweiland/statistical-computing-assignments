#Inputting data
Deaths <- c(0:9)
Frequency <- c(162,267,271,185,111,61,27,8,3,1)
data <- data.frame(Deaths, Frequency)

#EM algorithm
em_hw7 <- function(n_iter, alpha=0.3, mu_1=1, mu_2=2.5) {
  for (i in 1:n_iter) {
    #E step
    for (q in 1:nrow(data)) {
      if (q == 1) { 
        z <- c() 
      }
      numerator <- alpha * exp(-mu_1) * mu_1^data$Deaths[q]
      denominator <- alpha * exp(-mu_1) * mu_1^data$Deaths[q] + (1-alpha)*exp(-mu_2)*mu_2^data$Deaths[q]
      z[q] <- numerator/denominator
    }
    #M step
      alpha <- sum(data$Frequency*z) / sum(data$Frequency)
      mu_1 <- sum(data$Frequency*data$Deaths*z) / sum(data$Frequency*z)
      mu_2 <- sum(data$Frequency*data$Deaths*(1-z)) / sum(data$Frequency*(1-z))
    #Printing results
      if (i == n_iter) {
      cat("Alpha for iteration", i, "is", alpha, fill=TRUE)
      cat("Mu_1 for iteration", i, "is", mu_1, fill=TRUE)
      cat("Mu_2 for iteration", i, "is", mu_2, fill=TRUE)
      }
  }
}

#What are the maximum likelihood estimates after 1 iteration?
em_hw7(1)

#What are the maximum likelihood estimates after 2 iterations?
em_hw7(2)

#What are the maximum likelihood estimates after 3 iterations?
em_hw7(3)

#What are the maximum likelihood estimates after 4 iterations?
em_hw7(4)

#What are the maximum likelihood estimates after 5 iterations?
em_hw7(5)

#What are the maximum likelihood estimates after 100,000 iterations?
em_hw7(100000)
