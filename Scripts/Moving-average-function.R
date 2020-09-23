# Moving Average function
mov_avg <- function(x, n) { 
  # x: series vector
  # n: size for the average
  ans <- numeric()
  for (i in 1:(length(x)-n+1)) {
    ans[i] <- mean(x[i:(i+n-1)])
  }
  return(c(rep(NA, (n-1)/2), ans, rep(NA, (n-1)/2)))
}

# test
# exa <- round(runif(10))
# mov_avg(exa, 3)
# mov_avg(exa, 7)
