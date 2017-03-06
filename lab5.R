# estimate p(x<=5) for x~n(0,1) and s is in set {-10, -5, -2, 2, 5, 10}
# then compare results with built-in function pnorm(s) and analyze the error
# the two types of errors are absolute and relative errors

runs <- 1e+6
s <- c(-10, -5, -2, 2, 5, 10)
sample <- rnorm(runs)
mc.est <- apply(sapply(sample, function(x) x<=s), 1, sum) / runs
print(mc.est)

# absolute error
abs(mc.est - pnorm(s))

# relative error
abs(mc.est - pnorm(s)) / pnorm(s)

# why is this the case?
# if we want to estimate, say, E, and the estimator is E + epsilon
# when E << 1, relative error epsilon / E cannot be neglected
# when we analyze "tail events", relative error may be significant
sample <- rnorm(10)
sapply(sample, function(x) x<=s)
sample(1)
sample[1]

#blackscholes
bs <- function(S, X, rf, T, sigma) {
  values <- c(2)
  
  d1 <- (log(S/X) + (rf+sigma^2/2)*T) / (sigma*sqrt(T))
  d2 <- d1 - sigma * sqrt(T)

  values[1] <- S * pnorm(d1) - X * exp(-rf*T) * pnorm(d2)
  values[2] <- X * exp(-rf*T) * pnorm(-d2) - S * pnorm(-d1)
  
  values
}
bs(200, 100, 0.05, 0.02, 0.5)
