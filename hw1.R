# u = up state, d = down state = 1/u, r = annualized I/R, s = security price at time t
# t = 1 (d/c by time increment of 1 every time), per = 2 (question states that this is a two-period binomial tree)

binomial_tree = function (u, d, r, s, t, per) {
  # create empty vector
  vec <- c()
  
  # discrete compounding
  dc = (1 + r)^(-t)
  
  # risk-neutral probability
  p = (1+r-d) / (u-d)
  q = 1 - p
  
  # calculate security price at time 1
  for (i in 1:(n+1)) {
    for (j in 1:i) {
      vec [i, j] = (s * u^(j-1) * p + u^(n - 1) * d^(n - 1) * s * p) * dc
    }
  }
  return (vec)
}

binomial_tree(1.1, 1/1.1, 0.05, 100, 1, 2)
