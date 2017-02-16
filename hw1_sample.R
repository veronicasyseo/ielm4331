# homework 1 question 3
# due friday february 17

price.BinaryTree <- function(s0, u, d = 1/u, r, payoff, timestep = 1, period = 1, collapse = TRUE, compound = "discrete") {
  
  # calculate the d/c factor with discrete / continuous compounding
  if (compound == "continuous")
    discount.factor <- exp(-r * timestep)
  else
    discount.factor <- 1 / (1 + r * timestep)
  
  # the risk-neutral probability
  probability.neural.up <- (1 / discount.factor - d) / (u - d)
  probability.neutral.down <- 1 - probability.neutral.up
  
  # adding collapsibility to tree
  if (collapse)
    if (length(payoff) == (period + 1)) {
      dis <- choose(period, 0:period) *
        probability.neutral.up ^ (0:period) * 
        probability.neutral.down ^ (period:0)
      discount.factor ^ period * sum(dis * payoff)
    }
  else
    stop("The payoff you provide is wrong when the middle nodes are collapsed")
  else {
    if (length(payoff) == 2 ^ period) {
      p <- c(probability.neutral.up, probability.neutral.down)
      dis <- p
      for (i in 1:(period - 1))
        dis <- rep(p, each = length(dis)) * dis
      discount.factor ^ period * sum(dis * payoff)
    }
    else
      stop("The payoff you provide is wrong when the middle nodes are not collapsed")
    }
  }

# test

period <- 2
f <- c(0, 1, 1, 0)
p <-
  price.BinaryTree(
    100,
    1.1,
    payoff = f,
    r = 0.05,
    period = period,
    collapse = FALSE,
    compound = "discrete"
  )
print (
  paste (
    "The value of this derivative in" , period , "period Binomial tree model is", p, sep = " "
  )
)