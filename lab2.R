# Given u, d, n
# write a function that will generate a vector similar to
# [u^n, u^(n-1)*d, u^(n-2)*d^2, ... , u^1*d^(n-1), d^n]
# append: use key word c() e.g. x <- c()

myfun <- function (u, d, n) {
  res = c()
  for (i in 0:n) {
    res <- c(res, u^(n-1)*d^i)
  }
  return (res)
}
myfun (0.1, 0.9, 3)
res

# some exercises
# pass an int to the a variable
# still saved in "numeric" format
a <- 10

# convert this into integer
a <- as.integer(a)
class(a)

name <- "veronica"
class(name)

# logical type
flag <- TRUE
class(flag)

# numeric vectors
v1 <- c(12, 24, 36, -23)
class(v1)

# char vectors
v2 <- c("hello", "world")
class(v2)

# logical vectors
v3 <- c(TRUE, FALSE, TRUE)
class(v3)

# combine vec1 and vec2
newV <- c(v1, v2)
class(newV) # returns character

newV

# constructing a matrix
rnames <- c("R1", "R2", "R3", "R4", "R5")
cnames <- c("C1", "C2", "C3", "C4", "C5")
matdata <- matrix(1:25, nrow=5,ncol=5, dimnames=list(rnames, cnames))
matdata

# a LIST is a sequence of data elements, like a vector, but can hold elements of different data types
# below, we will combine the vars created in the vector section
l1 <- list(v1, v2, v3)
l1
typeof(l1)

# FACTORS are categorical variables in R -- they take vals from a limited known set
# for some factor variables, R internally stores an equivalent integer value and maps it to the char string

# DATAFRAME is similar to a matrix, but its cols can hold data elements of different types
# this will be the most commonly used data type in analysis
??mtcars

# MTCARS is an example of an in-built  dataset that R comes with
