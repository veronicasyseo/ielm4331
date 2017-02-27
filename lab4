# explicit method
# Define basic variables
sigma <- 0.3
r <- 0.05
s0 <- 100
K <- 100
time <- 0.2
c <- 0.5*sigma^2

# grid param
a <- -20
b <- 20
N <- 1000
M <- 1000
dx <- (b/a)/N
dt <- time/M

# transformation param
alpha <- (r-sigma^2/2)*(sigma^2)
beta <- (r-sigma^2/2)^2/(2*sigma^2)-r

# create grid
u <- matrix(0, nrow = N+1, ncol = M+1)

# boundar cond for time = T
# u[0,t] refers to x=0, time=t -- row represents x and col represents time
# this is different from the lec notes, the unknown function is u(x,t), whereas here it will be u(x,T)
xN <- a+c(0:N)*dx
u[,M+1] <- exp(alpha*xN+beta*time) * pmax(exp(xN), K)
lambda <- c*dt/dx^2

for (n in M:1) {
  # boundary condition
  u[1,n] <- 0
  u[n+1, n] <-exp(alpha*b+beta*n*dt)*exp(b)
  for (j in 2:N) {
    u[j,n] <- lambda*u[j+1,n+1]+(1-2*lambda)*u[j,n+1]+lambda*u[j-1,n+1]
  }
}

print(u)
