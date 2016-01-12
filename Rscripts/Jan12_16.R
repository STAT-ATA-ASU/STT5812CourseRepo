# X ~ N(100, 10)
MU <- 100
SIGMA <- 10
#
f <- function(x, MU = 100, SIGMA = 10){
  (1/(sqrt(2*pi*SIGMA^2)))*exp(-(x - MU)^2/(2*SIGMA^2))
}

# P(90 < X < 110)
a1 <- integrate(f, 90, 110)$value
a1
a2 <- pnorm(110, MU, SIGMA) - pnorm(90, MU, SIGMA)
a2
a1 == a2
# 
sum(dbinom(10:100, 120, 0.03))
pbinom(100, 120, 0.03) - pbinom(9, 120, 0.03)
#
# X ~ N(100, 15), find P(80 < X < 105)
g <- function(x, MU = 100, SIGMA = 15){
  (1/(sqrt(2*pi*SIGMA^2)))*exp(-(x - MU)^2/(2*SIGMA^2))
}
integrate(g, 80, 105)
pnorm(105, 100, 15) - pnorm(80, 100, 15)


