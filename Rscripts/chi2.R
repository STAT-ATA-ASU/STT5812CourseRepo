sims <- 10^4
n <- 10
sigma2 <- 10
mu <- 100
chi2 <- numeric(sims)
for(i in 1:sims){
  stuff <- rnorm(n, mu, sigma2)
  chi2[i] <- (n - 1)*var(stuff)/sigma2
}
hist(chi2, breaks = "scott")
