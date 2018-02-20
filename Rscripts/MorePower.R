# 2/20/18
library(PASWR2)
tsum.test(mean.x = 3, s.x = 6, n.x = 36, mu = 4)
# Picture
curve(dt(x, 35, -1), -6, 5, col = "purple", ylab = "")
curve(dt(x, 35), -6, 5, add = TRUE, col = "darkgreen")
abline(v = c(qt(0.05, 35), qt(0.95, 35)), lty = "dashed", col = "red")
abline(h = 0)
# Using ggplot2 now
library(ggplot2)
p <- ggplot(data = data.frame(x = c(-5, 4)), aes(x = x))

dt_fun1 <- function(x){
  y <- dt(x, 35)
  y[x > qt(0.05, 35) & x < qt(0.95, 35)] <- NA
  return(y)
}

dt_fun2 <- function(x){
  y <- dt(x, 35, -1)
  y[x > qt(0.05, 35) & x < qt(0.95, 35)] <- NA
  return(y)
}

p + stat_function(fun = dt_fun1, geom = "area", n = 500, fill = "purple", alpha = 0.5) + 
    stat_function(fun = dt_fun2, geom = "area", n = 500, fill = "blue", alpha = 0.5) + 
    stat_function(fun = dt, args = list(35), n = 500) + 
    stat_function(fun = dt, args = list(35, -1), n = 500) + 
    geom_hline(yintercept = 0) + 
    theme_bw() + 
    labs(x = "", y = "")
######################################################


powerg <- function(n = 36, delta = -1, sd = 6, alpha = 0.10){
  gamma <- (delta)/(sd/sqrt(n))
  dv <- if(gamma < 0){
    ll <- -4 + gamma*1
    ul <- 4
    } else {
    ll <- 4
    ul <- 4 + gamma*1
  }
  p <- ggplot(data = data.frame(x = c(ll, ul)), aes(x = x))
  #
  dt_fun1 <- function(x){
    y <- dt(x, n - 1)
    y[x > qt(alpha/2, n - 1) & x < qt(1 - alpha/2, n - 1)] <- NA
    return(y)
  }
  
  dt_fun2 <- function(x){
    y <- dt(x, n - 1, gamma)
    y[x > qt(alpha/2, n - 1) & x < qt(1 - alpha /2, n - 1)] <- NA
    return(y)
  }
  
  p + stat_function(fun = dt_fun1, geom = "area", n = 500, fill = "purple", alpha = 0.5) + 
      stat_function(fun = dt_fun2, geom = "area", n = 500, fill = "blue", alpha = 0.5) + 
      stat_function(fun = dt, args = list(n - 1), n = 500) + 
      stat_function(fun = dt, args = list(n - 1, gamma), n = 500) + 
      geom_hline(yintercept = 0) + 
      theme_bw() + 
      labs(x = "", y = "")
}

powerg()
#
powerg(n = 64, delta = -1, sd = 3, alpha = 0.10)
powerg(n = 128, delta = -1, sd = 5, alpha = 0.20)
# Note as gamma gets large instability creeps in and a warning in R is issued.
#################################################################################

# Color in Power(mu_1 = 3)...
# Compute Power(mu_1 = 3)
Power3 <- pt(qt(0.05, 35), 35, -1) + pt(qt(0.95, 35), 35, -1, lower.tail = FALSE)
Power3
#
power.t.test(n = 36, delta = -1, sd = 6, type = "one.sample",
             alternative = "two.sided", strict = TRUE, sig.level = 0.10)
#
DELTA <- seq(-8, 8, length = 500)
POWERn9 <- power.t.test(n = 9, delta = DELTA, sd = 6, type = "one.sample",
                        alternative = "two.sided", strict = TRUE, sig.level = 0.10)$power
POWERn36 <- power.t.test(n = 36, delta = DELTA, sd = 6, type = "one.sample",
                        alternative = "two.sided", strict = TRUE, sig.level = 0.10)$power
plot(DELTA, POWERn9, type = "l", col = "blue", ylab = expression(Power(Delta)), xlab = expression(Delta))
lines(DELTA, POWERn36, lty = "dashed", col = "purple")
abline(h = 0.10, lty = "dashed", col = "red")
# Simulation
#
set.seed(14)
sims <- 50000
n <- 36
tstar <- numeric(sims)
for(i in 1:sims){
  rs <- rnorm(n, 3, 6)
  tstar[i] <- (mean(rs) - 4)/(sd(rs)/sqrt(n))
}
SPower <- mean(tstar < qt(0.05, n - 1)) + mean(tstar > qt(0.95, n - 1))
SPower
hist(tstar, breaks = "Scott", col = "red", freq = FALSE)
curve(dt(x, 35, -1), add = TRUE, col = "blue")





