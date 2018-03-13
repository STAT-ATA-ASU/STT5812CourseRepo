# March 13, 2018
# Alan T. Arnholt

library(PASWR2)
library(tidyverse)
p <- ggplot(data = GRADES, aes(x = sat, y = gpa))
p + geom_point() + geom_smooth(method = "lm") + theme_bw()

#
mod <- lm(gpa ~ sat, data = GRADES)
X <- model.matrix(mod)
Y <- GRADES$gpa

betahat <- solve(t(X)%*%X)%*%t(X)%*%Y
betahat
#
coef(mod)
# betahat ~ N(beta, Var(X'X)^-1 = Var(betahat))
# an estimate of Var(betahat) = MSE(X'X)^-1; MSE = SSE/(n-p) = sum(e_i^2)/(n-p)
anova(mod)
SSE <- anova(mod)[2, 2]
SSE
MSE <- SSE/(200 - 2)
MSE 
# or
MSE <- anova(mod)[2, 3]
MSE
XTXI <- solve(t(X)%*%X)
XTXI
vce <- MSE*XTXI
vce

# Preferred way to compute (X'X)^-1 = XTXI is summary(lm.object)$cov.unscaled

XTXI <- summary(mod)$cov.unscaled
XTXI
# Just checking
solve(t(X)%*%X)
#
pvce <- MSE * XTXI
pvce
# The variance covariance matrix can be computed directly with vcov
vcov(mod)
