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
# 
sb0 <- vcov(mod)[1, 1]^.5
sb0
sb1 <- vcov(mod)[2, 2]^.5
sb1
# Recall betahat
b0 <- betahat[1, 1]
b0
b1 <- betahat[2, 1]
b1
#
# CI
b1 + c(-1, 1)*qt(.975, 198)*sb1
# or using confint
confint(mod, level = 0.95)
# Test H_0; \beta_1 = 0
#
tobs <- b1/sb1
tobs
#######################
HSWRESTLER
ggplot(data = HSWRESTLER, aes(x = tanfat, y = skfat)) + 
  geom_point() + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) + 
  geom_smooth(se = FALSE)
###
mod_fat <- lm(skfat ~ tanfat, data = HSWRESTLER)
X <- model.matrix(mod_fat)
head(X)
XTX <- t(X)%*%X
XTX
XTXI <- solve(XTX)
XTXI
# Question is XTXI*XTX? I_{2*2}
I2_2 <- XTXI %*% XTX
I2_2
#####
mod_3 <- lm(hwfat ~ abs + triceps,data = HSWRESTLER)
X <- model.matrix(mod_3)
head(X)
XTX <- t(X)%*%X
XTXI <- solve(XTX)
XTXI
# preferred way
summary(mod_3)$cov.unscaled
#
betahat <- XTXI%*%t(X)%*%HSWRESTLER$hwfat
betahat
summary(mod_3)
vcov(mod_3)
#
anova(mod_3)
sum(anova(mod_3)[1:2, 2])
J <- matrix(1, nrow = 78, ncol = 78)
SSR <- t(betahat)%*%t(X)%*%HSWRESTLER$hwfat - 1/78*t(HSWRESTLER$hwfat)%*%J%*%HSWRESTLER$hwfat
SSR





nm <- matrix(c(1, 2, 2, 1), nrow = 2)
nm
eigen(nm)
