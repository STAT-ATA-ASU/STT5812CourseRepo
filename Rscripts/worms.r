worms <- c(1,  2,  2, 10,  7, 16, 10, 10,  7, 17)
worms
mean(worms[1:5])
mean(worms[6:10])
#

index <- sample(10, 5, replace = FALSE)
mean(worms[index]) - mean(worms[-index])




sims <- 10000
meand <- numeric(sims)
for(i in 1:sims){
  index <- sample(10, 5, replace = FALSE)
  meand[i] <- mean(worms[index]) - mean(worms[-index])
}
hist(meand)
pvalue <- (sum(meand >= 7.6) + 1)/(sims + 1)
pvalue