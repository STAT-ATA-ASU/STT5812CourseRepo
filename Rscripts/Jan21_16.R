library(PASWR2)
X <- COWS$butterfat[COWS$breed =="Ayrshire"]
Y <- COWS$butterfat[COWS$breed =="Canadian"]
t.test(X, Y)
FAT <- c(X, Y)
R <- 10^4 - 1
TS <- numeric(R)
for(i in 1:R){
  index <- sample(20, 20, replace = FALSE)
  TS[i] <- mean(FAT[index]) - mean(FAT[-index])
}
pvalue <- ((sum(TS >= abs(t.test(X, Y)$stat)))*2 + 1) / (R + 1)
pvalue
# Consider using dplyr
library(dplyr)
DF <- COWS %>% 
  filter(breed == "Ayrshire" | breed == "Canadian")
DF
X <- DF$butterfat[DF$breed == "Ayrshire"]
Y <- DF$butterfat[DF$breed == "Canadian"]
t.test(X, Y)
FAT <- c(X, Y)
R <- 10^4 - 1
TS <- numeric(R)
for(i in 1:R){
  index <- sample(20, 20, replace = FALSE)
  TS[i] <- mean(FAT[index]) - mean(FAT[-index])
}
pvalue <- ((sum(TS >= abs(t.test(X, Y)$stat)))*2 + 1) / (R + 1)
pvalue
# 
t.test(butterfat ~ breed, data = DF)
PTS <- numeric(R)
for(i in 1:R){
  PTS[i] <- t.test(butterfat ~ sample(breed), data = DF)$stat
}
pvalue <- (sum(PTS >= abs(t.test(butterfat ~ breed, data = DF)$stat))*2 + 1) / (R + 1)
pvalue