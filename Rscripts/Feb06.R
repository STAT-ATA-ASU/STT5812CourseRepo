# R script

names <- c("Coleman", "Stephen", "Jen", "Ashley", "Hunter", "Sam")
names
str(names)
sex <- c("Male", "Male", "Female", "Female", "Male", "Male")
height <- c(73, NA, 64, 64, 43, 6800)
age <- c(51, 25, 48, 20, NA, NA)
DF <- data.frame(names = names, sex = sex, height = height, age = age)
DF
str(DF)
rm(names, sex, height, age)
library(dplyr)
DF$height
with(DF, height)
with(DF, mean(height))
mean(DF$height)
mean(DF$height, na.rm = TRUE)
DF
na.omit(DF)
mean(DF$height, na.rm = TRUE)
mean(DF$age, na.rm = TRUE)
length(DF$age)
sum(is.na(DF$age))
sum(!is.na(DF$age))
# dplyr verbs: summarize, filter, mutate, arrange, select
DF %>%
  group_by(sex) %>%
  summarize(HeightAvg = mean(height, na.rm = TRUE), 
            AgeAvg = mean(age, na.rm = TRUE))
