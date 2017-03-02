library(PASWR2)
library(broom)
library(dplyr)


ggplot(data = GRADES, aes(x = sat, y = gpa)) + 
  geom_point(color = "lightblue") + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE)

GRADES %>%
  filter(sat >= 1000, sat <= 1200) %>%
  ggplot(aes(x = sat, y = gpa)) +
  geom_point(color = "lightblue") + 
  geom_smooth(method = "lm") + 
  theme_bw()

NDF <- GRADES %>%
  filter(sat >= 1000, sat <= 1200)
mod <-  lm(gpa ~ sat, data = NDF)
mod

confint(mod)

predict(mod, newdata = data.frame(sat = 1100), interval = "conf")
