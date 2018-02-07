library(MASS)
library(tidyverse)
ggplot(data = quine, aes(x = Days)) + 
  geom_density(fill = "purple", alpha = 0.3) + 
  facet_grid(Sex ~ Age) + 
  theme_bw()
ANS <- quine %>% 
  group_by(Sex, Age) %>% 
  summarize(median(Days), IQR(Days))
ANS
