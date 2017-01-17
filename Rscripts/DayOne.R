#  Comment
ws <- c(67, 64, 66, 67, 68, 61, 64, 65, 69, 71)
hi <- c(70, 69, 72, 70, 70, 62, 68, 65, 66, 77)
name <- c("Dustin", "Cescily", "Averia", "John", "Carter", "Jaehee",
          "Paige", "Sierra", "Austin", "Lee")
sex <- c("M", "F", "F", "M", "M", "F", "F", "F", "M", "M")
# Creating Data Frame

DF <- data.frame(Name = name, WingSpan = ws, 
                 HeightIn = hi, Sex = sex)
DF
#
library(ggplot2)
ggplot(data = DF, aes(x = HeightIn, y = WingSpan, color = Sex)) +
  geom_point()
#
library(dplyr)
Res <- DF %>%
  group_by(Sex) %>%
  summarize(MH = mean(HeightIn), SH = sd(HeightIn))
Res


