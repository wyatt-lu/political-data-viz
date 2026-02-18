library(tidyverse)

dat <- read_csv("ProblemSet7_StainData.csv")

for (i in 1:((nrow(dat)-1))) {
  for (j in (i+1):(nrow(dat))) {
    connections <- data.frame(
      rbind(connections, data.frame(
      x = dat$x[i],
      y = dat$y[i],
      xend = dat$x[j],
      yend = dat$y[j]
    )))
  }
}

ggplot(dat, aes(x, y)) +
  geom_point(size = 5, color = "black") +
  geom_segment(data = connections, aes(x, y, xend = xend, yend = yend),
               alpha = 0.3 , color = "black") +
  theme_void()

ggsave("ProblemSet7_StainPlot.png", width = 21, height = 14)
  