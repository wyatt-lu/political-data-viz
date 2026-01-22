library(tidyverse)

dat <- read_csv("fuel_economy_standards.csv")

grouped_data <- dat |>
  mutate(group = ifelse(year <= 2000, "A", "B"))

ggplot(grouped_data, aes(year, fuel_economy_standard)) +
  geom_line() +
  geom_point() +
  
  scale_y_continuous(limits = c(0,60), breaks = seq(0, 60, 10)) +
  
  scale_x_continuous(
    breaks = seq(1978,2025, 1),
    labels = function(x) paste0("'", substr(x, 3, 4)),
    expand = expansion(add = 0.5)
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_blank()
  ) +
  facet_wrap(~group, ncol = 1, scales = "free_x") +
  labs(
    x = "Year",
    y = "Miles Per Gallon"
  )

ggsave("ProblemSet3_DrawingPlot.png", width = 5)
