library(tidyverse)

dat = read_csv("ProblemSet5_RedesignData.csv")

ggplot(dat, aes(percent, category, color = group)) +
  geom_segment(aes(x = 0, xend = percent, 
                   y = category_number + ifelse(group == "Blacks", -0.1, 0.1),
                   yend = category_number + ifelse(group == "Blacks", -0.1, 0.1), 
                   linewidth = 1.5, alpha = 0.67)) +
  geom_point(aes( y = category_number + ifelse(group == "Blacks", -0.1, 0.1),
                  size = 4)) +
  geom_text(aes(label = paste0(percent, "%"),
                y = category_number + ifelse(group == "Blacks", -0.1, 0.1)),
            hjust = -0.5,
            fontface = "bold") +
  scale_y_continuous(breaks = 1:5,
                     labels = c("Agriculture",
                                "Domenstic Service",
                                "Manufacturing",
                                "Trade & Transportation",
                                "Professions")) +
  coord_cartesian(xlim = c(0, 67)) +
  annotate("text", x = 19, y = 1.26, label = "Whites", color = "#FFAE00", fontface = "bold") + 
  annotate("text", x = 19, y = 2.10, label = "Blacks", color = "#E73E44", fontface = "bold") +
  theme_minimal() + 
  theme (legend.position = "none") +
  labs(x = "Percent",
       y = "Occupation",
       title = "OCCUPATIONS OF BLACKS AND WHITES IN GEORGIA.") +
  scale_color_manual(values = c("Blacks" = "#E73E44", "Whites" = "#FFAE00"))
ggsave("ProblemSet5_RedesignPlot.png", width = 10, height = 6)
