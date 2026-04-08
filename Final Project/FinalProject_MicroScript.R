library(tidyverse)
library(ggthemes)
library(ggbeeswarm)

dat <- read.csv("pairsfs2017.csv")

dat <- dat |>
  mutate(judge_status = ifelse(judge_nation == "RUS", "Home Judge", "Panel Judge"))

dat$element_name <- factor(dat$element_name, 
                                  levels = rev(unique(dat$element_name)))

ggplot(dat, aes(y = element_name)) +
  geom_segment(aes(x = min, xend = max, y = element_name, yend = element_name), 
               color = "#E0E0E0", linewidth = 2) +
  geom_point(aes(x = base_value), shape = 124, size = 6, color = "black") +
  geom_point(aes(x = min), shape = 124, size = 4, color = "#eb4034") +
  geom_point(aes(x = max), shape = 124, size = 4, color = "#49ad5f") +
  
  geom_point(aes(x = awarded, color = judge_status, size = judge_status), 
             shape = 19, alpha = 0.8,
             position = position_beeswarm(orientation = "y", cex = 2)) +
  
  scale_color_manual(values = c("Home Judge" = "#FFD700",
                                "Panel Judge" = "black")) +
  scale_size_manual(values = c("Home Judge" = 6,
                               "Panel Judge" = 3)) +
  
  scale_x_continuous(breaks = 0:12) +


  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#F0F0F0"),
    axis.title.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = 10),
    axis.title = element_text(face = "bold", size = 12),
    legend.position = "none",
  ) +
  
  labs(x = "Awarded Score") +
  
  annotate("text", x = 6.70, y = 2.50, label = "Home Judge",
           color = "gold", size = 3, fontface = "bold") +
  annotate("text", x = 6.00, y = 3.50, label = "Base Value",
           color = "black", size = 3, fontface = "bold") +
  annotate("text", x = 3.90, y = 2.50, label = "Min Points",
           color = "#eb4034", size = 3, fontface = "bold") +
  annotate("text", x = 8.10, y = 2.50, label = "Max Points",
           color = "#49ad5f", size = 3, fontface = "bold") +
  
  annotate("label", x = 12, y = 1, 
           label = "Home Judge TES: 77.10\nRest of Panel Avg TES: 69.30",
           hjust = 1, vjust = 0, size = 2.5, fontface = "bold",
           fill = "white", color = "black", label.size = 0.3)

ggsave("FinalProject_Micro.png", height = 6, width = 15)

