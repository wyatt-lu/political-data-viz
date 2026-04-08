library(tidyverse)
library(ggthemes)

dat <- read_csv("data.csv") |>
  mutate(label = paste0(skater_place, ". ", skater_name),
         label = fct_reorder(label, skater_place),
         judge_status = ifelse(home_judge == TRUE, "Home Judge", "Panel Judge"))

scores <- dat |> 
  filter(segment_discipline == "Womens", segment_type == "FS", event_year == 2025) |>
  arrange(judge_status == "Home Judge") 

ggplot(scores, aes(x = label)) +
  
  geom_jitter(data = scores |> filter(judge_status == "Panel Judge"),
              aes(y = judge_score, color = judge_status, size = judge_status),
              width = 0.15, height = 0, alpha = 0.8) +
  
  geom_point(data = scores |> filter(judge_status == "Home Judge"),
             aes(y = judge_score, color = judge_status, size = judge_status),
             alpha = 0.8) +
  geom_point(data = scores |> distinct(label, official_score), 
             aes(y = official_score, fill = "Official Score"),
             shape = 23, size = 3.5, color = "black", stroke = 0.5) +
  geom_text(data = scores |> distinct(label, skater_nation),
            aes(y = min(scores$judge_score, na.rm = TRUE) - 5, label = skater_nation),
            size = 3.5, color = "black", fontface = "bold") +
  
  scale_color_manual(values = c("Home Judge" = "#76947d", "Panel Judge" = "gray")) +
  scale_size_manual(values = c("Home Judge" = 4, "Panel Judge" = 2)) +
  scale_fill_manual(values = c("Official Score" = "#e67e22")) +
  
  theme_tufte(base_family = "sans") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(), 
    axis.ticks.x = element_blank(),
    axis.title = element_text(face = "bold", size = 12),
    legend.position = "none",
  ) +
  labs(y = "Total Segment Score")
  

ggsave("FinalProject_w2025.png", height = 8, width = 14, bg = "white")

