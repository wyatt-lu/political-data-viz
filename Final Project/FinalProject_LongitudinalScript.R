library(tidyverse)
library(ggthemes)

dat <- read_csv("data.csv")

home_bias <- dat |>
  filter(home_judge == TRUE, event_year >= 2019) |>
  mutate(bias = judge_score - official_score)

ggplot(home_bias, aes(x = event_year, y = bias)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_jitter(width = 0.15, height = 0, alpha = 0.4, size = 1.5, color = "#76947d") +
  geom_smooth(method = "loess", se = TRUE, color = "#E67E22", fill = "#E67E22", alpha = 0.15) +
  scale_x_continuous(breaks = 2019:2025) +
  labs(x = "Season", y = "Home Judge Bias (Judge Score − Official Score)") +
  theme_minimal()

ggsave("FinalProject_AcrossSeasons.png", height = 6, width = 6, bg = "white")

panel_avg <- dat |>
  filter(home_judge == FALSE, event_year >= 2019) |>
  group_by(skater_name, event_year, segment_discipline, segment_type) |>
  summarise(panel_avg = mean(judge_score, na.rm = TRUE), .groups = "drop")

home_bias_clean <- dat |>
  filter(home_judge == TRUE, event_year >= 2019) |>
  select(skater_name, event_year, segment_discipline, segment_type,
         home_score = judge_score, skater_nation) |>
  left_join(panel_avg, by = c("skater_name", "event_year", "segment_discipline", "segment_type")) |>
  mutate(bias = home_score - panel_avg)

ggplot(home_bias_clean, aes(x = event_year, y = bias)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_jitter(width = 0.15, height = 0, alpha = 0.4, size = 1.5, color = "#76947d") +
  geom_smooth(method = "loess", se = TRUE, color = "#E67E22", fill = "#E67E22", alpha = 0.15) +
  scale_x_continuous(breaks = 2019:2025) +
  labs(x = "Season", y = "Home Judge Bias (Home Score − Rest of Panel Avg)") +
  facet_wrap(~ segment_discipline) +
  theme_minimal()

ggsave("FinalProject_AcrossDisciplines.png", height = 6, width = 7, bg = "white")