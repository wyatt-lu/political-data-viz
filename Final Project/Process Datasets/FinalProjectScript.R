library(tidyverse)

protocols <- read_csv("output_data.csv")

dat <- protocols |>
  mutate(home_judge = judge_nation == skater_nation) |>
  filter(segment %in% c("SD", "RD"), competition_year == "2017") |>
  mutate(skater_name = fct_reorder(skater_name, skater_place))

ggplot(dat, aes(x = skater_name, y = combined_segment_score)) +
  geom_tufteboxplot() +
  geom_point(data = dat |> distinct(skater_name, official_segment_score_result), 
             aes(y = official_segment_score_result),
             color = "darkorange", size = 2) +
  geom_point(data = dat |> filter(home_judge == TRUE), 
             aes(y = combined_segment_score),
             color = "forestgreen", size = 2) +
  
  geom_text(data = dat,
            aes(x = skater_name, y = 108, label = skater_nation),
            size = 4) +
  
  theme_tufte() +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))

RD_plot <- protocols |> 
  filter(segment %in% c("SD", "RD")) |> 
  ggplot(aes(x = skater_place, y = combined_segment_score, color = home_judge)) +
  geom_point() +
  facet_wrap(~competition_year)

ggsave("RD_plot.png", RD_plot, height = 10, width = 20)

FD_plot <- protocols |> filter(segment == "FD") |>
  ggplot(aes(x = skater_place, y = combined_segment_score, color = home_judge)) +
  geom_point() +
  facet_wrap(~competition_year)

ggsave("FD_plot.png", FD_plot, height = 10, width = 20)

LSP_plot <- protocols |> filter(discipline == "ladies", segment == "SP") |>
  ggplot(aes(x = skater_place, y = combined_segment_score, color = home_judge)) +
  geom_point() +
  facet_wrap(~competition_year)

ggsave("LSP_plot.png", LSP_plot, height = 10, width = 20)

LFS_plot <- protocols |> filter(discipline == "ladies", segment == "FS") |>
  ggplot(aes(x = skater_place, y = combined_segment_score, color = home_judge)) +
  geom_point() +
  facet_wrap(~competition_year)

ggsave("LFS_plot.png", LFS_plot, height = 10, width = 20)

MSP_plot <- protocols |> filter(discipline == "mens", segment == "SP") |>
  ggplot(aes(x = skater_place, y = combined_segment_score, color = home_judge)) +
  geom_point() +
  facet_wrap(~competition_year)

ggsave("MSP_plot.png", MSP_plot, height = 10, width = 20)

MFS_plot <- protocols |> filter(discipline == "mens", segment == "FS") |>
  ggplot(aes(x = skater_place, y = combined_segment_score, color = home_judge)) +
  geom_point() +
  facet_wrap(~competition_year)

ggsave("MFS_plot.png", MFS_plot, height = 10, width = 20)

PSP_plot <- protocols |> filter(discipline == "pairs", segment == "SP") |>
  ggplot(aes(x = skater_place, y = combined_segment_score, color = home_judge)) +
  geom_point() +
  facet_wrap(~competition_year)

ggsave("MFS_plot.png", MFS_plot, height = 10, width = 20)

PFS_plot <- protocols |> filter(discipline == "pairs", segment == "FS") |>
  ggplot(aes(x = skater_place, y = combined_segment_score, color = home_judge)) +
  geom_point() +
  facet_wrap(~competition_year)

ggsave("MFS_plot.png", MFS_plot, height = 10, width = 20)




