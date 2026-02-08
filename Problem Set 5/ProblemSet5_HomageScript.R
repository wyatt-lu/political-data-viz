library(tidyverse)

dat <- read_csv("ProblemSet5_HomageData.csv")

colors <- c(
  "Quad Jump"   = "#E73E44",
  "3A"          = "#7A81D3",
  "Triple Jump" = "#D4B8A2",
  "Double Jump" = "#FFAE00",
  "Single Jump" = "#E8DCC5",
  "Sp4"         = "#2D9C5A",
  "StSq4"       = "#673E2B",
  "1"           = "NA",
  "2"           = "NA"
)


dat_clean <- dat |>
  group_by(discipline) |>
  mutate(percent = value/sum(value)*40) |>
  ungroup()

gaps <- tibble(
  discipline = c("Gap", "Gap"),
  category = c("1", "2"),
  value = c(0,0),
  percent = c(10, 10)
)

dat_final <- bind_rows(
  filter(dat_clean, discipline == "Mens"),
  slice(gaps, 1),
  filter(dat_clean, discipline == "Womens"),
  slice(gaps, 2)
) |> mutate(
  plotting_order = row_number(),
)

legend_data <- tibble(
  label = c("Quad Jump", "Triple Axel", "Triple Jump", 
            "Double Jump", "Single Jump", "Level 4 Spin", "Level 4 Step"),
  color = c("#E73E44", "#7A81D3", "#D4B8A2", "#FFAE00", "#E8DCC5", "#2D9C5A", "#673E2B"),

  x_point  = c(1.22, 1.208, 1.22, 1.392, 1.38, 1.38, 1.392),
  y_point  = c(8.2, 5.2, 2.2, 58, 56, 54, 52),
  
  x_text = c(1.27, 1.269, 1.27, 1.35, 1.335, 1.335, 1.35),
  y_text = c(8.2, 5.2, 2.2, 58, 56, 54, 52) 
)

ggplot(dat_final, aes(x = "", y = percent, fill = category, group = plotting_order)) +
  geom_bar(stat = "identity", aes(color = ifelse(discipline == "Gap", NA, "#404142"))) +
  coord_polar("y", start = 1.255) +
  
  scale_fill_manual(values = colors) +
  scale_color_identity() +
  
  theme_void() + 
  theme(
    legend.position = "none",
    plot.title = element_text(
      hjust = 0.5, 
      face = "bold",
      size = 14)) +
  labs(title = "ELEMENTS OF MEN AND WOMEN AT 2025 WORLD FIGURE SKATING CHAMPIONSHIPS.") +
  
  geom_point(data = legend_data, 
               aes(x = x_point, y = y_point, color = color), 
               size = 5, inherit.aes = FALSE) +
  geom_text(data = legend_data, 
            aes(x = x_text, y = y_text, label = label), 
            hjust = 0, size = 3, fontface = "bold", inherit.aes = FALSE) +
  
  annotate("text", x = 1.5, y = 29.9, label = "WOMENS", 
           size = 5, fontface = "bold") +
  annotate("text", x = 1.5, y = 80, label = "MENS", 
           size = 5, fontface = "bold")

ggsave("ProblemSet5_HomagePlot.png", width = 10, height = 10)