library(tidyverse)
library(ggraph)
library(tidygraph)

groups <- read_csv("data.csv")

strong_alliances <- groups |>
  filter(home_judge == FALSE) |>
  group_by(judge_nation, skater_nation) |>
  summarize(
    average_bias = mean(judge_score-official_score),
    comparisons = n()
  ) |>
  filter(comparisons >= 5) |>
  filter(average_bias > 1)

data <- as_tbl_graph(strong_alliances, directed=TRUE) |>
  activate(nodes) |>
  mutate(
    outd = centrality_degree(mode = "out"),
    ind = centrality_degree(mode = "in"),
    role = case_when(outd > 0 & ind > 0 ~ "Both",
                     outd > 0 ~ "Judge Only",
                     TRUE ~ "Skater Only")
  )

set.seed(4821)
ggraph(data, layout = "kk") + 
  geom_edge_link(aes(edge_width = average_bias, edge_alpha = average_bias), 
                 arrow = arrow(length = unit(3, 'mm')), 
                 end_cap = circle(6, 'mm'),
                 color = "gray") +
  geom_node_point(aes(color = role), size = 9) +
  geom_node_text(aes(label = name), color = "white", fontface = "bold", size = 3) +
  scale_edge_width(range = c(0.8, 3)) +
  scale_edge_alpha(range = c(0.4, 1)) +
  scale_color_manual(values = c(
    "Judge Only" = "#f5c982",
    "Skater Only" = "#99c2b3",
    "Both"= "#c2b299"
  )) +
  theme_void() +
  theme(legend.position = "none",
        legend.title = element_blank())

ggsave("FinalProject_Macro.png", width = 10, height = 8, bg = "white")