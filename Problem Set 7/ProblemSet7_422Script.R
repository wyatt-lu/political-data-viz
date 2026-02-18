library(ggplot2)
library(colorspace)

colors <- c("#c5b4a3", "#f2d050", "#dd5971", "#59aac7")

blend_colors <- function(indices) {
  
  if (length(indices) == 1) {
    return(colors[indices])
  }
  
  mixed <- hex2RGB(colors[indices[1]])
  
  for (i in 2:length(indices)) {
    next_color <- hex2RGB(colors[indices[i]])
    mixed <- mixcolor(1/i, mixed, next_color)
  }
  
  return(hex(mixed))
}

all_combos = unlist(lapply(1:4, function(x) combn(1:4, x, simplify = FALSE)), recursive = FALSE)
blended_colors = sapply(all_combos, blend_colors)

plot_data <- tibble(
  section = 1:length(blended_colors),
  color = blended_colors
)

ggplot(plot_data) +
  geom_rect(aes(xmin = section - 1, xmax = section,
                ymin = 0, ymax = 1, fill = color)) +
  scale_fill_identity() +
  theme_void()

ggsave("ProblemSet7_422Plot.png", width = 9, height = 4.5)