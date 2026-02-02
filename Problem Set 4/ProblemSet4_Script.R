library(tidyverse)
library(janitor)

dat <- read_csv("ProblemSet4_DearData.csv")

clean_dat <- dat |> 
  mutate(
    category = case_when(
      check_reason == "Text"    ~ "Text/Email",            
      check_reason == "Text2"   ~ "Text (with Recipient)", 
      check_reason == "Text3"   ~ "Text (Person in Room)",
      check_reason == "Social"  ~ "Social Media",
      check_reason == "Time"    ~ "Check Time",
      check_reason == "Weather" ~ "Check Weather",
      check_reason == "Call"    ~ "Phone Call",
      check_reason == "Photo"   ~ "Photographing Postcard",
      check_reason == "Charge"  ~ "Charge Phone",
      check_reason == "Other"   ~ "Miscellaneous App",
      check_reason == "Lazy"    ~ "Don't Want to Track",
      check_reason == "Thought" ~ "Thought Phone Rang",
      check_reason == "Turn"    ~ "Turn Phone Over"
    ),
    
    place = as.factor(place_name),
    is_alert = (check_fault == "In"),
    is_purposeful = (check_fault == "Out"),
    with_others_count = as.numeric(check_company) |> replace_na(0)
  )

place_order <- clean_dat |> 
  count(place) |> 
  arrange(desc(n)) |> 
  mutate(rank_id = row_number())

layout <- tibble(
  rank_id = 1:9,
  cx = c(20, 50, 80,   20, 50, 80,   20, 50, 80), 
  cy = c(80, 80, 80,   50, 50, 50,   20, 20, 20)
)

final_layout <- place_order |> 
  left_join(layout, by = "rank_id")

plot_data <- clean_dat |> 
  left_join(final_layout, by = "place") |> 
  group_by(place) |> 
  mutate(
    angle = seq(0, 2*pi, length.out = n())
  ) |> 
  ungroup()

# radii for the elements
r_mainstart <- 4.5
r_mainend   <- 5.5
r_instart   <- 2.5  
r_outend    <- 7.0  
r_with1     <- 8.0
r_with2     <- 8.7
r_with3     <- 9.4

geometry_data <- plot_data |> 
  mutate(
#main
    x_start = cx + r_mainstart * cos(angle),
    y_start = cy + r_mainstart * sin(angle),
    x_end   = cx + r_mainend * cos(angle),
    y_end   = cy + r_mainend * sin(angle),
# in 
    x_in    = cx + r_instart * cos(angle),
    y_in    = cy + r_instart * sin(angle),
# out
    x_out   = cx + r_outend * cos(angle),
    y_out   = cy + r_outend * sin(angle),
# with others
    x_with1 = cx + r_with1 * cos(angle),
    y_with1 = cy + r_with1 * sin(angle),
    x_with2 = cx + r_with2 * cos(angle),
    y_with2 = cy + r_with2 * sin(angle),
    x_with3 = cx + r_with3 * cos(angle),
    y_with3 = cy + r_with3 * sin(angle)
  )

colors <- c(
  "Text/Email"             = "#f4772e",
  "Social Media"           = "#ae8c63",
  "Miscellaneous App"      = "#979188",
  "Check Time"             = "#6b8ecf",
  "Check Weather"          = "#38695c",
  "Phone Call"             = "#008a9b",
  "Text (Person in Room)"  = "#00866f",
  "Charge Phone"           = "#ff002f",
  "Text (with Recipient)"  = "#fcab9f",
  "Photographing Postcard" = "#d8a895"
)

caption = "From Left to Right: Walking, Working, Waiting, Bathroom, Couch, Bed, Home, Cafe, Transit."
caption2 = "Inside ticks: Due to Notification. Outside ticks: On Purpose. Number of dots: Company."

ggplot(geometry_data) +
  # main things
  geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end, color = category),
               linewidth = 0.8, lineend = "round") +
  # in
  geom_segment(data = filter(geometry_data, is_alert),
               aes(x = x_start, y = y_start, xend = x_in, yend = y_in, color = category),
               linewidth = 0.8, lineend = "round") +
  # out 
  geom_segment(data = filter(geometry_data, is_purposeful),
               aes(x = x_end, y = y_end, xend = x_out, yend = y_out, color = category),
               linewidth = 0.8, lineend="round") +
  # dots
  geom_point(data = filter(geometry_data, with_others_count >= 1),
             aes(x = x_with1, y = y_with1),
             size = 0.1, color = "black") +
  # ring2 of dots
  geom_point(data = filter(geometry_data, with_others_count >= 2),
             aes(x = x_with2, y = y_with2),
             size = 0.1, color = "black") +
  # ring3 of dots
  geom_point(data = filter(geometry_data, with_others_count >= 3),
             aes(x = x_with3, y = y_with3),
             size = 0.1, color = "black") +
  
  annotate("text", x = 50, y = 3, label = caption, size = 3) +
  annotate("text", x = 50, y = 0, label = caption2, size = 3) +
 
  coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
  scale_color_manual(values = colors,
                     breaks = c(
                       "Text/Email", 
                       "Text (with Recipient)", 
                       "Text (Person in Room)",
                       "Social Media", 
                       "Phone Call", 
                       "Check Time", 
                       "Check Weather", 
                       "Photographing Postcard", 
                       "Charge Phone", 
                       "Miscellaneous App"
                     )) +
  theme_void() +

  theme(
    legend.title = element_blank(),
    plot.margin = margin(20, 20),
    plot.background = element_rect(
      fill = "white",
    )
  )

ggsave("ProblemSet4_DearDataPlot.png", width = 8, height = 6)

