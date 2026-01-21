library(xml2) # reading the xml files
library(tidyverse)
library(lubridate) # for dates
library(ggthemes)

julia_dat    <- read_xml("JuliaW-export.xml")
lisa_dat     <- read_xml("LisaZ-export.xml")
adela_dat    <- read_xml("AdelaS-export.xml")
hulan_dat    <- read_xml("HulanC-export.xml")
isabella_dat <- read_xml("IsabellaK-export.xml")
wyatt_dat    <- read_xml("WyattL-export.xml") # wyatt got new phone, so no data during quarter 1

julia_records    <- xml_find_all(julia_dat, "//Record[@type='HKQuantityTypeIdentifierStepCount']")
lisa_records     <- xml_find_all(lisa_dat, "//Record[@type='HKQuantityTypeIdentifierStepCount']")
adela_records    <- xml_find_all(adela_dat, "//Record[@type='HKQuantityTypeIdentifierStepCount']")
hulan_records    <- xml_find_all(hulan_dat, "//Record[@type='HKQuantityTypeIdentifierStepCount']")
isabella_records <- xml_find_all(isabella_dat, "//Record[@type='HKQuantityTypeIdentifierStepCount']")
wyatt_records    <- xml_find_all(wyatt_dat, "//Record[@type='HKQuantityTypeIdentifierStepCount']")

julia_df <- tibble(
  date_time = xml_attr(julia_records, "startDate"),
  steps     = as.numeric(xml_attr(julia_records, "value")),
  person    = "Julia",
  device    = xml_attr(julia_records, "sourceName")
)
lisa_df <- tibble(
  date_time = xml_attr(lisa_records, "startDate"),
  steps     = as.numeric(xml_attr(lisa_records, "value")),
  person    = "Lisa",
  device    = xml_attr(lisa_records, "sourceName")
)
adela_df <- tibble(
  date_time = xml_attr(adela_records, "startDate"),
  steps     = as.numeric(xml_attr(adela_records, "value")),
  person    = "Adela",
  device    = xml_attr(adela_records, "sourceName")
)

hulan_df <- tibble(
  date_time = xml_attr(hulan_records, "startDate"),
  steps     = as.numeric(xml_attr(hulan_records, "value")),
  person    = "Hulan",
  device    = xml_attr(hulan_records, "sourceName")
)

isabella_df <- tibble(
  date_time = xml_attr(isabella_records, "startDate"),
  steps     = as.numeric(xml_attr(isabella_records, "value")),
  person    = "Isabella",
  device    = xml_attr(isabella_records, "sourceName")
)

wyatt_df <- tibble(
  date_time = xml_attr(wyatt_records, "startDate"),
  steps     = as.numeric(xml_attr(wyatt_records, "value")),
  person    = "Wyatt",
  device    = xml_attr(wyatt_records, "sourceName")
)

other_info <- tibble(
  person = c("Julia", "Lisa", "Adela", "Hulan", "Isabella", "Wyatt"),
  school = c("McCormick + Comms", "McCormick + ISP", "McCormick + ISP", "Weinberg + ISP", "Medill", "Weinberg"), # School
  dorm   = c("WFC", "McCulloch", "Allison", "Jones", "Hobart", "Hinman")
)

all_friends_data <- bind_rows(julia_df, lisa_df, adela_df, hulan_df, isabella_df, wyatt_df) |> 
  
  left_join(other_info, by = "person") |>
  
  mutate(date = as.Date(ymd_hms(date_time))) |>
  
  filter(
    (person == "Julia" & str_detect(device, "Orange")) | # owns an apple watch, so have to sort data to only be taking from phone (named orange)
      (person != "Julia")) |>
  
  filter(
    date >= "2025-09-09" & date <= "2025-12-12")|>
  
  group_by(date, person, school, dorm)|>
  summarize(total_steps = sum(steps), .groups = "drop")

events <- tibble(
  date = as.Date(c("2025-09-10", "2025-09-16", "2025-11-22", "2025-12-08")),
  event = c("Wildcat Welcome","First Day of Classes", "Thanksgiving Break", "Finals Week")
)

month_boundaries <- as.Date(c("2025-09-01", "2025-10-01", "2025-11-01", "2025-12-01", "2026-01-01"))
month_centers    <- as.Date(c("2025-09-15", "2025-10-16", "2025-11-15", "2025-12-16"))
month_names      <- c("Sep", "Oct", "Nov", "Dec")
labels_df        <- tibble(date = month_centers, label = month_names)

nu_palette <- c(
  "Julia"    = "#bf8faf", 
  "Lisa"     = "#4E2A84", 
  "Adela"    = "#342F2E", 
  "Hulan"    = "#32a852", 
  "Isabella" = "#3279a8"
)

name_labels <- all_friends_data |> 
  select(person, school, dorm) |> 
  distinct() |> 
  mutate(label_text = paste0(person, "\n", school, " • ", dorm))

ggplot(all_friends_data, aes(date, total_steps, color = person)) +
  
  geom_line(linewidth = 0.4, alpha = 0.8) +
  
  geom_vline(data = events, aes(xintercept = date), 
             linetype = "dashed", color = "gray", alpha = 0.6) + 
  
  geom_smooth(se = FALSE, span = 0.3, linewidth = 1.5) +
  
  geom_text(data = events, aes(x = date, label = event, y = Inf), 
            color = "black", angle = 90, hjust = 1.05, 
            size = 3, inherit.aes = FALSE) +
  
  facet_wrap(~person, nrow = 1) +
  
  scale_x_date(
    breaks = month_boundaries,
    labels = NULL,            
    expand = expansion(add = 5)
  ) +
  
  geom_text(data = labels_df, aes(x = date, label = label, y = -Inf), 
            vjust = 2, size = 4, inherit.aes = FALSE, family = "sans") +
  
  geom_text(data = name_labels, 
            aes(x = as.Date("2025-09-12"), y = Inf, label = label_text, color = person),
            fontface = "bold", size = 4.5, 
            hjust = 0, vjust = 15,        
            lineheight = 0.9,              
            family = "sans") +
  
  scale_color_manual(values = nu_palette) +
  coord_cartesian(clip = "off") +
  labs(x = "", y = "Daily Step Count") +
  
  theme_minimal() +
  theme(
    
    strip.text = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    
    plot.margin = margin(t = 10, r = 10, b = 40, l = 10),
    legend.position = "none",
    
    axis.line.x  = element_line(color = "black", linewidth = 0.5), 
    axis.ticks.x = element_line(color = "black", linewidth = 0.5), 
    axis.ticks.length.x = unit(0.2, "cm")
  )

ggsave("fall_quarter_steps.png", width = 16, height = 6)
