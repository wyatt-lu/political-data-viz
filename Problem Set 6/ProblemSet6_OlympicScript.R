library(tidyverse)
library(ggthemes)

dat <- read_csv("ProblemSet6_OlympicData.csv")

dat <- dat |> 
  mutate(
    label = paste0(place, ". ", name, " n=9"),
    label = fct_reorder(label,place)
  )

ggplot(dat, aes(x = label, y = judge_s_score)) +
  stat_boxplot(geom = "errorbar", width = 0.4) +
  geom_boxplot(width = 0.7,
               median.linewidth = 0) +
  geom_crossbar(data = dat |> distinct(label, final_s_score), 
                aes(y = final_s_score, ymin = final_s_score, ymax = final_s_score),
                color = "darkorange", width = 0.7, linewidth = 0.2) +
  geom_text(data = dat |> distinct(label, final_s_score),
            aes(y = final_s_score, label = sprintf("%.2f", final_s_score)),
            vjust = -0.4) +
  geom_crossbar(data = dat |> filter(home_country_judge == "Yes"), 
                aes(y = judge_s_score, ymin = judge_s_score, ymax = judge_s_score),
                color = "forestgreen", width = 0.7, linetype = "dotted", linewidth = 0.2) +
  theme_minimal() +
  scale_y_continuous(breaks = c(110, 115, 120, 125, 130, 135)) +
  labs(x = NULL, y = NULL,
       title = "2026 Olympic Winter Games Senior Free Dance Individual Judge TSS Tally Spreads") +
  theme(axis.text.x = element_text(angle = -90, hjust = 0.5),
        plot.title = element_text(face = "bold")) +
  
  geom_text(data = dat,
            aes(x = label, y = 108, label = country),
            size = 4) +
  annotate("text", x = 3.6, y = 135, hjust = 0,
           label = "Orange Lines + Labels: Awarded (not median) TSS\n
                    Green Dotted Lines: TSS from Home Country Judge\n
                    Box: Middle 50% of Individual Judge TSS\n
                    Whiskers: Non-\"Outlier\" High & Low Individual Judge TSS\n
                    Dots: \"Outlier\" Individual Judge TSS (<Q1-1.5IQR or >Q3+1.5IQR)")

ggsave("ProblemSet6_Olympic1Ratio.png", width = 8, height = 11)

# high data-ink ratio:

dat <- dat |> 
  mutate(
    label = paste0(place, ". ", name),
    label = fct_reorder(label, place)
  )

ggplot(dat, aes(x = label, y = judge_s_score)) +
  geom_tufteboxplot() +
  geom_point(data = dat |> distinct(label, final_s_score), 
             aes(y = final_s_score),
             color = "darkorange", size = 2) +
  geom_point(data = dat |> filter(home_country_judge == "Yes"), 
             aes(y = judge_s_score),
             color = "forestgreen", size = 2) +
  
  geom_text(data = dat,
            aes(x = label, y = 108, label = country),
            size = 4) +
  
  annotate("text", x = 3, y = 135.15, hjust = -0.1,
           size = 3, color = "forestgreen",
           label = "TSS from Home Country Judge") +
  annotate("text", x = 3.15, y = 132, hjust = -0.1,
           size = 2.5, color = "black",
           label = "Median TSS") +
  annotate("text", x = 2.9, y = 131.5, hjust = -0.1,
           size = 3, color = "darkorange",
           label = "Final Awarded TSS (average after high/low dropped)") +

  theme_tufte() +
  labs(x = NULL, y = "TSS Score",
       title = "2026 Winter Olympic Free Dance Judge Total Segment Score Spreads") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))

ggsave("ProblemSet6_Olympic2Ratio.png", width = 10, height = 7)

# low data-ink ratio:

ggplot(dat, aes(x = label, y = judge_s_score)) +
  geom_boxplot(width = 0.7,
               linewidth = 1.5,
               aes(fill = name)) +
  geom_crossbar(data = dat |> distinct(label, final_s_score), 
                aes(y = final_s_score, ymin = final_s_score, ymax = final_s_score),
                color = "darkorange", width = 0.6, linewidth = 1) +
  geom_text(data = dat |> distinct(label, final_s_score),
            aes(y = final_s_score, label = sprintf("%.2f", final_s_score)),
            vjust = -0.4, fontface = "bold", color = "white")+
  geom_crossbar(data = dat |> filter(home_country_judge == "Yes"), 
                aes(y = judge_s_score, ymin = judge_s_score, ymax = judge_s_score),
                color = "forestgreen", width = 0.6, linetype = "dotted", linewidth = 1) +
  theme_dark() +
  scale_y_continuous(breaks = seq(110, 140)) +
  labs(x = "Dance Team", y = "Total Free Dance Segment Score",
       fill = "Free Dance Competitors",
       title = "2026 Olympic Winter Games Senior Free Dance Individual Judge TSS Tally Spreads") +
  theme(axis.text.x = element_text(angle = -90, hjust = 0.5),
        plot.title = element_text(face = "bold")) +
  
  geom_text(data = dat,
            aes(x = label, y = 108, label = country),
            size = 5, fontface = "bold") +
  annotate("text", x = 7, y = 135, size = 3, fontface = "bold", color = "white",
           label = "Orange Lines + Labels: Awarded (not median) TSS\n
                    Green Dotted Lines: TSS from Home Country Judge\n
                    Box: Middle 50% of Individual Judge TSS\n
                    Whiskers: Non-\"Outlier\" High & Low Individual Judge TSS\n
                    Dots: \"Outlier\" Individual Judge TSS (<Q1-1.5IQR or >Q3+1.5IQR)")

ggsave("ProblemSet6_Olympic0.5Ratio.png", width = 8, height = 11)
