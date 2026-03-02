library(tidyverse)
library(sf)

kingcounty <- st_read("2024_Voting_Districts_of_King_County___votdst_area_2024/2024_Voting_Districts_of_King_County___votdst_area_2024.shp")
election <- read_csv("final-results-report.csv")

prop1 <- election |>
  filter(
    Race == "King County Proposition No. 1",
    CounterType %in% c("Approved", "Rejected", "Registered Voters")
  ) |>
  select(Precinct, CounterType, SumOfCount) |>
  pivot_wider(names_from = CounterType, values_from = SumOfCount) |>
  mutate(approved_share = Approved/(Approved + Rejected)) |>
  mutate(voter_share = (Approved + Rejected) / `Registered Voters`)

joined <- left_join(kingcounty, prop1, by = c("NAME" = "Precinct"))

prop1kingcountyplot <- ggplot(joined) +
  geom_sf(aes(fill = approved_share), color = NA) +
  scale_fill_gradient2(
    low = "#d7191c", mid = "white", high = "#2c7bb6",
    midpoint = 0.5,
    na.value = "gray"
  ) +
  theme_void() + theme(legend.position = "none")

prop1kingcountyturnoutplot <- ggplot(joined) +
  geom_sf(aes(fill = voter_share), color = NA) + 
  scale_fill_gradient(
    low = "white", high = "darkblue",
    na.value = "gray"
  ) +
  theme_void() +
  theme(legend.position = "none")

seattle <- joined |> 
  filter(str_starts(NAME, "SEA"), !NAME %in% c("SEALTH", "SEAN"))

prop1seattleplot <- ggplot(seattle) +
  geom_sf(aes(fill = approved_share), color = NA) +
  scale_fill_gradient2(
    low = "#d7191c", mid = "white", high = "#2c7bb6",
    midpoint = 0.5,
    na.value = "gray"
  ) +
  theme_void() +
  theme(legend.position = "none")

prop1seattleturnoutplot <- ggplot(seattle) +
  geom_sf(aes(fill = voter_share), color = NA) + 
  scale_fill_gradient(
    low = "white", high = "darkblue",
    na.value = "gray"
  ) +
  theme_void() + theme(legend.position = "none")

nov2024 <- read_csv("general-election-nov2024-final-results-report.csv")

cd7 <- nov2024 |>
  filter(
    Race == "Congressional District No. 7 United States Representative",
    CounterType %in% c("Pramila Jayapal", "Dan Alexander")
  ) |>
  select(Precinct, CounterType, SumOfCount) |>
  pivot_wider(names_from = CounterType, values_from = SumOfCount) |>
  rename(Jayapal = `Pramila Jayapal`, Alexander = `Dan Alexander`) |>
  mutate(jayapal_share = Jayapal / (Jayapal + Alexander))

seattle_cd7 <- left_join(seattle |> select(NAME, geometry), cd7, by = c("NAME" = "Precinct")) |>
  filter(!is.na(Jayapal), !is.na(Alexander))

cd7seattleplot <- ggplot(seattle_cd7) +
  geom_sf(aes(fill = jayapal_share), color = NA) +
  scale_fill_gradient(
    low = "white", high = "#2c7bb6",
    limits = c(0.5, 1),
    labels = scales::percent
  ) + theme_void() + theme(
    legend.position = "none"
  )

dot_scale <- 5

n_j <- pmax(0L, round(seattle_cd7$Jayapal / dot_scale))
n_a <- pmax(0L, round(seattle_cd7$Alexander / dot_scale))

jayapal_list <- map2(seattle_cd7$geometry, n_j, \(geom, n) if (n > 0) st_sample(geom, n) else NULL)
alexander_list <- map2(seattle_cd7$geometry, n_a, \(geom, n) if (n > 0) st_sample(geom, n) else NULL)

all_jayapal  <- do.call(c, Filter(Negate(is.null), jayapal_list))
all_alexander <- do.call(c, Filter(Negate(is.null), alexander_list))

all_geom_cd7 <- c(all_jayapal, all_alexander)
st_crs(all_geom_cd7) <- st_crs(seattle_cd7)

dots_cd7 <- st_sf(
  candidate = c(rep("Jayapal", length(all_jayapal)), rep("Alexander", length(all_alexander))),
  geometry = all_geom_cd7
)
dots_cd7 <- dots_cd7[sample(nrow(dots_cd7)), ]

cd7dotplot <- ggplot() +
  geom_sf(data = seattle_cd7, fill = "black", color = "gray40", linewidth = 0.25) +
  geom_sf(data = dots_cd7, aes(color = candidate), size = 0.3, alpha = 0.5) +
  scale_color_manual(values = c("Jayapal" = "#2c7bb6", "Alexander" = "#d7191c")) +
  theme_void() +
  theme(
    legend.position = "none"
  )

ggsave("ProblemSet8_Prop1King.png", prop1kingcountyplot)
ggsave("ProblemSet8_Prop1KingTurnout.png", prop1kingcountyturnoutplot)
ggsave("ProblemSet8_Prop1Seattle.png", prop1seattleplot)
ggsave("ProblemSet8_Prop1SeattleTurnout.png", prop1seattleturnoutplot)
ggsave("ProblemSet8_CD7.png", cd7seattleplot)
ggsave("ProblemSet8_CD7Turnout.png", cd7dotplot)
