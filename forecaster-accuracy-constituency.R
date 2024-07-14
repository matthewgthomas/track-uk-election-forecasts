library(tidyverse)

# ---- Load constituency results ----
results <- read_csv("https://github.com/matthewgthomas/ad-hoc-analysis/raw/main/analysis/elections/data/constituency-results-2024.csv")

# Rename winning parties to match the names in the forecasts
results <- 
  results |> 
  select(constituency_code = cons_code, winner) |> 
  mutate(winner = case_match(
    winner,
    "Labour" ~ "Lab",
    "Conservative" ~ "Con",
    "LibDem" ~ "Lib Dems",
    .default = winner
  ))

# ---- Calculate accuracy for constituency-level forecasts ----
forecast_economist <- read_csv("economist-mrp-2024-07-02.csv")

accuracy_economist <- 
  forecast_economist |> 
  left_join(results) |> 
  mutate(correct = if_else(prediction == winner, "correct", "incorrect")) |> 
  count(winner, correct) |> 
  mutate(forecaster = "The Economist (MRP)")

forecast_election_maps <- read_csv("election-maps-uk-2024-07-02.csv")

accuracy_election_maps <- 
  forecast_election_maps |> 
  left_join(results) |> 
  mutate(correct = if_else(prediction == winner, "correct", "incorrect")) |> 
  count(winner, correct) |> 
  mutate(forecaster = "Election Maps UK")

forecast_principal_fish <- read_csv("principal-fish-2024-07-02.csv")

accuracy_principal_fish <- 
  forecast_principal_fish |> 
  left_join(results) |> 
  mutate(correct = if_else(prediction == winner, "correct", "incorrect")) |> 
  count(winner, correct) |> 
  mutate(forecaster = "Principal Fish")

forecast_new_statesman <- read_csv("new-statesman-2024-07-03.csv")

accuracy_new_statesman <- 
  forecast_new_statesman |> 
  left_join(results) |> 
  mutate(correct = if_else(prediction == winner, "correct", "incorrect")) |> 
  count(winner, correct) |> 
  mutate(forecaster = "New Statesman")

# ---- How did they do? ----
accuracy_overall <- 
  bind_rows(
    accuracy_economist, accuracy_election_maps, accuracy_new_statesman, accuracy_principal_fish
  )

# Who was most accurate overall?
accuracy_overall |> 
  group_by(forecaster, correct) |> 
  summarise(n = sum(n)) |> 
  drop_na() |> 
  pivot_wider(names_from = "correct", values_from = "n") |> 
  arrange(desc(correct), incorrect)

# Per-party accuracy
accuracy_overall |> 
  drop_na() |> 
  ggplot(aes(x = winner, y = n)) +
  geom_point(aes(colour = forecaster), position = position_jitter(height = 0)) +
  facet_wrap(~correct)

accuracy_overall |> 
  filter(correct == "correct") |> 
  group_by(winner) |> 
  slice_max(n)
