library(tidyverse)
library(yardstick)

forecasts <- read_csv("forecasts.csv")
election_results <- read_csv("results.csv")

recent_forecasts <- 
  forecasts |> 
  group_by(Forecaster) |> 
  filter(Week == max(Week)) |> 
  ungroup() |> 
  select(-Week)

# Calculate mean forecasts for each party
combined_forecast <- 
  recent_forecasts |> 
  drop_na() |> 
  group_by(Party) |> 
  summarise(Seats = mean(Seats)) |> 
  ungroup() |> 
  mutate(Forecaster = "Average of recent forecasts")

exit_poll <- 
  election_results |> 
  mutate(Forecaster = "Exit poll") |> 
  select(Forecaster, Party, Seats = `Exit poll`)

forecasts_and_results <-
  recent_forecasts |>
  add_row(combined_forecast) |> 
  add_row(exit_poll) |> 
  left_join(election_results) |> 
  drop_na()
  
# Who was closest overall, using mean absolute error?
overall_accuracy_mae <-
  forecasts_and_results |>
  filter(Party %in% election_results$Party) |>
  group_by(Forecaster) |>
  mae(Seats, `Actual seats`)

overall_accuracy_mae |> 
  arrange(.estimate)
