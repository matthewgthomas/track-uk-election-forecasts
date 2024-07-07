library(tidyverse)
library(googlesheets4)
library(plotly)

gs4_deauth()  # Don't need auth for my Google Sheet
sweepstake <- read_sheet("https://docs.google.com/spreadsheets/d/1R1c7oT6T8uCl8VlYhSIyjlLxy8_7J-p3voXcg-9T5fE/edit?gid=996500119#gid=996500119", sheet = "Sweepstake")

# Were later entries into the sweepstake more accurate?
sweepstake |> 
  ggplot(aes(x = `Order of sweepstake entry`, y = `Difference with actual Lab seats`)) +
  geom_point(aes(text = Name)) +
  geom_smooth(method = "lm")

ggplotly()

summary(lm(`Overall accuracy (mean absolute error)` ~ `Order of sweepstake entry`, data = sweepstake))
