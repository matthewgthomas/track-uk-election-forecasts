library(googlesheets4)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(tidyr)
library(lubridate)
library(readr)

# ---- Fetch data from projection models ----
gs4_deauth()

projections_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1R1c7oT6T8uCl8VlYhSIyjlLxy8_7J-p3voXcg-9T5fE/edit?gid=996500119#gid=996500119", sheet = "Projections")

projections <- 
  projections_raw |> 
  mutate(Week = ceiling_date(`Date published`, "1 week")) |> 
  select(Week, Forecaster, Con, Lab, SNP:Others) |> 
  pivot_longer(Con:Others, names_to = "Party", values_to = "Seats")

# ---- Fetch MRP poll data from Wikipedia ----
tables <- 
  read_html("https://en.m.wikipedia.org/wiki/Opinion_polling_for_the_2024_United_Kingdom_general_election") |> 
  # list all tables on the page
  html_elements(css = "table")

# Convert to a table
mrp <- 
  tables[[6]] |> 
  html_table(fill = T)

# Clean up MRP data
mrp <- 
  mrp |> 
  slice(-1) |> 
  
  # Remove footnotes from the seat projections
  mutate(across(Con:Others, ~str_remove(.x, "\\[.+\\]") |> as.integer(.x))) |> 
  filter(!is.na(Con)) |> 
  
  # Get week numbers from the date the poll finished
  mutate(Published = str_extract(Datesconducted, "[0-9]+ [A-Za-z]{3} 2024")) |> 
  mutate(Published = dmy(Published)) |> 
  mutate(Week = ceiling_date(Published, "1 week")) |> 
  
  # Keep polls that were conducted after the General Election was called
  filter(Week >= ymd("2024-05-22")) |> 
  
  select(Week, Forecaster = Pollster, Con:Others) |> 
  pivot_longer(cols = Con:Others, names_to = "Party", values_to = "Seats")

# ---- Fetch data from the New Statesman's model ----
ns_url <- "https://docs.google.com/spreadsheets/d/1lh0YfXwxNqLQXTvH-wKu0_zG1SVKjLqCTls7WTEXiVk/"

ns_sheets <- 
  sheet_names(ns_url) |> 
  set_names()

ns_forecast_raw <- 
  ns_sheets[1:(length(ns_sheets) - 2)] |> 
  map_df(~read_sheet(ns_url, sheet = .x), .id = "sheet")

ns_forecast <- 
  ns_forecast_raw |> 
  rename(
    `Lib Dems` = LDem,
    Green = Grn,
    Reform = Ref,
    `Plaid Cymru` = PC,
    Others = Ind_Oth
  ) |> 
  
  # Find the party projected to win in each constituency
  rownames_to_column("id") |> 
  pivot_longer(Con:Others, names_to = "Party", values_to = "VoteShare") |> 
  group_by(id) |> 
  slice_max(VoteShare) |> 
  ungroup() |> 
  
  group_by(sheet) |> 
  count(Party, name = "Seats") |> 
  ungroup() |> 
  
  mutate(Date = mdy(paste0(sheet, "/2024"))) |> 
  mutate(Week = ceiling_date(Date, "1 week")) |> 
  mutate(Forecaster = "New Statesman") |> 
  select(Week, Forecaster, Party, Seats)

# ---- Combine forecasts ----
forecasts <- bind_rows(projections, mrp, ns_forecast)

write_csv(forecasts, "forecasts.csv")
