library(tidyverse)
library(rvest)
library(jsonlite)
library(sf)
library(googlesheets4)

# ---- List of constituencies ----
# Source: https://geoportal.statistics.gov.uk/datasets/ons::westminster-parliamentary-constituencies-july-2024-names-and-codes-in-the-uk-v2/about
cons <- read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/PCON_2024_UK_NC_v2/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

cons <- 
  cons |> 
  st_drop_geometry() |> 
  select(constituency_code = PCON24CD, constituency_name = PCON24NM)

# ---- Scrape nowcasts from ElectionMapsUK ----
url <- "https://flo.uri.sh/visualisation/17427609/embed?auto=1"

electionmapsuk <- 
  read_html_live(url)

flourish_script <- 
  electionmapsuk |> 
  html_elements("script") |> 
  html_text()

# json_regex <- "\\{(?:(?>[^{}\\"'\\/]+)|(?>"(?:(?>[^\\\\"]+)|\\\\.)*\\")|(?>'(?:(?>[^\\\\']+)|\\\\.)*')|(?>\\/\\/.*\\n)|(?>\\/\\*.*?\\*\\/)|(?-1))*\\}"

flourish_data_string <- 
  flourish_script[4] |> 
  str_extract("_Flourish_data = \\{.*\\}") |> 
  str_remove("_Flourish_data = ")

flourish_data_list <- jsonlite::fromJSON(flourish_data_string, flatten = TRUE)

flourish_data <- do.call(rbind, flourish_data_list$regions$metadata)

flourish_data <- 
  flourish_data |> 
  as_tibble() |> 
  select(
    constituency_code = V1, 
    constituency_name = V2, 
    prediction = V6
  ) |> 
  filter(!str_detect(constituency_code, "^N"))  # Keep GB predictions only

unique(flourish_data$prediction)

flourish_data <- 
  flourish_data |> 
  mutate(prediction = case_when(
    str_detect(prediction, "LAB") ~ "Lab",
    str_detect(prediction, "CON") ~ "Con",
    str_detect(prediction, "LDM") ~ "Lib Dems",
    str_detect(prediction, "RFM") ~ "Reform",
    str_detect(prediction, "SNP") ~ "SNP",
    str_detect(prediction, "GRN") ~ "Green",
    str_detect(prediction, "PLC") ~ "Plaid Cymru",
    str_detect(prediction, "SPKR") ~ "Speaker",
  ))

write_csv(flourish_data, str_glue("election-maps-uk-{format(lubridate::now(), '%Y-%m-%d')}.csv"))

# ---- Scrape Principal Fish predictions ----
url <- "https://principalfish.co.uk/electionmaps/?map=prediction_new"

principal_fish_web <- 
  read_html_live(url)

principal_fish_web$click("#seatlist-extend", n_clicks = 1)

principal_fish_data <- 
  principal_fish_web |> 
  html_elements("#seatlist-extended-data")

principal_fish_constituencies <- 
  principal_fish_data |> 
  html_elements(".extended-seat") |> 
  html_text()

principal_fish_predictions <- 
  principal_fish_data |> 
  html_elements("div.party-flair") |> 
  html_attr("class") |> 
  str_remove("party-flair ")

principal_fish <- tibble(constituency_name = principal_fish_constituencies, prediction = principal_fish_predictions)

principal_fish <- 
  principal_fish |> 
  left_join(cons) |> 
  relocate(constituency_code) |> 
  filter(!str_detect(constituency_code, "^N")) |>   # Keep GB predictions only  
  mutate(prediction = case_match(
    prediction,
    "labour" ~ "Lab",
    "conservative" ~ "Con",
    "libdems" ~ "Lib Dems",
    "snp" ~ "SNP",
    "green" ~ "Green",
    "plaidcymru" ~ "Plaid Cymru",
    "other" ~ "Other"
    # This site wasn't predicting any Reform seats at the time of coding
  ))

unique(principal_fish$prediction)

write_csv(principal_fish, str_glue("principal-fish-{format(lubridate::now(), '%Y-%m-%d')}.csv"))

# ---- The Economist ----
# Fetch the data from their MRP model
# Source: https://github.com/TheEconomist/britain-mrp-2024-estimates
economist <- read_csv("https://github.com/TheEconomist/britain-mrp-2024-estimates/raw/main/economist_wethink_2024_mrp.csv")

economist <- 
  economist |> 
  select(constituency_code = const_cd, constituency_name = const_nm, prediction = winner24) |> 
  mutate(prediction = case_match(
    prediction,
    "lab" ~ "Lab",
    "con" ~ "Con",
    "ld" ~ "Lib Dems",
    "snp" ~ "SNP",
    "green" ~ "Green",
    "pc" ~ "Plaid Cymru",
    "ref" ~ "Reform",
    "speaker" ~ "Speaker"
  ))

write_csv(economist, str_glue("economist-mrp-{format(lubridate::now(), '%Y-%m-%d')}.csv"))

# ---- The Economist (ensemble model) ----
economist <- 
  read_html_live("https://www.economist.com/interactive/uk-general-election/forecast")

econ_html <- 
  economist |> 
  html_elements(xpath="*") |> 
  html_text()

str_extract(econ_html[145], "Stratford")

economist$view()

economist_predictions <- 
  cons |> 
  mutate(prediction = NA_character_)

economist$click("#by-constituency-sidebar")
economist$click(".svelte-select")



for (i in 1:nrow(economist_predictions)) {
  economist$type(".value-container .svelte-82qwg8", economist_predictions$constituency_name[i])
  economist$press(".value-container .svelte-82qwg8", key_code = "Enter")
  
  Sys.sleep(0.1)
  
  economist_predictions$prediction[i] <- 
    economist |> 
    html_elements(".constituency-probability") |> 
    html_text()
  
  print(paste0("Scraped ", economist_predictions$constituency_name[i]))
  Sys.sleep(0.1)
}




# ---- Electoral Calculus ----
ec_data_url <- "https://www.electoralcalculus.co.uk/fcgi-bin/calcwork23.py?seat="

ec <- 
  cons |> 
  mutate(ec_name = case_when(
    constituency_code == "S14000027" ~ "Na h-Eileanan An Iar (Western Isles)",
    .default = constituency_name
  )) |> 
  mutate(ec_name = str_replace_all(ec_name, " ", "+")) |> 
  mutate(ec_name = str_replace_all(ec_name, ",", "%2C")) |> 
  mutate(prediction = NA_character_)

for (i in 1:nrow(ec)) {
  ec_page <- 
    read_html(paste0(ec_data_url, ec$ec_name[i]))
  
  ec_prediction <- 
    ec_page |> 
    html_element(".pills") |> 
    html_text() |> 
    str_remove("Prediction: ")
  
  ec$prediction[i] <- ec_prediction
  
  print(paste0("Scraped ", ec$ec_name[i]))
  Sys.sleep(1)
}

# ---- New Statesman ----
gs4_deauth()  # Don't need auth
ns_url <- "https://docs.google.com/spreadsheets/d/1lh0YfXwxNqLQXTvH-wKu0_zG1SVKjLqCTls7WTEXiVk/"

ns_sheets <- 
  sheet_names(ns_url) |> 
  set_names()

ns_latest_raw <- 
  read_sheet(ns_url, sheet = ns_sheets[1])

ns_forecast <- 
  ns_latest_raw |> 
  mutate(prediction = case_match(
    Win,
    "LDem" ~ "Lib Dems",
    "Ref" ~ "Reform",
    "Grn" ~ "Green",
    "Oth" ~ "Other",
    "PC" ~ "Plaid Cymru",
    .default = Win
  )) |> 
  
  left_join(cons, by = c("Constituency" = "constituency_name")) |> 
  select(constituency_code, prediction)

write_csv(ns_forecast, "new-statesman-2024-07-03.csv")
