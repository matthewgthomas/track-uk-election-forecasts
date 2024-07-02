library(tidyverse)
library(rvest)
library(jsonlite)

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
