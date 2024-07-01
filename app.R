library(shiny)
library(googlesheets4)
library(rvest)
library(dplyr)
library(stringr)

# ---- Load data ----
# - Fetch data from Economist and Telegraph models -
gs4_deauth()

economist <- read_sheet("https://docs.google.com/spreadsheets/d/1R1c7oT6T8uCl8VlYhSIyjlLxy8_7J-p3voXcg-9T5fE/edit?gid=996500119#gid=996500119", sheet = "The Economist")

telegraph <- read_sheet("https://docs.google.com/spreadsheets/d/1R1c7oT6T8uCl8VlYhSIyjlLxy8_7J-p3voXcg-9T5fE/edit?gid=996500119#gid=996500119", sheet = "The Telegraph")

# - Fetch MRP poll data from Wikipedia -
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
  mutate(Week = isoweek(Published)) |> 
  
  # Keep polls that were conducted after the General Election was called
  filter(Week >= 22) |> 
  
  select(Week, Con:Others)

#TODO: Refactor this into a function

# Get dates
current_date <- max(economist$`Date published`)

# ---- UI ----
ui <- fluidPage(
    titlePanel("Seat Seer"),

    h3("What the forecasts are predicting"),
    p("As of", format(current_date, "%A %d %B %Y"), ", blah")
    
    
)

# ---- Server ----
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
