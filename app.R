library(shiny)
library(googlesheets4)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(forcats)

# ---- Load data ----
# - Fetch data from projection models -
gs4_deauth()

projections_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1R1c7oT6T8uCl8VlYhSIyjlLxy8_7J-p3voXcg-9T5fE/edit?gid=996500119#gid=996500119", sheet = "Projections")

projections <- 
  projections_raw |> 
  mutate(Week = ceiling_date(`Date published`, "1 week")) |> 
  select(Week, Forecaster, Con, Lab, SNP:Others) |> 
  pivot_longer(Con:Others, names_to = "Party", values_to = "Seats")

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
  mutate(Week = ceiling_date(Published, "1 week")) |> 
  
  # Keep polls that were conducted after the General Election was called
  filter(Week >= ymd("2024-05-22")) |> 
  
  select(Week, Forecaster = Pollster, Con:Others) |> 
  pivot_longer(cols = Con:Others, names_to = "Party", values_to = "Seats")

#TODO: Refactor this into a function

# - Fetch data from the New Statesman's model -
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

# - Combine forecasts -
forecasts <- bind_rows(projections, mrp, ns_forecast)

recent_forecasts <- 
  forecasts |> 
  filter(Week == max(Week))

lab_forecasts <- 
  recent_forecasts |> 
  filter(Party == "Lab")

con_forecasts <- 
  recent_forecasts |> 
  filter(Party == "Con")

# Get dates
election_date <- ymd("2024-07-04")
current_date <- max(forecasts$Week)

# ---- UI ----
ui <- fluidPage(
    titlePanel("Seat Seer: Tracking seat predictions for the UK's General Election"),

    h3("Every forecaster is predicting a Labour majority"),
    p(str_glue("As of {format(current_date, '%A %d %B %Y')}, Labour is projected to win anywhere from {min(lab_forecasts$Seats)} to {max(lab_forecasts$Seats)} seats. The Conservatives could win between {min(con_forecasts$Seats)} and {max(con_forecasts$Seats)} seats.")),
    p(str_glue("Combining these forecasts, Labour could win {round(mean(lab_forecasts$Seats), 0)} seats while the Conservatives could win {round(mean(con_forecasts$Seats), 0)}.")),
    plotlyOutput("most_recent_forecasts")
)

# ---- Server ----
server <- function(input, output) {
  output$most_recent_forecasts <- renderPlotly({
    plt <- 
      recent_forecasts |>
      ggplot(aes(x = reorder(Party, Seats, sum, na.rm = TRUE), y = Seats, colour = Party)) +
      geom_point(aes(text = str_glue("{Forecaster} projects {Party} will win {Seats} seats.")), show.legend = FALSE) +
      coord_flip() +
      scale_y_continuous(position = "right") +
      scale_color_manual(values = c("#0087dc", "#6AB023", "#d50000", "#F6B527", "gray", "#3B822B", "#12B6CF", "#F8ED7E")) +
      theme_minimal() +
      theme(
        legend.position = "none"
      ) +
      labs(
        x = NULL,
        y = "Projected number of seats"
      )
    
    ggplotly(plt, tooltip = "text")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
