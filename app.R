library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(forcats)
library(stringr)
library(lubridate)

# ---- Load data ----
forecasts <- read_csv("forecasts.csv")

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
    plotlyOutput("most_recent_forecasts"),

    h3("How have the forecasts changed over recent weeks?"),    
    plotlyOutput("trends"),
    
    h3("About the forecasts"),
    p("This app brings together various prediction models from several forecasters:"),
    tags$ul(
      tags$li(a("The Economist", href = "https://www.economist.com/interactive/uk-general-election/forecast")),
      tags$li(a("ElectionMapsUK", href = "https://electionmaps.uk/nowcast")),
      tags$li(a("Electoral Calculus", href = "https://www.electoralcalculus.co.uk/prediction_main.html")),
      tags$li(a("The New Statesman", href = "https://sotn.newstatesman.com/2024/05/britainpredicts")),
      tags$li(a("Principal Fish", href = "https://principalfish.co.uk/electionmaps/?map=prediction_new")),
      tags$li(a("The Telegraph", href = "https://www.telegraph.co.uk/news/UK-election-predictions/")),
      tags$li(span("Multilevel regression with poststratification (MRP) and stacked regression with poststratification (SRP) polls", a("scraped from Wikipedia", href = "https://en.m.wikipedia.org/wiki/Opinion_polling_for_the_2024_United_Kingdom_general_election#MRP_and_SRP_polls")))
    ),
    p("I use the terms 'forecast', 'prediction' and 'projection' interchangeably, although technically they refer to different things.")
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
  
  output$trends <- renderPlotly({
    plt <- 
      forecasts |> 
      filter(Party %in% c("Lab", "Con")) |> 
      ggplot(aes(x = Week, y = Seats, group = Forecaster)) +
      geom_line(data = forecasts |> filter(Party == "Lab"), colour = "#d50000") +
      geom_line(data = forecasts |> filter(Party == "Con"), colour = "#0087dc") +
      geom_point(data = forecasts |> filter(Party == "Lab"), colour = "#d50000", size = 2) +
      geom_point(data = forecasts |> filter(Party == "Con"), colour = "#0087dc", size = 2) +
      scale_y_continuous(limits = c(0, NA)) +
      theme_minimal() +
      theme(
        legend.position = "none"
      ) +
      labs(
        x = NULL,
        y = "Projected number of seats"
      )
    
    ggplotly(plt)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
