library(shiny)
library(dplyr)
library(readr)
library(plotly)
library(ggplot2)
library(forcats)
library(stringr)
library(lubridate)
library(yardstick)

# ---- Load data ----
forecasts <- read_csv("forecasts.csv")

recent_forecasts <- 
  forecasts |> 
  group_by(Forecaster) |> 
  filter(Week == max(Week)) |> 
  ungroup()

lab_forecasts <- 
  recent_forecasts |> 
  filter(Party == "Lab")

con_forecasts <- 
  recent_forecasts |> 
  filter(Party == "Con")

# Try to load election results
election_results <- read_csv("results.csv")

show_election_results <- !is.na(election_results$`Actual seats`[1])
show_exit_poll <- !is.na(election_results$`Exit poll`[1])

# Which forecaster(s) were most accurate?
if (show_election_results) {
  forecasts_and_results <-
    recent_forecasts |>
    left_join(election_results) |>
    mutate(
      Difference = abs(Seats - `Actual seats`)
    )

  # Who was closest for Labour and Conservative?
  lab_accuracy <-
    forecasts_and_results |>
    filter(Party == "Lab") |>
    filter(Difference == min(Difference, na.rm = TRUE)) |>
    distinct(Forecaster) |>
    pull(Forecaster)

  con_accuracy <-
    forecasts_and_results |>
    filter(Party == "Con") |>
    filter(Difference == min(Difference, na.rm = TRUE)) |>
    distinct(Forecaster) |>
    pull(Forecaster)

  # Who was closest overall, using mean absolute error?
  overall_accuracy_mae <-
    forecasts_and_results |>
    filter(Party %in% election_results$Party) |>
    group_by(Forecaster) |>
    mae(Seats, `Actual seats`) |>
    filter(.estimate == min(.estimate, na.rm = TRUE))
  
  overall_accuracy <- 
    overall_accuracy_mae |> 
    pull(Forecaster)
  
  overall_accuracy_mae <- overall_accuracy_mae$.estimate[1]
  
  # How accurate was the exit poll?
  exit_poll_accuracy_mae <-
    election_results |>
    mae(`Exit poll`, `Actual seats`) |>
    pull(.estimate)
}

# Get dates
election_date <- ymd("2024-07-04")
current_date <- max(forecasts$Week)

# ---- UI ----
ui <- fluidPage(
    titlePanel("Seat Seer: Tracking seat predictions for the UK's General Election"),
    
    uiOutput("results"),

    h3("Every forecaster predicted a Labour majority"),
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
    p("I use the terms 'forecast', 'prediction' and 'projection' interchangeably, although technically they refer to different things."),
    p(a("App and analysis", href = "https://github.com/matthewgthomas/track-uk-election-forecasts"), "by", a("Matthew Gwynfryn Thomas", href = "https://github.com/matthewgthomas"))
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
    
    # Include exit poll, if known
    if (show_exit_poll) {
      plt <- 
        plt +
        geom_point(data = election_results, aes(x = Party, y = `Exit poll`, text = str_glue("Exit poll predicts {Party} has won {`Exit poll`} seats.")), shape = 4, size = 3) +
        theme(plot.title.position = "plot") +
        labs(
          title = "Projected number of seats (dots) and exit poll (crosses)",
          y = "Number of seats"
        )
    }
    
    # Include actual seats won, if known
    if (show_election_results) {
      plt <-
        plt +
        geom_point(data = election_results, aes(x = Party, y = `Actual seats`, text = str_glue("{Party} won {`Actual seats`} seats.")), shape = 2, size = 4) +
        theme(plot.title.position = "plot") +
        labs(
          title = "Projected number of seats (dots), exit poll (crosses), and actual seats won (triangles)",
          y = "Number of seats"
        )
    }
    
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
    
    if (show_election_results) {
      plt <-
        plt +
        geom_hline(data = election_results |> filter(Party == "Lab"), aes(yintercept = `Actual seats`), linetype = 2, colour = "#d50000") +
        geom_hline(data = election_results |> filter(Party == "Con"), aes(yintercept = `Actual seats`), linetype = 2, colour = "#0087dc")
    }
    
    ggplotly(plt)
  })
  
  output$results <- renderUI({
    if (show_election_results) {
      forecast_word <- ifelse(length(overall_accuracy) == 1, "forecast", "forecasts")
      
      exit_poll_message <- ifelse(
        exit_poll_accuracy_mae < overall_accuracy_mae, 
        "However, the exit poll was more accurate than all the pre-election projections",
        ""
      )
      
      div(
        h3(str_glue("{str_flatten_comma(overall_accuracy, ', and')} published the most accurate {forecast_word} overall")),
        p(str_glue("{str_flatten_comma(lab_accuracy, ', and')} most closely predicted Labour's seats.")),
        p(str_glue("{str_flatten_comma(con_accuracy, ', and')} most closely predicted the number of Conservative seats.")),
        p(exit_poll_message)
      )
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
