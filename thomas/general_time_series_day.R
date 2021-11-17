library(shiny)
library(tidyverse)
library(ggmap)
library(janitor)
library(sf)
library(mapview)
library(leaflet)
library(rgdal)
library(RColorBrewer)
library(lubridate)
library(plotly)
library(reshape2)
library(shinyWidgets)

#load("Taxi_Trips.RData")

#taxi_trips <- subset(taxi_trips, select = c("Trip.Start.Timestamp","Trip.End.Timestamp",
#                                            "Trip.Total","Pickup.Community.Area",
#                                            "Dropoff.Community.Area","Company",
#                                            "Trip.Miles","Trip.Seconds"))
# DATA WITHOUT NA
#taxi_trips <- na.omit(taxi_trips,cols=c("Trip.Start.Timestamp","Trip.End.Timestamp",
#                                        "Trip.Total","Pickup.Community.Area",
#                                        "Dropoff.Community.Area"))
#taxi_trips <- clean_names(taxi_trips)

# Taking a sample of taxi_trips
#trips <- slice_sample(taxi_trips,n=500000)

# Load the sample date
load("to_use.RData")

# Adding dates for months only, years only, and day-month only
date_month <- format(trips$date, "%m")
date_year <- format(trips$date,"%Y")
date_day_month <- format(trips$date, "%m-%d")
trips <- cbind(trips,date_month,date_year,date_day_month)

# Adding a new one with floor month for each day
month_year <- floor_date(trips$date,"month")
year_floor <- floor_date(trips$date,"year")
trips <- cbind(trips,month_year,year_floor)

data_selection <- function(trips, start, end, choice){
  if (choice == "year"){
    my_data <- trips %>%
      filter(year_floor >= start & year_floor <= end)
    trip_date <- my_data %>%
      select(date_year, trip_total) %>%
      mutate(trip_total = as.numeric(trip_total)) %>%
      group_by(date_year) %>%
      summarise(trip_total = sum(trip_total))
  } else if (choice == "day") {
    my_data <- trips %>%
      filter(date >= start & date <= end)
    trip_date <- my_data %>%
      select(date, trip_total) %>%
      mutate(trip_total = as.numeric(trip_total)) %>%
      group_by(date) %>%
      summarise(trip_total = trip_total)
  } else {
    my_data <- trips %>%
      filter(month_year >= start & month_year <= end)
    trip_date <- my_data %>%
      select(month_year, trip_total) %>%
      mutate(trip_total = as.numeric(trip_total)) %>%
      group_by(month_year) %>%
      summarise(trip_total = sum(trip_total))
  }
  return(trip_date)
}

time_serie <- function(trips, choice, dates_m, dates_y, dates_d){
  if (choice == "day") {
    start <- dates_d[1]
    end <- dates_d[2]
    p <- ggplot(data = data_selection(trips, start, end, choice),
                aes(x = date, y = trip_total, group = 1,
                    text = paste('Date :', date,
                                 '<br>Total revenue : $',  round(trip_total,digits = 0)))) +
      geom_line(size=0.4,color = "blue") +
      labs(x = "Date", y = "Total revenue") +
      scale_x_date(expand = expansion(mult = c(.02, .02))) +
      scale_y_continuous(n.breaks = 10, 
                         expand = expansion(mult = c(.02, .02))) +
      theme(axis.text.x = element_text(angle = 45))
  } else if (choice == "year") {
    dates_y <- floor_date(as.Date(dates_y),"year")
    start <- dates_y[1]
    end <- dates_y[2]
    p <- ggplot(data = data_selection(trips, start, end, choice),
                aes(x = date_year, y = trip_total, group=1,
                    text = paste('Year :', date_year,
                                 '<br>Total revenue : $', round(trip_total / 1000000,
                                                                digits = 3), 'M'))) +
      geom_line(size=0.4,color = "blue") +
      labs(x = "Date", y = "Total revenue") +
      scale_x_discrete(expand = expansion(mult = c(.02, .02))) +
      scale_y_continuous(n.breaks = 10, 
                         expand = expansion(mult = c(.02, .02))) +
      theme(axis.text.x = element_text(angle = 45))
  } else {
    start <- dates_m[1]
    end <- dates_m[2]
    p <- ggplot(data = data_selection(trips, start, end, choice),
                aes(x = month_year, y = trip_total, group=1,
                    text = paste('Date :', format(month_year,"%Y-%m"),
                                 '<br>Total revenue : $', round(trip_total / 1000000,
                                                                digits = 3), 'M'))) +
      geom_line(size=0.4,color = "blue") +
      labs(x = "Date", y = "Total revenue") +
      scale_x_date(expand = expansion(mult = c(.02, .02))) +
      scale_y_continuous(n.breaks = 10, 
                         expand = expansion(mult = c(.02, .02))) +
      theme(axis.text.x = element_text(angle = 45))
  }
  
  p <- ggplotly(p + geom_smooth(size = 0.5,color = "red"), dynamicTicks = TRUE, tooltip = c("text")) %>%
    layout(hovermode = "x")
  p <- p %>%
    style(hoverinfo = "skip", traces = 3)
  return(p)
}
#################

# Defining the UI
ui <- fluidPage(
  titlePanel(
    h1("Total revenue per day, month or year", align = "center")
  ),
  fluidRow(
    column(6,
           selectInput(inputId = "choice",
                       label = "Choose if you want to visualize by day, month or year:",
                       choices = c("Day"="day","Month"="month","Year"="year"),
                       selected = "month")),
    column(6,
           conditionalPanel(
             condition = "input.choice == 'month'",
             airMonthpickerInput(inputId = "dates_month", range = TRUE, dateFormat = "mm-yyyy",
                                 label = "Choose the interval of time you want to visualize :",
                                 minDate = "2013-01-01", maxDate = "2021-10-01",
                                 separator = "  to  ", value = c("2013-01-01","2021-10-01"),
                                 view = "months", autoClose = TRUE)),
           conditionalPanel(
             condition = "input.choice == 'year'",
             airYearpickerInput(inputId = "dates_year", range = TRUE, dateFormat = "yyyy",
                                label = "Choose the interval of time you want to visualize :",
                                minDate = "2013-01-01", maxDate = "2021-10-01",
                                separator = "  to  ", value = c("2013-01-01","2021-10-01"),
                                view = "years", autoClose = TRUE, minView = "years")),
           conditionalPanel(
             condition = "input.choice == 'day'",
             airDatepickerInput(inputId = "dates_day", range = TRUE, dateFormat = "mm-dd-yyyy",
                                label = "Choose the interval of time you want to visualize :",
                                minDate = "2013-01-01", maxDate = "2021-10-01",
                                separator = "  to  ",
                                value = c("2013-01-01","2021-10-01"), autoClose = TRUE)))
  ),
  fluidRow(
    column(12,
           # Hide errors
           tags$style(type = "text/css",
                      ".shiny-output-error { visibility: hidden; }",
                      ".shiny-output-error:before { visibility: hidden; }"),
           plotlyOutput("plot"))
  )
)

# Defining the server
server = function(input, output) {
  output$plot <- renderPlotly(time_serie(trips, input$choice, input$dates_month,
                                         input$dates_year, input$dates_day))
  #output$res <- renderPrint(input$dates_year)
}

shinyApp(ui = ui, server = server)

