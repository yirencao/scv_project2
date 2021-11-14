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
load("trips_sample.RData")
trips <- trips_thomas
rm(trips_thomas)

# Adding dates for trip_start
date <- as.Date(trips$trip_start_timestamp,format = "%m/%d/%Y")
trips <- cbind(trips,date)

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
      summarise(trip_total = mean(trip_total))
  } else if (choice == "day") {
    my_data <- trips %>%
      filter(date >= start & date <= end)
    trip_date <- my_data %>%
      select(date, trip_total) %>%
      mutate(trip_total = as.numeric(trip_total)) %>%
      group_by(date) %>%
      summarise(trip_total = mean(trip_total))
  } else {
    my_data <- trips %>%
      filter(month_year >= start & month_year <= end)
    trip_date <- my_data %>%
      select(month_year, trip_total) %>%
      mutate(trip_total = as.numeric(trip_total)) %>%
      group_by(month_year) %>%
      summarise(trip_total = mean(trip_total))
  }
  return(trip_date)
}

time_serie <- function(trips, choice, dates_m, dates_y, dates_d){
  if (choice == "day") {
    start <- dates_d[1]
    end <- dates_d[2]
    p <- ggplot(data = data_selection(trips, start, end, choice),
                aes(x = date, y = trip_total)) +
      geom_line(size=0.4) +
      labs(x = "Date") +
      scale_x_date(expand = expansion(mult = c(.02, .02))) +
      scale_y_continuous(n.breaks = 10, 
                         expand = expansion(mult = c(.02, .02))) +
      theme(axis.text.x = element_text(angle = 45))
  } else if (choice == "year") {
    dates_y <- floor_date(as.Date(dates_y),"year")
    start <- dates_y[1]
    end <- dates_y[2]
    p <- ggplot(data = data_selection(trips, start, end, choice),
                aes(x = date_year, y = trip_total, group=1)) +
      geom_line(size=0.4) +
      labs(x = "Date") +
      scale_x_discrete(expand = expansion(mult = c(.02, .02))) +
      scale_y_continuous(n.breaks = 10, 
                         expand = expansion(mult = c(.02, .02))) +
      theme(axis.text.x = element_text(angle = 45))
  } else {
    start <- dates_m[1]
    end <- dates_m[2]
    p <- ggplot(data = data_selection(trips, start, end, choice),
                aes(x = month_year, y = trip_total)) +
      geom_line(size=0.4) +
      labs(x = "Date") +
      scale_x_date(expand = expansion(mult = c(.02, .02))) +
      scale_y_continuous(n.breaks = 10, 
                         expand = expansion(mult = c(.02, .02))) +
      theme(axis.text.x = element_text(angle = 45))
  }
  
  p <- ggplotly(p)
  return(p)
}
#################

# Defining the UI
ui <- fluidPage(
  sidebarPanel(
    selectInput(inputId = "choice",
                label = "Choose if you want to visualize by day, month or year:",
                choices = c("Day"="day","Month"="month","Year"="year"),
                selected = "month"),
    conditionalPanel(
      condition = "input.choice == 'month'",
      airMonthpickerInput(inputId = "dates_month", range = TRUE, dateFormat = "mm-yyyy",
                          label = "Choose the interval of time you want to visualize :",
                          minDate = "2013-01-01", maxDate = "2021-10-01",
                          separator = "  to  ", value = "2013-01-01",
                          view = "months", autoClose = TRUE)),
    conditionalPanel(
      condition = "input.choice == 'year'",
      airYearpickerInput(inputId = "dates_year", range = TRUE, dateFormat = "yyyy",
                         label = "Choose the interval of time you want to visualize :",
                         minDate = "2013-01-01", maxDate = "2021-10-01",
                         separator = "  to  ", value = "2013-01-01",
                         view = "years", autoClose = TRUE, minView = "years")),
    conditionalPanel(
      condition = "input.choice == 'day'",
      airDatepickerInput(inputId = "dates_day", range = TRUE, dateFormat = "mm-dd-yyyy",
                         label = "Choose the interval of time you want to visualize :",
                         minDate = "2013-01-01", maxDate = "2021-10-01",
                         separator = "  to  ",
                         value = "2013-01-01", autoClose = TRUE))
  ),
  
  # Main panel for displaying outputs
  mainPanel(
    # Hide errors
    tags$style(type = "text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    plotlyOutput("plot")
    #verbatimTextOutput("res")
  )
)

# Defining the server
server = function(input, output) {
  output$plot <- renderPlotly(time_serie(trips, input$choice, input$dates_month,
                                         input$dates_year, input$dates_day))
  #output$res <- renderPrint(input$dates_year)
}

shinyApp(ui = ui, server = server)
