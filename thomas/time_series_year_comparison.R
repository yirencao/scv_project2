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


data_selection2 <- function(trips, years, choice){
  my_data <- trips %>%
    filter(date_year %in% years)
  if (choice == "month") {
    trip_date <- my_data %>%
      select(trip_total, month_year) %>%
      mutate(trip_total = as.numeric(trip_total)) %>%
      group_by(month_year) %>%
      summarise(trip_total = median(trip_total))
    year <- format(trip_date$month_year, "%Y")
    date_month <- format(trip_date$month_year, "%m")
    trip_date <- cbind(trip_date,year,date_month)
    #trip_date <- trip_date[,-1]
  } else {
    trip_date <- my_data %>%
      select(trip_total, date) %>%
      mutate(trip_total = as.numeric(trip_total)) %>%
      group_by(date) %>%
      summarise(trip_total = median(trip_total))
    date_year <- format(trip_date$date,"%Y")
    date_day_month <- format(trip_date$date, "%m-%d")
    trip_date <- cbind(trip_date,date_year,date_day_month)
    trip_date <- trip_date[,-1]
  }
  
  return(trip_date)
}

time_serie2 <- function(trips, years, choice){
  if (choice == "month") {
    p <- ggplot(data = data_selection2(trips,years,choice), 
                aes(x = date_month, y = trip_total, group = factor(year),
                    colour = factor(year))) + 
      geom_line(size=0.4) +
      labs(x = "Date", colour = "Year") +
      scale_x_discrete(expand = expansion(mult = c(.02, .02))) +
      theme(axis.text.x = element_text(angle = 45)) +
      scale_y_continuous(n.breaks = 10, 
                         expand = expansion(mult = c(.02, .02)))
  } else {
    p <- ggplot(data = data_selection2(trips,years,choice), 
                aes(x = date_day_month, y = trip_total, group = factor(date_year),
                    colour = factor(date_year))) + 
      geom_line(size=0.4) +
      labs(x = "Date", colour = "Year") +
      scale_x_discrete(breaks = c("01-01","02-01","03-01","04-01","05-01","06-01",
                                  "07-01","08-01","09-01","10-01","11-01","12-01",
                                  "12-31"),
                       expand = expansion(mult = c(.02, .02))) +
      theme(axis.text.x = element_text(angle = 45)) +
      scale_y_continuous(n.breaks = 10, 
                         expand = expansion(mult = c(.02, .02)))
  }
  p <- ggplotly(p)
  return(p)
}

# Defining the UI
ui2 <- fluidPage(
  
  sidebarPanel(
    selectInput(inputId = "choice",
                label = "Choose if you want to visualize by day or month:",
                choices = c("Day"="day","Month"="month"),
                selected = "month"),
    selectInput(inputId = "years",
                label = "Choose which years you wish to compare (two or more):",
                choices = c("2013"="2013","2014"="2014","2015"="2015","2016"="2016",
                            "2017"="2017","2018"="2018","2019"="2019",
                            "2020"="2020","2021"="2021"),
                selected = c("2013","2014"),
                multiple = TRUE)
  ),
  
  # Main panel for displaying outputs
  mainPanel(
    plotlyOutput("plot")
  )
)

# Defining the server
server2 = function(input, output) {
  output$plot <- renderPlotly(time_serie2(trips, input$years, input$choice))
}

shinyApp(ui = ui2, server = server2)
