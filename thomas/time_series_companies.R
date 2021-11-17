library(shiny)
library(tidyverse)
library(janitor)
library(RColorBrewer)
library(lubridate)
library(plotly)
library(shinyWidgets)
library(dplyr)

# Load the sample date
load("to_use.RData")
#trips <- trips_thomas
#rm(trips_thomas)

# Adding dates for months only, years only, and day-month only
date_month <- format(trips$date, "%m")
date_year <- format(trips$date,"%Y")
date_day_month <- format(trips$date, "%m-%d")
trips <- cbind(trips,date_month,date_year,date_day_month)
trips <- rename(trips, date_month = ...4, date_year = ...5, date_day_month = ...6)
rm(date_month,date_year,date_day_month)

# Adding a new one with floor month for each day
month_year <- floor_date(trips$date,"month")
year_floor <- floor_date(trips$date,"year")
trips <- cbind(trips,month_year,year_floor)
trips <- rename(trips, month_year = ...7, year_floor = ...8)
rm(month_year,year_floor)

###############################################
# Checking for the companies that made the most revenue
#trips_2 <- na.omit(trips, cols = c("company"))

my_data <- trips %>%
  filter(company != "") %>%
  select(trip_total, company) %>%
  mutate(trip_total = as.numeric(trip_total)) %>%
  group_by(company) %>%
  summarise(trip_total = sum(trip_total),n()) %>%
  arrange(desc(trip_total))
  
companies <- my_data[1:30,1]
rm(my_data)
companies <- rename(companies, Company = company)

trips <- trips %>%
  filter(company %in% companies$Company)

##############################################
# Data selection function
data_selection <- function(trips, start, end, choice, comp){
  if (is.atomic(comp) == 1) {
    my_data <- trips %>%
      filter((company == comp))
    comp_use <- comp
  } else {
    my_data <- trips %>%
      filter(company %in% comp$Company)
    comp_use <- comp$Company
  }
  dt <- c()
  for (x in comp_use) {
    if (choice == "year"){
      trip_date <- my_data %>%
        filter(year_floor >= start & year_floor <= end & company == x)
      trip_date <- trip_date %>%
        select(date_year, trip_total) %>%
        mutate(trip_total = as.numeric(trip_total)) %>%
        group_by(date_year) %>%
        summarise(trip_total = sum(trip_total))
    } else if (choice == "day") {
      trip_date <- my_data %>%
        filter(date >= start & date <= end & company == x)
      trip_date <- trip_date %>%
        select(date, trip_total) %>%
        mutate(trip_total = as.numeric(trip_total)) %>%
        group_by(date) %>%
        summarise(trip_total = sum(trip_total))
    } else {
      trip_date <- my_data %>%
        filter(month_year >= start & month_year <= end & company == x)
      trip_date <- trip_date %>%
        select(month_year, trip_total) %>%
        mutate(trip_total = as.numeric(trip_total)) %>%
        group_by(month_year) %>%
        summarise(trip_total = sum(trip_total))
    }
    company <- trip_date$trip_total
    trip_date <- cbind(trip_date,company)
    trip_date$company <- x
    rm(company)
    dt <- rbind(dt, trip_date)
  }
  return(dt)
}

# Plotting
time_serie <- function(trips, choice, dates_m, dates_y, dates_d, comp){
  beg <- 2 * length(comp) + 1
  fin <- 3 * length(comp)
  if (choice == "day") {
    start <- dates_d[1]
    end <- dates_d[2]
    p <- ggplot(data = data_selection(trips, start, end, choice, comp),
                aes(x = date, y = trip_total, group = factor(company),
                    colour = factor(company),
                    text = paste('Date :', date,
                                 '<br> Total revenue : $', round(trip_total,digits = 0),
                                 '<br> Company :', factor(company)))) +
      geom_line(size=0.4) +
      labs(x = "Date", y = "Total revenue", color = "Company") +
      scale_x_date(expand = expansion(mult = c(.02, .02))) +
      scale_y_continuous(n.breaks = 10, 
                         expand = expansion(mult = c(.02, .02))) +
      theme(axis.text.x = element_text(angle = 45))
  } else if (choice == "year") {
    dates_y <- floor_date(as.Date(dates_y),"year")
    start <- dates_y[1]
    end <- dates_y[2]
    p <- ggplot(data = data_selection(trips, start, end, choice, comp),
                aes(x = date_year, y = trip_total, group = factor(company),
                    colour = factor(company),
                    text = paste('Date :', date_year,
                                 '<br>Total revenue : $', round(trip_total / 1000000,
                                                               digits = 3), 'M',
                                 '<br>Company :', factor(company)))) +
      geom_line(size=0.4) +
      labs(x = "Date", y = "Total revenue", color = "Company") +
      scale_x_discrete(expand = expansion(mult = c(.02, .02))) +
      scale_y_continuous(n.breaks = 10, 
                         expand = expansion(mult = c(.02, .02))) +
      theme(axis.text.x = element_text(angle = 45))
  } else {
    start <- dates_m[1]
    end <- dates_m[2]
    p <- ggplot(data = data_selection(trips, start, end, choice, comp),
                aes(x = month_year, y = trip_total, group = factor(company),
                    colour = factor(company),
                    text = paste('Date :', format(month_year,"%Y-%m"),
                                 '<br>Total revenue : $', round(trip_total,digits = 0),
                                 '<br>Company :', factor(company)))) +
      geom_line(size=0.4) +
      labs(x = "Date", y = "Total revenue", color = "Company") +
      scale_x_date(expand = expansion(mult = c(.02, .02))) +
      scale_y_continuous(n.breaks = 10, 
                         expand = expansion(mult = c(.02, .02))) +
      theme(axis.text.x = element_text(angle = 45))
  }
  
  p <- ggplotly(p + geom_smooth(size = 0.5), dynamicTicks = TRUE, tooltip = c("text")) %>%
    layout(hovermode = "x", legend = list(orientation = "h"), xaxis = list(side = "top"))
  p <- p %>%
    style(hoverinfo = "skip", traces = beg:fin)
  return(p)
}

# Defining the UI
ui <- fluidPage(
  titlePanel(
    h1("Comparison between companies of total revenue", align = "center")
  ),
  fluidRow(
    column(4,
           selectInput(inputId = "company", label = "Choose which company to visualize:",
                       choices = companies[,1], multiple = TRUE,
                       selected = c("Taxi Affiliation Services","Flash Cab"))),
    column(4,
           selectInput(inputId = "choice",
                       label = "Choose if you want to visualize by day, month or year:",
                       choices = c("Day"="day","Month"="month","Year"="year"),
                       selected = "month")),
    column(4,
           conditionalPanel(
             condition = "input.choice == 'month'",
             airMonthpickerInput(inputId = "dates_month", range = TRUE, dateFormat = "mm-yyyy",
                                 label = "Choose the interval of time you want to visualize :",
                                 minDate = "2013-01-01", maxDate = "2021-10-01",
                                 separator = "  to  ",
                                 view = "months", autoClose = TRUE,
                                 value = c("2013-01-01","2021-10-01"))),
           conditionalPanel(
             condition = "input.choice == 'year'",
             airYearpickerInput(inputId = "dates_year", range = TRUE, dateFormat = "yyyy",
                                label = "Choose the interval of time you want to visualize :",
                                minDate = "2013-01-01", maxDate = "2021-10-01",
                                separator = "  to  ",
                                view = "years", autoClose = TRUE, minView = "years",
                                value = c("2013-01-01","2021-10-01"))),
           conditionalPanel(
             condition = "input.choice == 'day'",
             airDatepickerInput(inputId = "dates_day", range = TRUE, dateFormat = "mm-dd-yyyy",
                                label = "Choose the interval of time you want to visualize :",
                                minDate = "2013-01-01", maxDate = "2021-10-01",
                                separator = "  to  ",
                                autoClose = TRUE,
                                value = c("2013-01-01","2021-10-01"))))
  ),
  fluidRow(
    column(12,
           
             # Hide errors
             tags$style(type = "text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"),
             plotlyOutput("plot")
             #verbatimTextOutput("res")
    )
  )
)


# Defining the server
server = function(input, output) {
  output$plot <- renderPlotly(time_serie(trips, input$choice, input$dates_month,
                                         input$dates_year, input$dates_day,
                                         input$company))
  #output$res <- renderPrint(input$dates_year)
}

shinyApp(ui = ui, server = server)

#########################  Not enough data
  