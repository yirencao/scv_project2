library(tidyverse)
library(ggplot2)
library(gganimate)
library(janitor)
library(lubridate)
library(rjson)

#Get workspace: 
load("./olav_used_files/data/companies_time_workspace.RData")


revenue_month <- fromJSON(file = "./data/2013_company_revenue_month.json")

"https://data.cityofchicago.org/resource/wrvz-psew.json?
    $select=date_extract_y(trip_start_timestamp) as year,
            date_extract_m(trip_start_timestamp) as month,
            company,sum(trip_total) as revenue,count(*) as activity
    &$where=year=2013
    &$group=year,month,company"


  
#Setup
taxi_trips <- read_csv("./data/taxi_trips_2020.csv") %>%
  clean_names() %>%
  select(trip_id, taxi_id, trip_start_timestamp, trip_end_timestamp,
         trip_seconds, trip_miles, pickup_community_area, dropoff_community_area, 
         trip_total, company) %>%
  mutate(
    trip_end_timestamp = mdy_hms(trip_end_timestamp),
    trip_start_timestamp = mdy_hms(trip_start_timestamp),
    year = as.intger(year(trip_start_timestamp)),
    month = as.integer(month(trip_start_timestamp)),
    season = case_when(
      month %% 12 <= 2 ~ "Winter",
      month <= 5 ~ "Spring",
      month <= 8 ~ "Summer",
      month <= 11 ~ "Autumn"),
    day = as.integer(day(trip_start_timestamp))  
  )

sample <- taxi_trips %>%
  sample_n(1000) %>%
  print()

theme_set(theme_bw())
#-------------------------------------------------------------------------------

#Sum revenue over each month and plot - easily translated to each year since 2013
revenue_month <- taxi_trips %>%
  select(trip_total, month) %>%
  group_by(month) %>%
  summarise(revenue = sum(trip_total, na.rm = TRUE)) %>%
  print()

revenue_month_plot <- ggplot(
  data = revenue_month,
  mapping = aes(month, revenue)
  ) + 
  geom_line() +
  scale_color_viridis_d()

load("./data/companies_time_workspace.RData")
revenue_month_plot + 
  geom_point() + 
  transition_reveal(month)  

#-------------------------------------------------------------------------------

#Plotting activity (num trips) per day for each season

taxi_trips_seasons_and_days <- taxi_trips %>%
  count(season, day) %>%
  print()
  

activity_days_in_season <- ggplot(
  data = taxi_trips_seasons_and_days,
  mapping = aes(day, n, group = season, color=factor(season))) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Days of month", y = "Number of trips") + 
  theme(legend.position = "top")
activity_days_in_season  

activity_days_in_season + 
  geom_point() + 
  transition_reveal(day)

#-------------------------------------------------------------------------------

#Time series of how each company has grown (don't think we need to use this)

taxi_trips_companies <- taxi_trips %>%
  group_by(month, company) %>%
  summarise(size = n_distinct(taxi_id), #where size = number of taxis in company
            trips = n_distinct(trip_id),
            revenue = sum(trip_total, na.rm = TRUE)) %>%
  print()

trips_per_company <- ggplot(
  data = taxi_trips_companies,
  mapping = aes(x = revenue, y = trips, size = size, color = company)
  ) +
  geom_point(show.legend = FALSE, alpha = 0.7) + 
  scale_color_viridis_d() + 
  scale_size(range = c(2, 12)) + 
  labs(x = "Revenue", y = "Number of trips")
trips_per_company

trips_per_company + 
  transition_time(month) + 
  labs(title = "Month: {frame_time}")

#-------------------------------------------------------------------------------

#Bar chart race of company revenue per month



trips_per_company <- ggplot(
  data = taxi_trips_companies,
  mapping = aes(x = revenue, group = company, 
                color = as.factor(company), fill = as.factor(company))
) +
  geom_tile(mapping = aes(y = revenue), show.legend = FALSE, alpha = 0.7) + 
  scale_color_viridis_d() + 
  scale_size(range = c(2, 12)) + 
  labs(x = "Revenue", y = "Number of trips")


trips_per_company + transition_states(month, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'GDP per Year : {closest_state}',
       subtitle  =  "Top 10 Countries",
       caption  = "GDP in Billions USD | Data Source: World Bank Data")

