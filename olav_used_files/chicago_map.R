#Plotting a map of Chicago

library(tidyverse)
library(sf)
library(mapview)
library(ggmap)
library(leaflet)
library(rgdal)
library(RColorBrewer)
library(janitor)
library(lubridate)
library(boot)



#Register API key to google
register_google(key="AIzaSyBT6yzDKV_DGcFGK9E-cXu0zNUD4WTJOZA")

taxi_trips<-read_csv("./data/taxi_trips_2020.csv") %>%
  clean_names() 

trip_total_in_area <- taxi_trips %>%
  select(pickup_community_area, trip_total) %>%
  mutate(pickup_community_area = as.factor(pickup_community_area),
         trip_total = (as.numeric(pickup_community_area))) %>%
  group_by(pickup_community_area) %>%
  summarise(trip_total = sum(trip_total))
            
#Get location of chicago
chicago <- geocode("Chicago, IL")

#Make leaflet map with chicago center
chicago_leaflet <- leaflet() %>% 
  setView(lng=chicago$lon, lat=chicago$lat, zoom=10) %>%
  addProviderTiles(providers$CartoDB.Positron) 

#Save chicago areas to use for plotting
areas <- st_read("./data/chicago_community_areas/chicago_community_areas.shp", as_tibble=T, quiet=T) %>%
  clean_names() %>%
  select(area_numbe, community, geometry) %>%
  mutate(geometry = st_geometry(geometry),
         area_numbe = as.factor(area_numbe)) %>% #if this stops working extract it to own variable 
  rename(pickup_community_area = area_numbe,
         community_name = community)

areas_trip_total <- areas %>%
  inner_join(trip_total_in_area, by="pickup_community_area") %>%
  print()

#Trying to color each area by trip_total
revenue_pal <- colorBin("Blues",bins=4, domain =log10(areas_trip_total$trip_total), )

#Generate html labels
label_trip_total <- 
  sprintf("<strong>%s</strong><br/>$ %s",
          areas_trip_total$community_name, prettyNum(areas_trip_total$trip_total,
                                                     big.mark=",")) %>% 
  lapply(htmltools::HTML)

#Make the map
chicago_leaflet %>%
  addPolygons(data=areas_trip_total,
              fillColor=~revenue_pal(log10(areas_trip_total$trip_total)),
              color="lightgrey",
              weight=2,
              fillOpacity=0.8,
              highlightOptions = highlightOptions(
                weight=3,
                opacity=2,
                color="azure1",
                bringToFront=TRUE),
              label=label_trip_total,
              labelOptions=labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  
  addLegend("bottomright",
            pal = revenue_pal,
            values = log10(areas_trip_total$trip_total),
            title = "Trip total (in USD)",
            opacity = 0.8,
            labFormat = leaflet::labelFormat(
              transform = function(x) 10^(x)))


#Preparing relevant dataset 
pickup_dropoff_times <- taxi_trips %>%
  select(taxi_id, trip_end_timestamp, trip_start_timestamp, 
         pickup_community_area, dropoff_community_area,
         trip_total, trip_seconds) %>% #for in hourly_rate estimator
  mutate(trip_end_timestamp = as.double(mdy_hms(trip_end_timestamp)),
         pickup_community_area = as.factor(pickup_community_area),
         dropoff_community_area = as.factor(dropoff_community_area),
         trip_start_timestamp = as.double(mdy_hms(trip_start_timestamp)),
         trip_total = as.numeric(trip_total),
         trip_seconds = as.numeric(trip_seconds)) %>%
  drop_na() 
  

dropoff_times <- pickup_dropoff_times %>% 
  select(taxi_id, trip_end_timestamp, dropoff_community_area) %>%
  group_by(taxi_id) %>%
  arrange(trip_end_timestamp, .by_group=TRUE) %>%
  filter(row_number() != n()) 

pickup_times <- pickup_dropoff_times %>%
  select(taxi_id, trip_start_timestamp, pickup_community_area,
         trip_total, trip_seconds) %>% 
  group_by(taxi_id) %>%
  arrange(trip_start_timestamp, .by_group=TRUE) %>%
  filter(row_number() != 1) 
  
#Dataset used as basis for plotting bost avg time to pickup in each area and
#our expected hourly rate
dropoff_to_pickup <- pickup_times %>%
  add_column(trip_end_timestamp = dropoff_times$trip_end_timestamp, .after=1) %>%
  add_column(dropoff_community_area = dropoff_times$dropoff_community_area, .after=5) 


time_to_pickup <- dropoff_to_pickup %>%
  mutate(time_between = (trip_start_timestamp - trip_end_timestamp)/60) %>%
  filter(pickup_community_area == dropoff_community_area) %>%
  select(pickup_community_area, time_between) %>% 
  group_by(pickup_community_area) %>%
  filter(time_between < 300) %>% 
  summarise(avg_pickup_time = mean(time_between, na.rm=TRUE)) 
  
areas_pickup_time <- areas %>%
  inner_join(time_to_pickup, by="pickup_community_area") %>%
  mutate(avg_pickup_time=as.integer(avg_pickup_time)) 

time_pal <- colorBin("OrRd", domain=areas_pickup_time$avg_pickup_time, bins=c(0, 6, 12, 18, 24, 30), 
                reverse=TRUE, na.color='floralwhite')

#Generate html labels
label_avg_pickup_time <- sprintf(
  "<strong>%s</strong><br/>Average time from dropoff to new pickup:<br/>%g minutes",
  areas_pickup_time$community_name, areas_pickup_time$avg_pickup_time
) %>% lapply(htmltools::HTML)


#Labels for the legend
waiting_time_labels<-c('< 6 minutes', '6 - 12 minutes', '12 - 18 minutes', '18 - 24 minutes', 
         '> 24 minutes')
#Make the map
chicago_leaflet %>%
  addPolygons(data=areas_pickup_time,
              fillColor=~time_pal(areas_pickup_time$avg_pickup_time),
              color="lightgrey",
              weight=2,
              fillOpacity=0.8,
              highlightOptions = highlightOptions(
                weight=3,
                opacity=2,
                color="azure1",
                bringToFront=TRUE),
              label=label_avg_pickup_time,
              labelOptions=labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  
  addLegend("bottomright",
            pal = time_pal,
            labFormat = function(type, cuts, p) {  # To get custom labels
              paste0(waiting_time_labels)
            },
            values = areas_pickup_time$avg_pickup_time,
            title = "Average time between pickup and dropoff",
            opacity = 0.8)



# average trip total per trip per area / (average waiting time + average trip time for each area) 
# Use this for bootstrapping
avg_hourly_rate <- dropoff_to_pickup %>%
  mutate(waiting_time = (trip_start_timestamp - trip_end_timestamp)/60) %>%
  filter(pickup_community_area == dropoff_community_area) %>%
  filter(waiting_time < 300) %>%##remove outliers which indicate end of workday
  group_by(pickup_community_area) %>%
  summarise(avg_trip_total = mean(trip_total), 
            avg_trip_seconds = mean(trip_seconds),
            waiting_time = mean(waiting_time)) 


avg_hourly_rate <- avg_hourly_rate %>%
  mutate(hourly_rate = avg_trip_total / (waiting_time/60 + avg_trip_seconds/3600)) %>%
  select(pickup_community_area, hourly_rate) 
  

  
areas_hourly_rate <- areas %>%
  inner_join(avg_hourly_rate, by="pickup_community_area") %>%
  mutate(hourly_rate=as.integer(hourly_rate)) 


hourly_pal <- colorBin("OrRd", domain=areas_hourly_rate$hourly_rate, 
                       bins = c(0, 20, 25, 30, 35, 40, 100), na.color='tomato4')

#Generate html labels
label_avg_hourly_rate <- sprintf(
  "<strong>%s</strong><br/>Average hourly rate:<br/><b>$ %.1f</b><br/><i>95&#37; CI:</i> [<b>$ %.1f</b>, <b>$ %.1f</b>]",
  areas_hourly_rate$community_name, areas_hourly_rate$hourly_rate, 
  areas_hourly_rate$lower_bound, areas_hourly_rate$upper_bound
) %>% lapply(htmltools::HTML)

hourly_labels <- c('< 20 $', '20 - 25 $', '25 - 30 $', '30 - 25 $', '35 - 40 $', '> 40 $')



#Bootstrap for exptected hourly rate
#load("./data/chicago_maps_workspace.RData")

bootstrap_hourly_rate <- dropoff_to_pickup %>%
  mutate(waiting_time = (trip_start_timestamp - trip_end_timestamp)/60,
         trip_time = trip_seconds/60) %>%
  filter(pickup_community_area == dropoff_community_area) %>%
  filter(waiting_time < 300) %>%##remove outliers which indicate end of workday
  ungroup() %>%
  select(pickup_community_area, waiting_time, trip_total, trip_time) %>%
  group_by(pickup_community_area) %>%
  arrange(pickup_community_area) 




boot_hourly_rate <- function(data, indices) {
  d <- data[indices,] %>% #allows boot to extract sample
    group_by(pickup_community_area) %>%
    summarise(avg_trip_total = mean(trip_total), 
              avg_trip_seconds = mean(trip_time),
              waiting_time = mean(waiting_time)) %>%
    mutate(hourly_rate = 60*avg_trip_total / (waiting_time + avg_trip_seconds))
  
  d$hourly_rate
}

res <- boot(data=bootstrap_hourly_rate, statistic=boot_hourly_rate, R=100)

conf_int <- tibble(
  lower_bound = numeric(),
  upper_bound = numeric()
)

hourly_rate_boots <- as_tibble(res$t)

for (col in hourly_rate_boots) {
  len <- length(col)
  indices = as.integer(c(len*0.025 + 1, len*0.975 + 1))
  vals <- c(sort(col, partial=indices[1])[indices[1]],
            sort(col, partial=indices[2])[indices[2]])
  conf_int <- conf_int %>% add_row(lower_bound = vals[1], upper_bound = vals[2])
}

  

areas_hourly_rate <- areas_hourly_rate %>%
  arrange(pickup_community_area) %>%
  add_column(lower_bound = conf_int$lower_bound, upper_bound = conf_int$upper_bound) %>%
  mutate(hourly_rate = (lower_bound + upper_bound) / 2) 


#Make the map
chicago_leaflet %>%
  addPolygons(data=areas_hourly_rate,
              fillColor=~hourly_pal(areas_hourly_rate$hourly_rate),
              color="lightgrey",
              weight=2,
              fillOpacity=0.8,
              highlightOptions = highlightOptions(
                weight=3,
                opacity=2,
                color="azure1",
                bringToFront=TRUE),
              label=label_avg_hourly_rate,
              labelOptions=labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  
  addLegend("bottomright",
            pal = hourly_pal,
            labFormat = function(type, cuts, p) {  # To get custom labels
              paste0(hourly_labels)
            },
            values = areas_hourly_rate$hourly_rate,
            title = "Average hourly rate",
            opacity = 0.8)
