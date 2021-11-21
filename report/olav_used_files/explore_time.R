library(tidyverse)
library(ggplot2)
library(gganimate)
library(janitor)
library(lubridate)
library(jsonlite)


#Get workspace: 
load("./olav_used_files/data/companies_time_workspace.RData")

check <- monthly_revenue_area %>%
  group_by(year, month) %>%
  summarise(revenue = sum(revenue, na.rm = TRUE)) %>%
  View()



#Soda API query - must be on single line
QUERY_BASE <- "https://data.cityofchicago.org/resource/wrvz-psew.json?"
query_select <- 
  "$select=%20date_extract_y(trip_start_timestamp)%20as%20year,%20date_extract_m(trip_start_timestamp)%20as%20month,%20company,sum(trip_total)%20as%20revenue,count(*)%20as%20activity%20"
query_where <- "&$where=year=2013%20"
query_group <- "&$group=year,month,company"
#Note: Always have to specify group of the variables which are selected, except aggregations


query <- paste0(QUERY_BASE, query_select, query_where, query_group)

#Read json files
monthly_revenue_company <- jsonlite::fromJSON(query) 

for(year in 2014:2021) {
  temp <- jsonlite::fromJSON(paste0(QUERY_BASE, query_select, "&$where=year=", year, "%20", query_group))
  monthly_revenue_company <- rbind(monthly_revenue_company, temp)
}

#Format table
monthly_revenue_company <- monthly_revenue_company %>%
  as_tibble() %>% 
  drop_na() %>%
  mutate(year = as.integer(year), month = as.integer(month),
         revenue = as.integer(revenue), activity = as.integer(activity),
         company = sub(".*-", "", company),
         company = sub("\\(.*", "", company),
         company = 
         season = case_when(
           month %% 12 <= 2 ~ "Winter",
           month <= 5 ~ "Spring",
           month <= 8 ~ "Summer",
           month <= 11 ~ "Autumn"),
         date = ymd(paste0(year, "-", month, "-", "1"))
  ) 

#Query  monthly revenue and activity by area -----------------------------------
query_select_monthly_revenue_area <- 
  "$select=%20date_extract_y(trip_start_timestamp)%20as%20year,%20date_extract_m(trip_start_timestamp)%20as%20month,%20pickup_community_area,sum(trip_total)%20as%20revenue,count(*)%20as%20activity%20"

query_where_monthly_revenue_area <- "&$where=year=2013%20"
query_group_monthly_revenue_area <- "&$group=year,month,pickup_community_area"

query_monthly_revenue_area <- paste0(QUERY_BASE,
  query_select_monthly_revenue_area,
  query_where_monthly_revenue_area,
  query_group_monthly_revenue_area
)

#Read json query for monthly revenue and activity in each area
monthly_revenue_area <- jsonlite::fromJSON(query_monthly_revenue_area) 

for(year in 2014:2021) {
  temp <- jsonlite::fromJSON(paste0(QUERY_BASE, 
                                    query_select_monthly_revenue_area,
                                    "&$where=year=", year, "%20", 
                                    query_group_monthly_revenue_area))
  monthly_revenue_area <- rbind(monthly_revenue_area, temp)
}

#Format table
monthly_revenue_area <- monthly_revenue_area %>%
  as_tibble() %>% 
  drop_na() %>%
  mutate(year = as.integer(year), month = as.integer(month),
         revenue = as.integer(revenue), activity = as.integer(activity),
         date = ymd(paste0(year, "-", month, "-", "1")),
         pickup_community_area = as.integer(pickup_community_area)
  ) 


theme_set(theme_bw())

#-------------------------------------------------------------------------------

#Plotting activity (num trips) per year for each season

taxi_trips_seasons_and_days <- monthly_revenue_company %>%
  group_by(season, date) %>%
  summarise(activity = sum(activity)) %>%
  filter(date <= as.Date("2021-09-01"))
  
  
yearly_activity_season <- ggplot(
  data = taxi_trips_seasons_and_days,
  mapping = aes(x = date, y = activity, group = season, color=factor(season))) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Year", y = "Number of trips (in Million)", color = "Season:") + 
  scale_y_continuous(limits = c(0, 3200000), breaks = c(0, 1000000, 2000000, 3000000), labels = c("0", "1", "2", "3")) +
  theme(legend.position = "top",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(face = "bold", angle = -45, size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.line = element_line(colour = "dark grey", size = 1, linetype = "solid"),
        legend.text = element_text()
        )
  
show(yearly_activity_season)

yearly_activity_season + 
  geom_point() + 
  transition_reveal(date) 
  
anim_save(filename = "revenue_season.gif", width = 1800, height = 900)
#-------------------------------------------------------------------------------

#This can be used to visualize each district (0-9) compared to Population size and income, with taxi trips as 
#measure of how big the ball is

library(janitor)

#Problem: timestamp??

population_area <- read_csv("./yiren/data/mywiki.csv") %>%
  as_tibble() %>%
  clean_names() %>%
  select(no, population) %>%
  mutate(no = as.integer(no), population = as.integer(population)) %>%
  View()

income_area <- read_csv("./yiren/data/Per_Capita_Income.csv") %>%
  as_tibble() %>%
  clean_names() %>%
  select(community_area_number, per_capita_income) %>%
  mutate(community_area_number = as.integer(community_area_number),
         per_capita_income = as.integer(per_capita_income)) %>%
  View()


monthly_revenue_area <- monthly_revenue_area %>%
  group_by(date, pickup_community_area) %>%
  summarise(size = sum(activity), #where size = number of trips/month
            trips = n_distinct(trip_id), #here i want population size
            revenue = sum(trip_total, na.rm = TRUE)) %>% #here i want population income 
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

number_of_companies <- monthly_revenue_company%>%
  select(date, company) %>%
  group_by(date) %>%
  unique() %>%
  summarise(companies = n()) %>%
  print()

number_of_companies_plot <- ggplot(
  data = number_of_companies,
  mapping = aes(x = date, y = companies)) +
  geom_line() +
  scale_x_date(limits = as.Date(c("2013-01-01", "2021-09-01"))) +
  labs(x = "Year", y = "Number of companies") 


show(number_of_companies_plot)

ggsave(file = "number_of_companies.png", dpi = 75, width = 5, height = 3)

revenue_per_company <- monthly_revenue_company %>%
  group_by(company) %>%
  summarise(revenue = sum(revenue)) %>%
  arrange(desc(revenue)) 

other_companies <- revenue_per_company[-(1:10),] %>% 
  summarise(revenue = sum(revenue)) %>% print()

top10companies <- revenue_per_company %>%
  head(10) %>% 
  add_row(company = "Other", revenue = pull(other_companies[1, 1])) %>%
  mutate(share = revenue / sum(revenue),
         company = paste0(company, ", ", as.character(scales::percent(round(share,2))))) %>%
  print()
  

pie_chart <- top10companies %>%
  ggplot(mapping = aes(x = "", y = share, fill = reorder(company, -share))) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  scale_color_viridis_d() +
  labs(x = "", y = "") +
  scale_fill_discrete(name = "Company, Market share") +
  theme(axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        axis.text.x = element_blank()) %>%
  show()

ggsave(file = "company_market_share.png", dpi = 75, width = 7, height = 6)

#---------------#---------------#---------------#---------------#---------------
#Bar chart race of company revenue per month DONE

company_rank <- monthly_revenue_company %>%
  drop_na() %>%
  group_by(year, month) %>%
  mutate(rank = rank(-revenue),
         revenue_lbl = paste0(" ", round(revenue / 1e3))) %>% #made to animate year
  ungroup() %>%
  group_by(date) %>%
  filter(rank<=7) %>%
  print()


revenue_per_company <- ggplot(
  data = company_rank,
  mapping = aes(rank, group = company, 
                color = as.factor(company), fill = as.factor(company))
  ) +
  geom_tile(mapping = aes(y = revenue/2, height = revenue), show.legend = FALSE, alpha = 0.7) + 
  geom_text(aes(y = 0, label = paste(company, " ")), vjust = 0.2, hjust = 1, size = 8) +
  geom_text(aes(y = revenue, label = revenue_lbl, hjust=0), size = 6) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  scale_color_viridis_d() + 
  #scale_size(range = c(2, 12)) + 
  labs(x = "Revenue", y = "Company") +
  guides(color = "none", fill = "none") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=40, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=25, hjust=0.5, face="italic", color="grey"),
        plot.caption = element_text(size=25, hjust=0.5, vjust=1, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 13, "cm")) #top, rigth, bottom, left



anim <- revenue_per_company + transition_states(date, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE) +
  labs(title = 'Total revenue in month {closest_state}',
       subtitle  =  "Top 7 Earning Companies",
       caption  = "Total Revenue in Thousand USD")

animate(anim, 1000, fps = 20,  width = 1200, height = 1000,
        renderer = gifski_renderer("company_revenue.gif"))

