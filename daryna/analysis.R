library(dplyr)
library(car)
library(ggplot2)

taxi_trips = read.csv("taxi_trips.csv")

taxi_trips <- taxi_trips[!is.na(taxi_trips$Pickup.Community.Area), ]

# The city of Chicago is made up of 77 community areas and can be grouped into 9 districts
# We will do that in the next steps of our analysis.

typeof(taxi_trips$Pickup.Community.Area)
# [1] "integer"

typeof(taxi_trips$Dropoff.Community.Area)
# [1] "integer"

# Tranfrom Areas into categorical factors
taxi_trips$Pickup.Community.Area <- as.factor(taxi_trips$Pickup.Community.Area)
taxi_trips$Dropoff.Community.Area <- as.factor(taxi_trips$Dropoff.Community.Area)


## new factor group (copy of species)
taxi_trips[,"pickup"] <- taxi_trips[,"Pickup.Community.Area"]

# Rename the levels 
levels(taxi_trips$pickup) <- c("Rogers Park", "West Ridge", "Uptown", "Lincoln Square", "North Center", 
                                                "Lake View", "Lincoln Park", "Near North Side", "Edison Park", "Norwood Park",
                                                "Jefferson Park", "Forest Glen", "North Park", "Albany Park", "Portage Park", 
                                                "Irving Park", "Dunning", "Montclare", "Belmont Cragin", "Hermosa", 
                                                "Avondale", "Logan Square", "Humboldt Park", "West Town", "Austin",
                                                "West Garfield Park", "East Garfield Park", "Near West Side", "North Lawndale", "South Lawndale",
                                                "Lower West Side", "The Loop", "Near South Side", "Armour Square","Douglas",
                                                "Oakland", "Fuller Park", "Grand Boulevard", "Kenwood", "Washington Park",
                                                "Hyde Park", "Woodlawn", "South Shore","Chatham", "Avalon Park",
                                                "South Chicago", "Burnside", "Calumet Heights" ,"Roseland", "Pullman",
                                                "South Deering", "East Side", "West Pullman", "Riverdale", "Hegewisch",
                                                "Garfield Ridge", "Archer Heights", "Brighton Park", "McKinley Park", "Bridgeport",
                                                "New City","West Elsdon","Gage Park", "Clearing", "West Lawn",
                                                "Chicago Lawn", "West Englewood", "Englewood", "Greater Grand Crossing", "Ashburn",
                                                "Auburn Gresham","Beverly","Washington Heights", "Mount Greenwood", "Morgan Park",
                                                "O'Hare","Edgewater") 

FarNorthSide.set <- c("Rogers Park", "West Ridge", "Uptown", "Lincoln Square", "Edison Park", 
                      "Norwood Park", "Jefferson Park", "Forest Glen", "North Park", "Albany Park",
                      "O'Hare", "Edgewater")

NorthSide.set <- c("North Center", "Lake View", "Lincoln Park","Avondale","Logan Square")

NorthWestSide.set <- c("Portage Park", "Irving Park", "Dunning", "Montclare", "Belmont Cragin", "Hermosa")

Central.set <- c("Near North Side", "The Loop", "Near South Side")

West.set <- c("Humboldt Park", "West Town", "Austin" , "West Garfield Park", "East Garfield Park", 
              "Near West Side" , "North Lawndale", "South Lawndale", "Lower West Side")

Soutwest.set <- c("Garfield Ridge", "Archer Heights", "Brighton Park", "McKinley Park", "New City",
                  "West Elsdon", "Gage Park", "Clearing", "West Lawn","Chicago Lawn" , 
                  "West Englewood", "Englewood")


South.set <- c("Armour Square", "Douglas", "Oakland","Fuller Park", "Grand Boulevard",
               "Kenwood", "Washington Park", "Hyde Park" ,"Woodlawn", "South Shore",
               "Bridgeport", "Greater Grand Crossing")


FarSouthwest.set <- c("Ashburn", "Auburn Gresham", "Beverly", "Washington Heights", "Mount Greenwood",
                  "Morgan Park")


FarSouth.set <- c("Chatham", "Avalon Park", "South Chicago", "Burnside", "Calumet Heights",
              "Roseland", "Pullman", "South Deering", "East Side", "West Pullman",
              "Riverdale", "Hegewisch")



taxi_trips$district <- recode(taxi_trips$pickup, "FarNorthSide.set = 'Far North Side'; NorthSide.set = 'North Side';NorthWestSide.set = 'Northwest Side';
                              Central.set = 'Central, Near North, and Near South Side'; West.set = 'West and Near West Side'; Soutwest.set = 'Southwest Side';
                              South.set = 'South Side'; FarSouthwest.set = 'Far Southwest Side'; FarSouth.set =  'Far South Side' ; else=NA")



levels(taxi_trips$district)
sum(is.na(taxi_trips$district)) # recoded all the values

# Time series of frequencies over time
library(plotly)
library(tidyverse)
library(tidyquant)
library(ggplot2)
library(hrbrthemes)
library(scales)

# Get a frequency table
frequency_time_series <- as.data.frame(table(taxi_trips$days))

# Rename column name 
frequency_time_series <- frequency_time_series %>% 
  rename(
    Day = Var1,
    Frequency = Freq
  )

# As Time data type
frequency_time_series$Day <- as.Date(frequency_time_series$Day)



# Too much data for days , we can get a smoother function if we do by month_year
taxi_trips$Month_Yr <- format(as.Date(taxi_trips$days), "%Y-%m")




# Per District
# Multiple Time series on one plot
library(zoo)
# DataFrame to get frequency by area
by_district <- as.data.frame(table(as.Date(taxi_trips$days), as.factor(taxi_trips$district)))

# Rename column name 
by_district <- by_district %>% 
  rename(
    Day = Var1,
    District = Var2,
    Frequency = Freq
  )

# As Time data type
by_district$Day <- as.Date(by_district$Day)

ggplot(by_district, aes(x = Day, y = Frequency, color = District, group = District)) +
  geom_point()  +   geom_smooth()+ theme_test()



# Too much data for days , we can get a smoother function if we do by month_year
taxi_trips$Month_Yr <- format(as.Date(taxi_trips$days), "%Y-%m")



by_month_district <- as.data.frame(table(taxi_trips$Month_Yr,  taxi_trips$district))



by_month_district <- by_month_district %>% 
  rename(
    Day = Var1,
    District = Var2,
    Frequency = Freq
  )



by_month_district$Day <- as.Date(as.yearmon(by_month_district$Day))


p <- ggplot(by_month_district, aes(x = Day, y = Frequency, color = District, group = District)) + scale_x_date(breaks = "1 year", minor_breaks = "1 month") +
  geom_line()  +  geom_smooth()+ theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 0.1, hjust=0.5)) 
  

ggplotly(p)

### Get a TOTAL AGGREGATED VALUE PER DAY
all_districts <- aggregate(x = by_month_district[c("Frequency")],
                     FUN = sum,
                     by = list(Day = by_month_district$Day))

all_districts['District'] = 'All Districts'
all_districts <- all_districts[, c(1, 3, 2)]

all_districts$Day <- as.Date(as.yearmon(all_districts$Day))

by_month_district_1 <- rbind(by_month_district,all_districts)


p <- ggplot(by_month_district_1, aes(x = Day, y = Frequency, color = District, group = District)) + scale_x_date(breaks = "1 year", minor_breaks = "1 month") +
  geom_line()  +  geom_smooth()+ theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 0.1, hjust=0.5)) 


ggplotly(p)

### By Area

### Estimate time series components : Trend and seasonal components
taxi_trips$Month_Yr <- format(as.Date(taxi_trips$days), "%Y-%m")
freq <- as.data.frame(table(taxi_trips$Month_Yr))

freq <- freq[order(freq$Var1),] 

dim(freq) # length of 105

number_of_taxis <- freq['Freq']

number_of_taxis=ts(number_of_taxis, start=c(2013,1), end=c(2021,9), frequency=12)

plot(number_of_taxis, xlab="year", ylab="number of taxis")
number_of_taxis.smooth=smooth.spline(number_of_taxis, df= 105) # ever satisfactory for seasonalities?
lines(number_of_taxis.smooth, col="red")

#variable 1, time
time <- 1:105
time

  #variable 2, dummy variables for seasons
season <- model.matrix(~-1+factor(cycle(number_of_taxis)))[,-1]#cycle := month as levels
dimnames(season)[[2]] <- c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "AUg", "Sep", "Oct", "Nov", "Dec")

head(season, n=14)


# model estimation

number_of_taxis.lm <- lm(number_of_taxis~time+season)
summary(number_of_taxis.lm) # why unsignificant level?

# with trend squared
number_of_taxis_sq.lm <- lm(number_of_taxis~time+ I(time^2) +season)
summary(number_of_taxis_sq.lm) # why unsignificant level?
plot(number_of_taxis_sq.lm)

# Residual analysis
residuals <- resid(number_of_taxis_sq.lm)
length(residuals)
residuals <- ts(residuals, start=c(2013,1), end=c(2021,9), frequency=12)

plot(number_of_taxis, xlab="year", ylab="number of taxis")
lines(number_of_taxis-residuals, col="red")

plot(residuals) 


## With trend in cube
number_of_taxis_cube.lm <- lm(number_of_taxis~time+ I(time^2) + I(time^3)+ season)
summary(number_of_taxis_cube.lm) # why unsignificant level?
# Remove the trend by Differencing:
######################################

diff12.taxi<-diff(number_of_taxis,12)

length(diff12.taxi)


par(mfrow=c(2,1))
plot(residuals)
plot(diff12.taxi)


## 
par(mfrow=c(2,2))
acf(residuals)
pacf(residuals)

acf(diff12.taxi)
pacf(diff12.taxi)

library("tseries")
library("stats")

# stochastic part analysis

mean(residuals)
plot(residuals,type="l")



par(mfrow=c(2,1))
acf(residuals, lag.max=80)
acf(residuals, type="partial", lag.max=80)
par(mfrow=c(1,1))


taxi.arima100 <- arima(x=residuals,order=c(1,0,0), n.cond=3)
taxi.arima100

tsdiag(taxi.arima100)
acf(taxi.arima100$residuals, type="partial")
# log likelihood = -605.22,  aic = 1216.45

# Best model AR(2)
taxi.arima200 <- arima(x=residuals,order=c(2,0,0), n.cond=3)
taxi.arima200
# log likelihood = -604.02,  aic = 1216.04

tsdiag(taxi.arima200)
acf(taxi.arima100$residuals, type="partial")


taxi.arima001 <- arima(x=residuals,order=c(0,0,1), n.cond=3)
taxi.arima001
# log likelihood = -638.16,  aic = 1282.32

tsdiag(taxi.arima001)
acf(taxi.arima001$residuals, type="partial")



taxi.arima002 <- arima(x=residuals,order=c(0,0,2), n.cond=3)
taxi.arima002
# log likelihood = -623.1,  aic = 1254.2

tsdiag(taxi.arima002)
acf(taxi.arima002$residuals, type="partial")



taxi.arima101 <- arima(x=residuals,order=c(1,0,1), n.cond=3)
taxi.arima101
# log likelihood = -603.72,  aic = 1219.44

tsdiag(taxi.arima101)
acf(taxi.arima101$residuals, type="partial")
# log likelihood = -604.21,  aic = 1216.42


taxi.arima202 <- arima(x=residuals,order=c(2,0,2), n.cond=3)
taxi.arima202
# log likelihood = -603.72,  aic = 1219.44

tsdiag(taxi.arima202)
acf(taxi.arima202$residuals, type="partial")

### Heatmap to represent seasonality
month_year <- as.data.frame(table(taxi_trips$year,taxi_trips$month))

# Rename column name 
month_year <- month_year %>% 
  rename(
    Year = Var1,
    Month = Var2,
    Frequency = Freq
  )



month_year$Month <- as.factor(as.character(month_year$Month)) 

month_year$Month <- ordered(month_year$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))


p <- ggplot(month_year, aes(x=Year, y=Month)) + scale_y_discrete(limits = rev(levels(month_year$Month))) + geom_tile(aes(fill=Frequency)) + scale_fill_gradient(low="white", high="blue") 
ggplotly(p)










