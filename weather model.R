# Need to join dailyBikes to station using the id column. Want city from the station dataset
dailyBikes <- left_join(dailyBikes, station, by=c("station_id"="id")) %>%
  select(station_id, date, bikes_available_mean, city)
head(dailyBikes)
# Match zip code to city
# Look up and match unique zip codes from weather to unique city from station
unique(weather$zip_code)
unique(station$city)
#94107 san fran, 94063 redwood, 94301 palo alto, 94041 mountain view, 95113 san jose
# Add zip code column to dailyBikes
dailyBikes <- dailyBikes %>%
  mutate(zip_code = case_when(
    city=="San Jose" ~ 95113,
    city=="Mountain View" ~ 94041,
    city=="Palo Alto" ~ 94301,
    city=="Redwood City" ~ 94063,
    city=="San Francisco" ~ 94107
  ))

head(dailyBikes)
# Force zip code to be integer
dailyBikes$zip_code <- as.integer(dailyBikes$zip_code)
# Change days in dailyBikes and weather to date format
dailyBikes$date <- as.Date(dailyBikes$date, format = "%Y/%m/%d")
head(dailyBikes)
weather$date <- as.Date(weather$date, format = "%m/%d/%Y")
head(weather)

#How many unique day and zipcode in the weather?
nrow(distinct(weather[,c('zip_code', 'date')]))

#How many unique day and station_id in the dailyBikes?
nrow(distinct(dailyBikes[,c('station_id', 'date')]))

# Join dailyBikes to weather on zip_code and day
dailyBikes <- merge(dailyBikes, weather, by=c("zip_code", "date"))
head(dailyBikes)
# Maybe Finally ready to model! - think about max gust missing and event
