
getwd()
setwd("/Users/josephreynolds/Documents/Analytics - NU CPS/6040/Final Project/Dataset")
station <- read.csv("station.csv", header = TRUE, sep = ",")
trip <- read.csv("trip.csv", header = TRUE, sep = ",")
weather <- read.csv("weather.csv", header = TRUE, sep = ",")
statusfiltered <- read.csv("statusfiltered.csv", header = TRUE, sep = ",")


library(ggplot2)
library(ggmap)
library(sp)
library(leaflet)
library(skimr)
library(dplyr)
library(gt)


## EXPLORE STATION DF
## ID variable is pretty straightforward
## Lat/Long - each station has coordinates

skim(station)

#Dock count per station histogram 

summary(station$dock_count)
range(station$dock_count)
hist(station$dock_count, xlim= c(10,30), cex.axis = .7, xlab = "Docks",
     main = "Number of Docks per Station", ylab = "", breaks = 20, col = "lightblue")


#@ Station size (amount of docks) frequency distro

#this was close, but to get decending, have to save the table before plot() can sort()
#barplot(table(station$dock_count), horiz = TRUE, col = "lightblue", cex.axis = .7, xlim= c(0,35), las=2,
#        xaxt='n', main = "Number of Docks Per Station")


dock_table <- table(station$dock_count)
barplot(sort(dock_table), horiz = TRUE, col = "lightblue", cex.axis = .7, xlim= c(0,35), las=2, font = 2,
             xaxt='n', main = "Station Size Frequency Distribution")
axis(1, at=seq(0, 35, by=5), las =1, cex.axis = .7)

docks_per <- as.data.frame(sort(table(station$dock_count), decreasing = TRUE))
colnames(docks_per) <- c("Number of Docks per Station", "Number of Stations")
gt(docks_per) %>% 
  gt_theme_538() %>% 
  tab_header(title = md("Station Size Distribution"))

##Installation date
summary(station$installation_date)
range(station$installation_date)
unique(station$installation_date, showNA = "always")
length(station$installation_date)

## table of installation dates, looks to be that most went in summer 2013, few next spring
table(sapply(station$installation_date, unique))
sum(table(sapply(station$installation_date, unique)))

table(sapply(station$installation_date, unique))
sort(table(sapply(station$installation_date, unique)), decreasing = TRUE, method = "date")
installation_schedule <- arrange(as.data.frame(table(sapply(station$installation_date, 
                                                            unique))))
colnames(installation_schedule) <- c("Date", "# Stations")
installation_schedule
class(installation_schedule$Date)

# Change class of date column
installation_schedule$Date <- as.character(installation_schedule$Date)
installation_schedule$Date <- as.Date(installation_schedule$Date, "%m/%d/%Y")
class(installation_schedule$Date)
installation_schedule
installation_schedule <- installation_schedule[order(installation_schedule$Date), ]
installation_schedule
gt(installation_schedule)
gt(installation_schedule) %>% 
  gt_theme_538() %>% 
  tab_header(title = md("Bay Area Bikeshare Station Installations"))

## barplot of stations by city

unique(station$city)
Redwood_City <- filter(station, city == "Redwood City")
Redwood_City
Mountain_View <- filter(station, city == "Mountain View")
Mountain_View
San__Jose <- filter(station, city == "San Jose")
San__Jose
Palo_Alto <- filter(station, city == "Palo Alto")
Palo_Alto
San_Francisco <- filter(station, city == "San Francisco")
San_Francisco

nrow(Redwood_City)/nrow(station)
nrow(Mountain_View)/nrow(station)
nrow(San__Jose)/nrow(station)
nrow(Palo_Alto)/nrow(station)
nrow(San_Francisco)/nrow(station)

by_city <- rbind(Palo_Alto, Redwood_City, Mountain_View, San_Francisco, San__Jose)
class(by_city$city)
barplot(sort(table(by_city$city)), las =2, cex.axis = .7, col= "lightblue", cex.names = .7,
        main ="Where are the stations?")


## PLOT THE STATIONS ON A MAP SO WE CAN SEE WHAT WE'RE DEALING WITH

df <- data.frame(longitude = station["long"], 
                 latitude = station["lat"])
df

coordinates(df) <- ~long+lat
leaflet(df) %>% addMarkers() %>% addTiles()



############# Weather  #############
# Fog is iconic for SF, so we will take a quick look at it for fun
# Good thing that we dug in a bit because we learned that one of the weather variables under "events" is
# pretty strange! 
View(trip)
View(weather)
str(weather)
unique(weather$zip_code)
weather_events <-sort(table(weather$events))
weather_events
hist(weather_events)
# wonder what this blank variable is

head(weather$events)

# merge "rain" and "Rain" , the two types of Fog, and the t.storms variables

Rain <- filter(weather, events == "rain" | events == "Rain")
Rain$events <- "Rain"
table(Rain$events)
t.storms <- filter(weather, events == "Rain-Thunderstorm")
t.storms$events <- "t.storms"
Fog<- filter(weather, events == "Fog-Rain" | events == "Fog")
Fog$events <- "Fog"
blank_w_event <- filter(weather, events == "")
blank_w_event$events <- "Blank"

# change class of "blank" so we can see it better

table(unique(blank_w_event$date))
blank_w_event$date <- as.character(installation_schedule$date)
blank_w_event$date <- as.Date(blank_w_event$date, "%m/%d/%Y")
class(blank_w_event$date)
range(blank_w_event)

blanks_2013 <- filter(blank_w_event, grepl("2013", blank_w_event$date) == TRUE)
head(blanks_2013)
blanks_2014 <- filter(blank_w_event, grepl("2014", blank_w_event$date) == TRUE)
blanks_2015 <- filter(blank_w_event, grepl("2015", blank_w_event$date) == TRUE)
blanks_clean <- rbind(blanks_2013, blanks_2014, blanks_2015)
blanks_clean <- data.frame("2013" = nrow(blanks_2013), "2014" = nrow(blanks_2014), "2015" = nrow(blanks_2015))
colnames(blanks_clean) <- c("2013", "2014", "2015")

gt(blanks_clean) %>% 
  gt_theme_538() %>% 
  tab_header(title = md("'Blank' Weather Events"))

## We still need to figure out what this "blank" variable actually means, and the dictionary doesn't
#give us much to go on

weather_events_clean_df <- rbind(Rain, t.storms, Fog, blank_w_event)
weather_events_clean_df
class(weather_events_clean_df$events)
unique(weather_events_clean_df$events)
barplot(sort(table(weather_events_clean_df$events)), las =2, cex.axis = .6, col= "lightblue", cex.names = .7,
        main ="Weather Event Counts", xlab = "   what is Blank? ^ ")





###### code has been cited in the EDA document, this is a theme for gt()

gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "left",
      ...
    ) 
}
