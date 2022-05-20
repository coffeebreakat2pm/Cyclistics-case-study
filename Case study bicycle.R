#==========================================================================
# Pre-work: Installation of required packages & change of directory for ease
#==========================================================================
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data

getwd() #display the working directory
setwd("C:/Users/Salt&Pepper/Documents") #sets our working directory to simplify calls for data



# 1. Importing the data

#upload the data 
mar2022 <- read.csv("202203-divvy-tripdata.csv")
feb2022 <- read.csv("202202-divvy-tripdata.csv")
jan2022 <- read.csv("202201-divvy-tripdata.csv")
dec2021 <- read.csv("202112-divvy-tripdata.csv")
nov2021 <- read.csv("202111-divvy-tripdata.csv")
oct2021 <- read.csv("202110-divvy-tripdata.csv")
sep2021 <- read.csv("202109-divvy-tripdata.csv")
aug2021 <- read.csv("202108-divvy-tripdata.csv")
jul2021 <- read.csv("202107-divvy-tripdata.csv")
jun2021 <- read.csv("202106-divvy-tripdata.csv")
may2021 <- read.csv("202105-divvy-tripdata.csv")
apr2021 <- read.csv("202104-divvy-tripdata.csv")



# 2. Check for empty cells 

# We can use something like following code to check whether a cell is empty or not

#empty_cells <- 0
#variable_name <- file$column
#  for(index in variable _name){
#     if(index == ""){
#        empty_cells <- empty_cells +1
#         }
#   }


# This is give us that columns start_station_name, start_station_id, end_station_name, end_station_id contains empty cells.
# We can remove these columns since we can get similar information with longtitude and lattitude coordinates.

mar2022 <- (mar2022 %>%
              select(-c(start_station_name, end_station_name,  start_station_id, end_station_id)))
feb2022 <- (feb2022 %>%
              select(-c(start_station_name, end_station_name,  start_station_id, end_station_id)))
jan2022 <- (jan2022 %>%
              select(-c(start_station_name, end_station_name,  start_station_id, end_station_id)))
dec2021 <- (dec2021 %>%
              select(-c(start_station_name, end_station_name,  start_station_id, end_station_id)))
nov2021 <- (nov2021 %>%
              select(-c(start_station_name, end_station_name,  start_station_id, end_station_id)))
oct2021 <- (oct2021 %>%
              select(-c(start_station_name, end_station_name,  start_station_id, end_station_id)))
sep2021 <- (sep2021 %>%
              select(-c(start_station_name, end_station_name,  start_station_id, end_station_id)))
aug2021 <- (aug2021 %>%
              select(-c(start_station_name, end_station_name,  start_station_id, end_station_id)))
jul2021 <- (jul2021 %>%
              select(-c(start_station_name, end_station_name,  start_station_id, end_station_id)))
jun2021 <- (jun2021 %>%
             select(-c(start_station_name, end_station_name,  start_station_id, end_station_id)))
may2021 <- (may2021 %>%
              select(-c(start_station_name, end_station_name,  start_station_id, end_station_id)))
apr2021 <- (apr2021 %>%
              select(-c(start_station_name, end_station_name,  start_station_id, end_station_id)))

# We will now bind all dataframes into one, use str() function before to make sure ride_id chr so it stacks correctly

all_trips <- bind_rows(mar2022, feb2022, jan2022, dec2021, nov2021, oct2021, sep2021, aug2021, jul2021, jun2021, may2021, apr2021)


# Inspect the new table that has been created

colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# Inspect that it only contains correct customer type and bike type
table(all_trips$member_casual) #results in either "member" or "casual"
table(all_trips$rideable_type) #results in either "classic_bike", "docked_bike", or "electric_bike"


# We also want to additional columns of data, such as dates (day, month, year etc) as well as trip duration
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# adding trip length/duration

all_trips$ended_at <- as.POSIXct(all_trips$ended_at, tz = 'UTC')
all_trips$started_at <- as.POSIXct(all_trips$started_at, tz = 'UTC')
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)


# the company has been moving away from "docked bike" and is slowing taking those out of circulation, we want to remove those from our dataset
all_trips_v2 <- all_trips[!(all_trips$rideable_type == "docked_bike" | all_trips$ride_length<0),]


# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# The days of the week will be out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides,  fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = comma_format(big.mark = ".",
                                           decimal.mark = ",")) +
  ggtitle("Cyclistics bike-sharing Apr 2021 - Mar 2022", subtitle = "number of rides by weekday and user type") + 
  xlab("Weekday") + ylab("Number of rides") +
  scale_fill_manual(values=c("#4285F4",
                             "#DB4437"))


# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) + 
  geom_col(position = "dodge") + scale_y_continuous() +
  ggtitle("Cyclistics bike-sharing Apr 2021 - Mar 2022", subtitle = "average ride duration (seconds) by weekday and user type") + 
  xlab("Weekday") + ylab("Average duration (in seconds") +
  scale_fill_manual(values=c("#4285F4",
                             "#DB4437"))

# Finally, create a visualization for distrubution between standard and electric bike among the customers

all_trips_v2 %>%
  group_by(member_casual, rideable_type) %>%
  summarise(number_of_rides = n()) %>% 
  ggplot(aes(x = member_casual, y = number_of_rides)) + 
  geom_col(aes(fill = rideable_type), width = 0.5) + 
  ylab("Number of rides") + xlab("User type") +
  scale_y_continuous(labels = comma_format(big.mark = ".",
                                           decimal.mark = ",")) +
  ggtitle("Cyclistics bike-sharing Apr 2021 - Mar 2022", subtitle = "number of rides by bike and user type") + 
  scale_fill_manual(values=c("#4285F4",
                             "#DB4437"))