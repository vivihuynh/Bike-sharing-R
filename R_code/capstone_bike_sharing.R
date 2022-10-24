#Get all required packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot")
library(tidyverse)
library(lubridate)
library(ggplot2)

#Set working directory to where cvs files are
setwd("/Users/vivi/Documents/google_cert/Divvy_case_study/CSV")

#Upload 12 months of divvy trip data
aug_2021 <- read_csv("202108-divvy-tripdata.csv")
sep_2021 <- read_csv("202109-divvy-tripdata.csv")
oct_2021 <- read_csv("202110-divvy-tripdata.csv")
nov_2021 <- read_csv("202111-divvy-tripdata.csv")
dec_2021 <- read_csv("202112-divvy-tripdata.csv")
jan_2022 <- read_csv("202201-divvy-tripdata.csv")
feb_2022 <- read_csv("202202-divvy-tripdata.csv")
mar_2022 <- read_csv("202203-divvy-tripdata.csv")
apr_2022 <- read_csv("202204-divvy-tripdata.csv")
may_2022 <- read_csv("202205-divvy-tripdata.csv")
jun_2022 <- read_csv("202206-divvy-tripdata.csv")
jul_2022 <- read_csv("202207-divvy-tripdata.csv")

#Check for column names to ensure consistency
colnames(aug_2021)
colnames(sep_2021)
colnames(oct_2021)
colnames(nov_2021)
colnames(dec_2021)
colnames(jan_2022)
colnames(feb_2022)
colnames(mar_2022)
colnames(apr_2022)
colnames(may_2022)
colnames(jun_2022)
colnames(jul_2022)

#Column names are consistent, proceed to inspecting the data frames before merging
str(aug_2021)
str(sep_2021)
str(oct_2021)
str(nov_2021)
str(dec_2021)
str(jan_2022)
str(feb_2022)
str(mar_2022)
str(apr_2022)
str(may_2022)
str(jun_2022)
str(jul_2022)
#Combine individual monthly data frames into one
total_trips <- bind_rows(aug_2021,sep_2021,oct_2021,nov_2021,dec_2021,jan_2022,feb_2022,mar_2022,apr_2022,may_2022,jun_2022,jul_2022)

#Inspect newly combined data frame
colnames(total_trips)
nrow(total_trips)
dim(total_trips)
summary(total_trips)
str(total_trips)

#Clean data
table(total_trips$member_casual) 
#ensure that there're only two categories
total_trips$hour <- format(as.POSIXct(total_trips$started_at), format = "%H")
#give hour of the day
total_trips$date <- as.Date(total_trips$started_at) 
#format as yyyy-mm-dd
total_trips$year <- format(as.Date(total_trips$date), "%Y") 
#give 4 digit year
total_trips$month <- format(as.Date(total_trips$date), "%m") 
#give month 0-12
total_trips$day <- format(as.Date(total_trips$date), "%d") 
#give day 0-31
total_trips$day_of_week <- format(as.Date(total_trips$date), "%A") 
#give unabbreviated day of the week
#Calculate duration of ride in second 
total_trips$ride_length <- difftime(total_trips$ended_at,total_trips$started_at)
total_trips$ride_length <- as.numeric(as.character(total_trips$ride_length))
is.numeric(total_trips$ride_length)
#Ensure that it's a numeric for calculation
#Remove all rows where duration of ride is negative & create a new cleaned data frame 
#total_trips_v2 <- total_trips[!(total_trips$ride_length<0),]
total_trips <- distinct(total_trips) #remove duplicate rows 
total_trips <- total_trips %>%  #remove columns not needed: ride_id, start_station_id, end_station_id, start_lat, start_long, end_lat, end_lng
  select(-c(ride_id, start_station_id, end_station_id,start_lat,start_lng,end_lat,end_lng)) %>%
  filter(
    start_station_name != "",
    end_station_name != "",
    ride_length < 86400,
    ride_length > 60)
#Done cleaning

#Quick Analysis
#Descriptive analysis of ride length
summary(total_trips$ride_length)
#Comparing ride length between casual and members
aggregate(total_trips$ride_length ~ total_trips$member_casual, FUN = min)
aggregate(total_trips$ride_length ~ total_trips$member_casual, FUN = max)
aggregate(total_trips$ride_length ~ total_trips$member_casual, FUN = mean)
aggregate(total_trips$ride_length ~ total_trips$member_casual, FUN = median)
#Notice for ride length, casual > members
#Comparing the average ride length by day between casual and members
total_trips$day_of_week <- ordered(total_trips$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(total_trips$ride_length ~ total_trips$member_casual + total_trips$day_of_week, FUN = mean)

#Visualize and compare the number of rides between member vs casual by day
total_trips %>%
    mutate(weekday = wday(started_at, label = TRUE)) %>%    #creates weekday field
    group_by(member_casual,weekday) %>%  
    summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
    arrange(member_casual,weekday) %>%  #arrange in desc order by rider type and weekday
    ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge") + labs(title = "how does casual users and members riding habit differs throughout the week?", subtitle = "Data spans over 12 months", y = "Number of rides", x = "Day of the week", fill = "Rider type")
#visualize with bar graph
#Visualize and compare the ride length between member vs casual by day
total_trips %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%    #
  group_by(member_casual,weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual,weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) + geom_col(position = "dodge") + labs(title = "What's the average ride duration for casual users and members throughout the week?", subtitle = "Data spans over 12 months", y = "Average ride duration in seconds", x = "Day of the week", fill = "Rider type")


#Export summary csv file for visualization via tableau 
getwd()
write_csv(total_trips, file = "final_summary.csv")
#Save to current working directory 



