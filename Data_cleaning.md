# Data Preparation and Cleaning for Analysis

The purpose is to transform all of the raw .csv files for each month into one single, cleaned csv ready for analysis.

Everything was done with Rstudio.

## Is the data reliable?
 1. Data is reliable since it is internal data provided by the company itself
 2. Data is original (internal data)
 3. Data is comprehensive for the purpose of our case study. It provides information on location, trip start and end time, membership type, etc.
 4. Data is current and up to date. I will be using the latest data within one year of the project date (August of 2021 and July of 2022)
 5. Data is cited
---
## Initial set up
First I imported all libraries required for cleaning and uploaded all raw .csv. files. Before combining them into a single data frame, I made sure to confirm that all of the columns are consistent across dataframes, and quickly performed an intial inspection.
``` r
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
```

---
## Summary of data
Let's take a look at the data and what information it gives us.
``` r
#Inspect newly combined data frame
colnames(total_trips)
```

    ##  [1] "ride_id"            "rideable_type"      "started_at"        
    ##  [4] "ended_at"           "start_station_name" "start_station_id"  
    ##  [7] "end_station_name"   "end_station_id"     "start_lat"         
    ## [10] "start_lng"          "end_lat"            "end_lng"           
    ## [13] "member_casual"
    
``` r
nrow(total_trips)
#give total number of trips
```
    ## [1] 5901463

``` r
dim(total_trips)
#give dimension (columns) of dataframe
```
    ## [1] 13
``` r
str(total_trips)
#display internal structure
```
| Character            |    
|:---------------------|
| ride\_id             |
| start\_station\_name |   
| start\_station\_id   |     
| end\_station\_name   |      
| end\_station\_id     |

| Factor               |    
|:---------------------|
| rideable\_type       |
| member\_casual       |   

| Numeric              |    
|:---------------------|
| start\_lat           |
| start\_lng           |   
| end\_lat             |      
| end\_lng             |

| POSIXct              |    
|:---------------------|
| started\_at          |
| ended\_at            | 

---
## Data Cleaning and Preprocessing
### Getting data into a workable format
  - made sure all of the date and time information are in workable format and added new variables as needed for analysis
#### New variables added
  - `hour`: Give hour of the day 
  - `date`: Format date as yyyy-mm-dd
  - `year`: Give 4 digits year
  - `month`: Give month as a number from 1-12 (1 as January)
  - `day`: Give day as a number from 0-31
  - `day_of_week`: Give unabbreviated day of the week
  - `ride_length`: Give duration of ride in seconds from start time to end time. Some length are negative, and will be filtered out later. This is due to Divvy taking bikes in and out of dock for quality control.

#### Issues Resolved
  - **Unreasonable Ride Length:** There are many rows with ride length as negative, too short or too long. I filtered out any trips with trip duration < 0 and are shorter than 1 minute or longer than 24 hours. The reason for these trip durations may be due to maintenance, system errors, etc. These values will either be outliers or irrelevant to our analysis; Therefore, it is better to remove them
  - **Missing,blank, duplicate values:** There are many rows with missing values (mainly location information). I decided to only remove any rows with missing starting and ending time.It's reasonable to remove them because there'd still enough data to perform analysis. I didn't remove rows with missing location information because we will only analyze the top 10 stations customers visit.Removing these as well will remove approximately 20% of our data, which can be use for analysis on trip length, etc.
       - 110579 rows removed

``` r
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
total_trips <- distinct(total_trips) #remove duplicate rows 
total_trips <- total_trips %>%  #remove columns not needed: ride_id, start_station_id, end_station_id, start_lat, start_long, end_lat, end_lng
  select(-c(ride_id, start_station_id, end_station_id,start_lat,start_lng,end_lat,end_lng)) %>%
  filter(
    !is.na(started_at),
    !is.na(ended_at),
    ride_length < 86400,
    ride_length > 60)
#remove any rows with no starting and ending time or unreasonable trip length
#Done cleaning
#110579 rows removed
``` 
