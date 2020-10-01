###########################################################################################-
###########################################################################################-
##
## Combining trips and weather data ----
##
###########################################################################################-
###########################################################################################-

# combining data on Citi Bike trips, stations, and weather conditions

#=========================================================================================#
# Setting up ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Loading libraries
#-----------------------------------------------------------------------------------------#

library(tidyverse)
library(data.table)
library(lubridate)
library(DBI)
library(RSQLite)
library(dbplyr)
library(scales)
library(fs)

#-----------------------------------------------------------------------------------------#
# Connecting to databases
#-----------------------------------------------------------------------------------------#

citibike_trip_db <- dbConnect(SQLite(), "data/citibike_trip_db.sqlite3")
nyc_weather_db   <- dbConnect(SQLite(), "data/weather/nyc_weather_db.sqlite3")

#-----------------------------------------------------------------------------------------#
# Specifying directories
#-----------------------------------------------------------------------------------------#

data_dir <- "data/2018"

dir_create(data_dir)

#=========================================================================================#
# Weather ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Extracting main data ----
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Pulling data from database
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

weather <-
    nyc_weather_db %>%
    tbl("nyc_weather") %>%
    filter(
        year == 2018 &
            month %in% 1:12 &
            usaf_wban %in% c(72503014732, 72505394728) &
            report_type == "FM-15"
    ) %>%
    collect() %>%
    mutate(
        date_time = as_datetime(date_time, tz = "US/Eastern"),
        date_time_hourly = as_datetime(date_time_hourly,  tz = "US/Eastern")
    ) %>%
    complete(usaf_wban, date_time_hourly)


#-----------------------------------------------------------------------------------------#
# Summarizing by hour ----
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Within stations
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

## Averaging multiple observations if present

weather_hourly <-
    
    weather %>%
    
    # using data.table methods, because they're *way* faster
    
    as.data.table() %>% 
    
    # mean of hourly value per station
    
    .[, .(
        temperature = mean(temperature, na.rm = TRUE),
        dew_point   = mean(dew_point, na.rm = TRUE),
        humidity    = mean(humidity, na.rm = TRUE),
        wind_speed  = mean(wind_speed, na.rm = TRUE),
        sky_cover   = max(sky_cover, na.rm = TRUE),
        precip      = mean(precip, na.rm = TRUE),
        heat_index  = mean(heat_index, na.rm = TRUE),
        precip_yn   = max(precip_yn, na.rm = TRUE)
    ), 
    keyby = .(date_time_hourly, usaf_wban)] %>%
    
    # mean of hourly values between stations
    
    .[, .(
        temperature = mean(temperature, na.rm = TRUE),
        dew_point   = mean(dew_point, na.rm = TRUE),
        humidity    = mean(humidity, na.rm = TRUE),
        wind_speed  = mean(wind_speed, na.rm = TRUE),
        sky_cover   = max(sky_cover, na.rm = TRUE),
        precip      = mean(precip, na.rm = TRUE),
        heat_index  = mean(heat_index, na.rm = TRUE),
        precip_yn   = max(precip_yn, na.rm = TRUE)
    ), 
    keyby = .(date_time_hourly)] %>%
    
    mutate(
        sky_cover = if_else(!is.finite(sky_cover), 0, sky_cover),
        precip_yn = if_else(!is.finite(precip_yn), 0, precip_yn)
    ) %>% 
    
    mutate(
        date   = as_date(date_time_hourly),
        year   = year(date_time_hourly)  %>% as.integer(),
        month  = month(date_time_hourly) %>% as.integer(),
        day    = day(date_time_hourly)   %>% as.integer(),
        hour   = hour(date_time_hourly)  %>% as.integer()
    ) %>% 
    
    as_tibble()


#-----------------------------------------------------------------------------------------#
# Extracting daily UV Index ----
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Pulling data from database
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

daily_uv <-
    nyc_weather_db %>%
    tbl("daily_uv") %>%
    filter(year == 2018 & month %in% 1:12) %>%
    select(
        date,
        year,
        month,
        day,
        daily_uv_index = uv_index,
        daily_ozone = ozone
    ) %>%
    collect() %>%
    mutate(date = as_date(date))


#-----------------------------------------------------------------------------------------#
# Combining all weather data
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Combining
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

weather_hourly_all <- left_join(weather_hourly, daily_uv)


#=========================================================================================#
# Trips ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Pulling out of database ----
#-----------------------------------------------------------------------------------------#

trips <- 
    citibike_trip_db %>%
    tbl("citibike_trips") %>%
    filter(year == 2018 & month %in% 1:12, !is.na(start_station_id)) %>%
    select(
        start_time,
        start_station_id,
        start_station_name
    ) %>%
    collect() %>%
    mutate(
        start_station_name = start_station_name %>% str_replace("Coming soon", "") %>% str_squish(),
        start_time = as_datetime(start_time, tz = "US/Eastern"),
        start_time_hourly = floor_date(start_time, "hours")
    )


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Disconnecting
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

dbDisconnect(citibike_trip_db)


#-----------------------------------------------------------------------------------------#
# Grouping by stations ----
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Station list
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

stations <- read_rds("data/stations.rds")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Station list
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Creating a complete list of hours during this time span, both for completeness, but also 
#   for the pirpose of making the trend variable

start_time_seq_df <-
    trips %>%
    summarise(
        start_time_min = min(start_time_hourly, na.rm = TRUE),
        start_time_max = max(start_time_hourly, na.rm = TRUE)) %$%
    seq(start_time_min, start_time_max, by = "1 hour") %>%
    tibble(start_time_hourly = .)


#== Creating a list of stations ==#

# one name

stations <- 
    trips %>%
    arrange(start_station_id, desc(start_time_hourly)) %>% 
    select(start_station_id, start_station_name) %>%
    distinct(start_station_id, .keep_all = TRUE)

write_rds(stations, path(data_dir, "stations.rds"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# hours with trips
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

## At each station, hours without trips are implicitly missing

####---- Summarizing ----###

trips_station_hourly_count <-
    trips %>%
    count(start_time_hourly, start_station_id) %>%
    rename(trips = n)


####---- Combining ----###

trips_station_hourly <- 
    left_join(
        trips_station_hourly_count,
        start_time_seq_df,
        by = "start_time_hourly"
    ) %>%
    select(
        start_time_hourly,
        start_station_id,
        trips
    )


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# all stations & all hours ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Explicitly listing out all hour + statyion combos, so that no observations are implicitly missing
#    At each station, hours without trips will be = `0` explicitly

trips_station_hourly_with_0s <- 
    full_join(
        start_time_seq_df, 
        trips_station_hourly, 
        by = "start_time_hourly"
    ) %>% 
    complete(
        start_time_hourly, 
        start_station_id,
        fill = list(trips = 0)
    ) %>% 
    left_join(
        .,
        stations,
        by = "start_station_id"
    ) %>% 
    drop_na(start_station_id, start_station_name)


#-----------------------------------------------------------------------------------------#
# all stations & all hours ----
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Adding recoded weather variables to trips data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trips_station_weather_data_with_0s <- 
    
    weather_hourly_all %>%

    mutate(wday = wday(date_time_hourly, label = TRUE)) %>%

    left_join(
        trips_station_hourly_with_0s,
        .,
        by = c("start_time_hourly" = "date_time_hourly")
    ) %>%

    rename(date_time_hourly = start_time_hourly) %>%
    arrange(date_time_hourly, start_station_id) %>% 
    
    mutate(
        trend24 = 1:nrow(.)/24,
        # 0 point is January 1st, 1 unit is 1 year
        
        # MULTIPLE HOURS 1:24 BY YDAY, THAT'S NUMBER OF HOURS SINCE THE START OF THE YEAR
        
        total_trend_yearly = (yday(date_time_hourly)-1)/(365.25),
        sin_year = sin(2 * pi * total_trend_yearly),
        cos_year = cos(2 * pi * total_trend_yearly),
        
        # 0 point is beginning, 1 unit is 1 day
        
        year_trend_daily = (yday(date_time_hourly)-1),
        
        # 0 point is noon, 1 unit is 1 day
        
        day_trend_hourly = (hour(date_time_hourly)-11)/24,
        sin_day = sin(2 * pi * year_trend_daily),
        cos_day = cos(2 * pi * year_trend_daily)
    )


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Writing RDS ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

write_rds(
    trips_station_weather_data_with_0s,
    path(
        data_dir, 
        "trips_station_weather_data_with_0s_2018.rds"
    ), 
    compress = "gz"
)

#=========================================================================================#
# Clearing workspace ----
#=========================================================================================#

rm(list = ls())

gc()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
