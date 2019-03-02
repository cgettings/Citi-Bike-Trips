############################################################-
############################################################-
##
## Analyzing time series for March, April, May ----
##
############################################################-
############################################################-

#===========================================================#
# Setting up ----
#===========================================================#

#-----------------------------------------------------------#
# Loading libraries ----
#-----------------------------------------------------------#

library(plyr)
library(lubridate)
library(tidyverse)
library(tsibble)
library(forecast)
library(DBI)
library(RSQLite)
library(dbplyr)
library(magrittr)
library(weathermetrics)
library(dynlm)
library(tsibble)
library(broom)
library(scales)
library(lme4)
library(glue)

# setwd("~/BASP/R analyses/Bike Share/NYC - Citi Bike/Trip Data/")

#-----------------------------------------------------------#
# Loading data from RDS ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Trips ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

####---- Raw trips ----####

# trips <-
#     read_rds("./data/march_april_may/trips.rds")

####---- Hourly trips ----####

# trips_hourly <-
#     read_rds("./data/march_april_may/trips_hourly.rds")

####---- Hourly trips with stations ----####

trips_station_hourly <-
    read_rds("./data/march_april_may/trips_station_hourly.rds")

####---- Hourly trips with stations - all combos of hours and stations ----####

trips_station_hourly_all <-
    read_rds("./data/march_april_may/trips_station_hourly_all.rds")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Weather ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

####---- Main data ----####

# weather <- 
#     read_rds("./data/march_april_may/weather.rds")
# 
# weather_hourly <-
#     read_rds("./data/march_april_may/weather_hourly.rds")


####---- Daily UV Index ----####

# daily_uv <-
#     read_rds("./data/march_april_may/daily_uv.rds")


####---- 3-minute UV Index ----####

# three_min_uv <-
#     read_rds("./data/march_april_may/three_min_uv.rds")
# 
# three_min_uv_hourly <-
#     read_rds("./data/march_april_may/three_min_uv_hourly.rds")


####---- Combined ----####

weather_hourly <- 
    read_rds("./data/march_april_may/weather_hourly_all.rds")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Combined with seasonal decompositions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trips_weather_data <-
    read_rds("./data/march_april_may/trips_weather_data.rds")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Combined with stations ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

### complete cases only ----

trips_station_weather_data <-
    read_rds("./data/march_april_may/trips_station_weather_data.rds")


### all combos of stations and hours ----

trips_station_weather_data_all <-
    read_rds("./data/march_april_may/trips_station_weather_data_all.rds")

#-----------------------------------------------------------#
# Connecting to databases ----
#-----------------------------------------------------------#

# citibike_trip_db <- dbConnect(SQLite(), "./data/citibike_trip_db.sqlite3")
# nyc_weather_db <- dbConnect(SQLite(), "./data/weather/nyc_weather_db.sqlite3")

#===========================================================#
# Weather ----
#===========================================================#

#-----------------------------------------------------------#
# Main data ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Pulling data from database ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# weather <-
#     nyc_weather_db %>%
#     tbl("nyc_weather") %>%
#     filter(
#         year == 2018 &
#             month %in% 3:5 &
#             usaf_wban %in% c(72503014732, 72505394728) &
#             report_type == "FM-15"
#     ) %>%
#     # select(-name, -usaf_wban, -callsign, -lat, -lon, -elevation, -pressure) %>%
#     # select(-lat, -lon, -elevation, -pressure) %>%
#     collect() %>%
#     mutate(
#         date_time = as_datetime(date_time, tz = "US/Eastern"),
#         date_time_hourly = as_datetime(date_time_hourly,  tz = "US/Eastern")
#         # heat_index = heat.index(
#         #     t = temperature,
#         #     dp = dew_point,
#         #     temperature.metric = "celsius",
#         #     output.metric = "celsius"
#         # )
#     ) %>% 
#     complete(usaf_wban, date_time_hourly) 


####---- Writing RDS ----####

# write_rds(
#     weather,
#     "./data/march_april_may/weather.rds")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Summarizing by hour ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

####---- Within stations ----####

# weather_station_hourly <-
#     weather %>%
#     group_by(usaf_wban, date_time_hourly) %>%
#     summarise(
#         temperature = mean(temperature, na.rm = TRUE),
#         dew_point   = mean(dew_point, na.rm = TRUE),
#         humidity    = mean(humidity, na.rm = TRUE),
#         wind_speed  = mean(wind_speed, na.rm = TRUE),
#         sky_cover   = mean(sky_cover, na.rm = TRUE),
#         precip      = mean(precip, na.rm = TRUE),
#         precip_yn   = mean(precip_yn, na.rm = TRUE),
#         heat_index  = mean(heat_index, na.rm = TRUE)
#     ) %>% 
#     ungroup() %>% 
#     mutate(callsign = recode(usaf_wban, "72503014732" = "KLGA", "72505394728" = "KNYC")) %>% 
#     select(-usaf_wban) %>% 
#     arrange(callsign, date_time_hourly)


####---- Replacing missing ----####

# weather_hourly <- weather_station_hourly
# 
# # temperature
# 
# weather_hourly <- 
#     weather_hourly %>% 
#     
#     select(date_time_hourly, callsign, temperature) %>% 
#     spread(key = callsign, value = temperature) %>%
#     mutate(KNYC = case_when(is.na(KNYC) ~ lag(KNYC) + (KLGA - lag(KLGA)), TRUE ~ identity(KNYC))) %>% 
#     mutate(KNYC = case_when(is.na(KNYC) ~ KLGA, TRUE ~ identity(KNYC))) %>% 
#     gather(key = "callsign", value = "temperature", KLGA, KNYC) %>% 
#     left_join(weather_hourly, ., by = c("callsign", "date_time_hourly"), suffix = c("", "_new"))
# 
# # dew_point
# 
# weather_hourly <- 
#     weather_hourly %>% 
#     
#     select(date_time_hourly, callsign, dew_point) %>% 
#     spread(key = callsign, value = dew_point) %>%
#     mutate(KNYC = case_when(is.na(KNYC) ~ lag(KNYC) + (KLGA - lag(KLGA)), TRUE ~ identity(KNYC))) %>% 
#     mutate(KNYC = case_when(is.na(KNYC) ~ KLGA, TRUE ~ identity(KNYC))) %>% 
#     gather(key = "callsign", value = "dew_point", KLGA, KNYC) %>% 
#     left_join(weather_hourly, ., by = c("callsign", "date_time_hourly"), suffix = c("", "_new"))
# 
# # humidity
# 
# weather_hourly <- 
#     weather_hourly %>% 
#     
#     select(date_time_hourly, callsign, humidity) %>% 
#     spread(key = callsign, value = humidity) %>%
#     mutate(KNYC = case_when(is.na(KNYC) ~ lag(KNYC) + (KLGA - lag(KLGA)), TRUE ~ identity(KNYC))) %>% 
#     mutate(KNYC = case_when(is.na(KNYC) ~ KLGA, TRUE ~ identity(KNYC))) %>% 
#     gather(key = "callsign", value = "humidity", KLGA, KNYC) %>% 
#     left_join(weather_hourly, ., by = c("callsign", "date_time_hourly"), suffix = c("", "_new"))
# 
# # wind_speed
# 
# weather_hourly <- 
#     weather_hourly %>% 
#     
#     select(date_time_hourly, callsign, wind_speed) %>% 
#     spread(key = callsign, value = wind_speed) %>%
#     mutate(KNYC = case_when(is.na(KNYC) ~ lag(KNYC) + (KLGA - lag(KLGA)), TRUE ~ identity(KNYC))) %>% 
#     mutate(KNYC = case_when(is.na(KNYC) ~ KLGA, TRUE ~ identity(KNYC))) %>% 
#     gather(key = "callsign", value = "wind_speed", KLGA, KNYC) %>% 
#     left_join(weather_hourly, ., by = c("callsign", "date_time_hourly"), suffix = c("", "_new"))
# 
# # sky_cover
# 
# weather_hourly <-
#     weather_hourly %>% 
#     
#     select(date_time_hourly, callsign, sky_cover) %>% 
#     spread(key = callsign, value = sky_cover) %>%
#     mutate(KNYC = case_when(is.na(KNYC) ~ KLGA,      TRUE ~ identity(KNYC))) %>% 
#     mutate(KNYC = case_when(is.na(KNYC) ~ lag(KNYC), TRUE ~ identity(KNYC))) %>% 
#     mutate(KNYC = case_when(is.na(KNYC) ~ lag(KLGA), TRUE ~ identity(KNYC))) %>% 
#     gather(key = "callsign", value = "sky_cover", KLGA, KNYC) %>% 
#     left_join(weather_hourly, ., by = c("callsign", "date_time_hourly"), suffix = c("", "_new"))
# 
# # precip
# 
# weather_hourly <-
#     weather_hourly %>%
#     
#     select(date_time_hourly, callsign, precip) %>% 
#     spread(key = callsign, value = precip) %>%
#     mutate(KNYC = case_when(is.na(KNYC) ~ lag(KNYC) + (KLGA - lag(KLGA)), TRUE ~ identity(KNYC))) %>% 
#     mutate(KNYC = case_when(is.na(KNYC) ~ KLGA, TRUE ~ identity(KNYC))) %>% 
#     gather(key = "callsign", value = "precip", KLGA, KNYC) %>% 
#     left_join(weather_hourly, ., by = c("callsign", "date_time_hourly"), suffix = c("", "_new"))
# 
# # precip_yn (replacing, b/c categorical)
# 
# weather_hourly <-
#     weather_hourly %>%
#     
#     select(date_time_hourly, callsign, precip_yn) %>% 
#     spread(key = callsign, value = precip_yn) %>%
#     mutate(KNYC = case_when(is.na(KNYC) ~ KLGA, TRUE ~ identity(KNYC))) %>% 
#     mutate(KNYC = case_when(is.na(KNYC) ~ lag(KNYC), TRUE ~ identity(KNYC))) %>% 
#     mutate(KNYC = case_when(is.na(KNYC) ~ lag(KLGA), TRUE ~ identity(KNYC))) %>% 
#     gather(key = "callsign", value = "precip_yn", KLGA, KNYC) %>% 
#     left_join(weather_hourly, ., by = c("callsign", "date_time_hourly"), suffix = c("", "_new"))
# 
# # heat_index
# 
# weather_hourly <-
#     weather_hourly %>%
#     
#     select(date_time_hourly, callsign, heat_index) %>% 
#     spread(key = callsign, value = heat_index) %>%
#     mutate(KNYC = case_when(is.na(KNYC) ~ lag(KNYC) + (KLGA - lag(KLGA)), TRUE ~ identity(KNYC))) %>% 
#     mutate(KNYC = case_when(is.na(KNYC) ~ KLGA, TRUE ~ identity(KNYC))) %>% 
#     gather(key = "callsign", value = "heat_index", KLGA, KNYC) %>% 
#     left_join(weather_hourly, ., by = c("callsign", "date_time_hourly"), suffix = c("", "_new"))


####---- Writing RDS ----####

# write_rds(
#     weather_hourly,
#     "./data/march_april_may/weather_hourly.rds")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Keeping only new variables from KNYC ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# weather_hourly_new <- 
#     weather_hourly %>% 
#     filter(callsign == "KNYC") %>% 
#     select(-temperature:-heat_index) %>% 
#     rename_at(
#         vars(temperature_new:heat_index_new), 
#         funs(str_remove(string = ., pattern = "_new"))
#     ) %>% 
#     mutate(
#         date   = as_date(date_time_hourly),
#         year   = year(date_time_hourly)  %>% as.integer(),
#         month  = month(date_time_hourly) %>% as.integer(),
#         day    = day(date_time_hourly)   %>% as.integer(),
#         hour   = hour(date_time_hourly)  %>% as.integer()
#     )


####---- Writing RDS ----####

# write_rds(
#     weather_hourly_new,
#     "./data/march_april_may/weather_hourly_new.rds")


#-----------------------------------------------------------#
# Daily UV Index -----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Pulling data from database ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# daily_uv <-
#     nyc_weather_db %>%
#     tbl("daily_uv") %>%
#     filter(year == 2018 & month %in% 3:5) %>%
#     select(date, year, month, day, uv_index, ozone) %>%
#     collect() %>%
#     mutate(date = as_date(date)) %>% 
#     rename(
#         daily_uv_index = uv_index,
#         daily_ozone = ozone
#     )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Writing RDS ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# write_rds(
#     daily_uv,
#     "./data/march_april_may/daily_uv.rds")


#-----------------------------------------------------------#
# 3-minute UV Index ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Pulling data from database ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# three_min_uv <-
#     nyc_weather_db %>%
#     tbl("three_min_uv") %>%
#     filter(year == 2018 & month %in% 3:5) %>%
#     select(date, date_time, date_time_hourly, year:minute, uv_index) %>%
#     collect() %>%
#     mutate(
#         date = as_date(date),
#         date_time = as_datetime(date_time, tz = "US/Eastern"),
#         date_time_hourly = as_datetime(date_time_hourly,  tz = "US/Eastern")
#     )

####---- Writing RDS ----####

# write_rds(
#     three_min_uv,
#     "./data/march_april_may/three_min_uv.rds")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Summarizing by hour ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# date_time_seq_df <- 
#     three_min_uv %>% 
#     summarise(
#         date_time_min = min(date_time_hourly, na.rm = TRUE), 
#         date_time_max = max(date_time_hourly, na.rm = TRUE)) %$% 
#     seq(date_time_min, date_time_max, by = "1 hour") %>% 
#     data_frame(date_time_seq = .)
# 
# 
# three_min_uv_hourly <-
#     three_min_uv %>%
#     group_by(date_time_hourly) %>%
#     summarise(hourly_uv_index  = mean(uv_index, na.rm = TRUE)) %>%
#     left_join(., three_min_uv %>% select(date_time_hourly, year, month, day, hour) %>% distinct()) %>% 
#     right_join(., date_time_seq_df, by = c("date_time_hourly" = "date_time_seq"))


####---- Writing RDS ----####

# write_rds(
#     three_min_uv_hourly,
#     "./data/march_april_may/three_min_uv_hourly.rds")


#-----------------------------------------------------------#
# Combining all weather data ----
#-----------------------------------------------------------#
# 
# weather_hourly_all <-
#     weather_hourly_new %>%
#     left_join(., three_min_uv_hourly) %>%
#     left_join(., daily_uv)


####---- Writing RDS ----####

# write_rds(
#     weather_hourly_all,
#     "./data/march_april_may/weather_hourly_all.rds")


#===========================================================#
# Trips ----
#===========================================================#

#-----------------------------------------------------------#
# Summarizing trips by hour ----
#-----------------------------------------------------------#


### Pulling out of database ###

# trips <- 
#     citibike_trip_db %>% 
#     tbl("citibike_trips") %>% 
#     filter(year == 2018 & month %in% 3:5) %>% 
#     select(
#         bike_id,
#         trip_duration,
#         start_time,
#         start_station_id, 
#         start_station_name,
#         stop_time,
#         end_station_id,
#         end_station_name) %>% 
#     collect() %>% 
#     mutate(
#         start_time = as_datetime(start_time, tz = "US/Eastern"),
#         stop_time  = as_datetime(stop_time,  tz = "US/Eastern")) %>% 
#     mutate(
#         start_time_hourly = floor_date(start_time, "hours"),
#         stop_time_hourly  = floor_date(stop_time,  "hours"))


### Writing RDS ###

# write_rds(
#     trips,
#     "./data/march_april_may/trips.rds",
#     compress = "gz",
#     compression = 9
# )


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Collapsing across stations ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #


# trips_hourly_count <-
#     trips %>%
#     count(start_time_hourly) %>%
#     rename(trips = n)
# 
# write_rds(trips_hourly_count, "./data/march_april_may/trips_hourly_count.rds")
# 
# 
# trips_hourly_duration <- 
#     trips %>% 
#     group_by(start_time_hourly) %>% 
#     summarise(average_trip_duration = mean(trip_duration))
# 
# write_rds(trips_hourly_duration, "./data/march_april_may/trips_hourly_duration.rds")
# 
# 
# trips_hourly <-
#     full_join(
#         trips_hourly_count,
#         trips_hourly_duration,
#         by = "start_time_hourly") %>% 
#     as_tsibble(key = id(), index = start_time_hourly) %>% 
#     fill_na()
# 
# 
# write_rds(trips_hourly, "./data/march_april_may/trips_hourly.rds")
# 
# rm(trips)
# rm(trips_hourly_count)
# rm(trips_hourly_duration)
# gc()


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Grouping by stations ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

### complete cases ----

trips_station_hourly_count <-
    trips %>%
    count(start_time_hourly, start_station_id) %>%
    rename(trips = n)

write_rds(
    trips_station_hourly_count,
    "./data/march_april_may/trips_station_hourly_count.rds")


trips_station_hourly_duration <-
    trips %>%
    group_by(start_time_hourly, start_station_id) %>%
    summarise(average_trip_duration = mean(trip_duration, na.rm = TRUE))

write_rds(
    trips_station_hourly_duration,
    "./data/march_april_may/trips_station_hourly_duration.rds")


trips_station_hourly <-
    full_join(
        trips_station_hourly_count,
        trips_station_hourly_duration,
        by = c("start_time_hourly", "start_station_id")) %>%
    # as_tsibble(key = id(), index = start_time_hourly) %>%
    left_join(
        .,
        trips %>%
            select(start_station_id, start_station_name) %>%
            distinct(start_station_id, .keep_all = TRUE),
        by = "start_station_id") %>%
    select(
        start_time_hourly,
        start_station_id,
        start_station_name,
        trips,
        average_trip_duration
    ) #%>%
    # complete(start_time_hourly, start_station_id)


write_rds(
    trips_station_hourly,
    "./data/march_april_may/trips_station_hourly.rds")

# rm(trips)
# rm(trips_station_hourly_count)
# rm(trips_station_hourly_duration)
# gc()


### all combinations ----

# start_time_seq_df <-
#     trips_station_hourly %>%
#     summarise(
#         start_time_min = min(start_time_hourly, na.rm = TRUE),
#         start_time_max = max(start_time_hourly, na.rm = TRUE)) %$%
#     seq(start_time_min, start_time_max, by = "1 hour") %>%
#     data_frame(start_time_hourly = .)
# 
# stations <- trips_station_hourly %>% select(start_station_id, start_station_name) %>% distinct()
# 
# trips_station_hourly_all <- 
#     trips_station_hourly %>%
#     group_by(start_time_hourly, start_station_id) %>%
#     full_join(start_time_seq_df, ., by = "start_time_hourly") %>%
#     complete(start_station_id, start_time_hourly, fill = list(trips = 0, average_trip_duration = NA_real_)) %>% 
#     drop_na(start_station_id)
# 
# write_rds(
#     trips_station_hourly_all,
#     "./data/march_april_may/trips_station_hourly_all.rds")


#===========================================================#
# Combining trips and weather ----
#===========================================================#

#-----------------------------------------------------------#
# Collapsing across stations ----
#-----------------------------------------------------------#


# trips_weather_data <- 
#     weather_hourly %>%
#     
#     mutate(
#         # .rownames = 1:nrow(.) %>% as.character(),
#         # trend1    = 1:nrow(.),
#         trend24   = 1:nrow(.)/24,
#         # trend168  = 1:nrow(.)/168,
#         temperature_c = QuantPsyc::meanCenter(temperature),
#         dew_point_c   = QuantPsyc::meanCenter(dew_point),
#         humidity_c    = QuantPsyc::meanCenter(humidity),
#         wind_speed_c  = QuantPsyc::meanCenter(wind_speed),
#         heat_index_c  = QuantPsyc::meanCenter(heat_index),
#         hourly_uv_index_c  = QuantPsyc::meanCenter(hourly_uv_index),
#         daily_uv_index_c   = QuantPsyc::meanCenter(daily_uv_index),
#         daily_ozone_c      = QuantPsyc::meanCenter(daily_ozone)) %>% 
#     
#     mutate(
#         trend24_r         = rescale(trend24),
#         temperature_r     = rescale(temperature),
#         dew_point_r       = rescale(dew_point),
#         humidity_r        = rescale(humidity),
#         wind_speed_r      = rescale(wind_speed),
#         precip_r          = rescale(precip),
#         heat_index_r      = rescale(heat_index),
#         hourly_uv_index_r = rescale(hourly_uv_index),
#         daily_uv_index_r  = rescale(daily_uv_index),
#         daily_ozone_r     = rescale(daily_ozone),
#         sky_cover         = as.factor(sky_cover)) %>%
#     
#     mutate(wday = wday(date_time_hourly, label = TRUE)) %>%
#     
#     left_join(trips_hourly, 
#               ., 
#               by = c("start_time_hourly" = "date_time_hourly")) %>%
#     
#     rename(date_time_hourly = start_time_hourly) %>% 
#     arrange(date_time_hourly)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Writing RDS ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# write_rds(
#     trips_weather_data,
#     "./data/march_april_may/trips_weather_data.rds")


#-----------------------------------------------------------#
# Grouping by stations ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# complete cases only ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trips_station_weather_data <-
    weather_hourly %>%
    
    mutate(
        # .rownames = 1:nrow(.) %>% as.character(),
        # trend1    = 1:nrow(.),
        trend24   = 1:nrow(.)/24,
        # trend168  = 1:nrow(.)/168,
        temperature_c = QuantPsyc::meanCenter(temperature),
        dew_point_c   = QuantPsyc::meanCenter(dew_point),
        humidity_c    = QuantPsyc::meanCenter(humidity),
        wind_speed_c  = QuantPsyc::meanCenter(wind_speed),
        heat_index_c  = QuantPsyc::meanCenter(heat_index),
        hourly_uv_index_c  = QuantPsyc::meanCenter(hourly_uv_index),
        daily_uv_index_c   = QuantPsyc::meanCenter(daily_uv_index),
        daily_ozone_c      = QuantPsyc::meanCenter(daily_ozone)) %>%
    
    mutate(
        trend24_r         = rescale(trend24),
        temperature_r     = rescale(temperature),
        dew_point_r       = rescale(dew_point),
        humidity_r        = rescale(humidity),
        wind_speed_r      = rescale(wind_speed),
        precip_r          = rescale(precip),
        heat_index_r      = rescale(heat_index),
        hourly_uv_index_r = rescale(hourly_uv_index),
        daily_uv_index_r  = rescale(daily_uv_index),
        daily_ozone_r     = rescale(daily_ozone),
        sky_cover         = as.factor(sky_cover)) %>%
    
    mutate(wday = wday(date_time_hourly, label = TRUE)) %>%
    
    left_join(trips_station_hourly,
              .,
              by = c("start_time_hourly" = "date_time_hourly")) %>%
    
    rename(date_time_hourly = start_time_hourly) %>%
    arrange(date_time_hourly, start_station_id)


### Writing RDS ----

write_rds(
    trips_station_weather_data,
    "./data/march_april_may/trips_station_weather_data.rds", compress = "gz")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# all combos of stations and hours ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# trips_station_weather_data_all <-
#     weather_hourly %>%
#     
#     mutate(
#         # .rownames = 1:nrow(.) %>% as.character(),
#         # trend1    = 1:nrow(.),
#         trend24   = 1:nrow(.)/24,
#         # trend168  = 1:nrow(.)/168,
#         temperature_c = QuantPsyc::meanCenter(temperature),
#         dew_point_c   = QuantPsyc::meanCenter(dew_point),
#         humidity_c    = QuantPsyc::meanCenter(humidity),
#         wind_speed_c  = QuantPsyc::meanCenter(wind_speed),
#         heat_index_c  = QuantPsyc::meanCenter(heat_index),
#         hourly_uv_index_c  = QuantPsyc::meanCenter(hourly_uv_index),
#         daily_uv_index_c   = QuantPsyc::meanCenter(daily_uv_index),
#         daily_ozone_c      = QuantPsyc::meanCenter(daily_ozone)) %>%
#     
#     mutate(
#         trend24_r         = rescale(trend24),
#         temperature_r     = rescale(temperature),
#         dew_point_r       = rescale(dew_point),
#         humidity_r        = rescale(humidity),
#         wind_speed_r      = rescale(wind_speed),
#         precip_r          = rescale(precip),
#         heat_index_r      = rescale(heat_index),
#         hourly_uv_index_r = rescale(hourly_uv_index),
#         daily_uv_index_r  = rescale(daily_uv_index),
#         daily_ozone_r     = rescale(daily_ozone),
#         sky_cover         = as.factor(sky_cover)) %>%
#     
#     mutate(wday = wday(date_time_hourly, label = TRUE)) %>%
#     
#     left_join(trips_station_hourly_all,
#               .,
#               by = c("start_time_hourly" = "date_time_hourly")) %>%
#     
#     rename(date_time_hourly = start_time_hourly) %>%
#     arrange(date_time_hourly, start_station_id)


### Writing RDS ----

# write_rds(
#     trips_station_weather_data_all,
#     "./data/march_april_may/trips_station_weather_data_all.rds", compress = "gz")


#===========================================================#
# Playing with time series ----
#===========================================================#

#-----------------------------------------------------------#
# Weather - Multi-Seasonal Time Series ----
#-----------------------------------------------------------#

## Creating a multi-seasonal time series with seasonal periods of
##  24 hours, and 365 days (6360 hours), with observations every
##  1 hour (freq = 24)


# temperature_msts <-
#     weather_hourly %$%
#     msts(
#         temperature,
#         # seasonal.periods = c(24, 24 * 365),
#         seasonal.periods = c(24),
#         start = c(hour(min(date_time_hourly)), yday(min(date_time_hourly))),
#         ts.frequency = 24)


### Adding msts column into data frame ###

# weather_hourly_msts <- 
#     weather_hourly %>% 
#     as_tsibble(key = id(), index = date_time_hourly) %>%
#     mutate(temperature_msts = temperature_msts)


### Fitting multi-seasonal STL decomposition ###

# temperature_mstl <- 
#     weather_hourly_msts %$%
#     mstl(
#         temperature_msts,
#         iterate = 5,
#         lambda = NULL,
#         # s.window = c(24 * 7, 24 * 365 * 7),
#         s.window = c(24 * 3, 24 * 365 * 3),
#         s.degree = 1, 
#         # t.window = 24 * 7 * 1,
#         t.window = 24 * 7 * 6,
#         t.degree = 1,
#         robust = TRUE
#     )


### Plotting MSTL ###

# temperature_mstl %>%
#     autoplot() +
#     theme_bw()


### Forecast ###

# forecast(temperature_mstl,
#          h = 24 * 7,
#          level = 80) %>%
#     autoplot() +
#     theme_bw() +
#     coord_cartesian(xlim = c(89, 100))


### Extracting seasonally adjusted data ###

# weather_temperature_hourly_decomp <-
#     weather_hourly_msts %>% 
#     mutate(
#         seasonal24_temperature     = seasonal(temperature_mstl),
#         # seasonal24_temperature   = seasonal(temperature_mstl)[,1],
#         # seasonal8760_temperature = seasonal(temperature_mstl)[,2],
#         trend_temperature          = trendcycle(temperature_mstl),
#         remainder_temperature      = remainder(temperature_mstl),
#         seasadj_temperature        = seasadj(temperature_mstl)
#     )


### Plotting seasonally adjected data ###

# autoplot(weather_hourly_msts$msts, series = "Data") +
#     # autolayer(weather_temperature_hourly_decomp$remainder_temperature, series = "Remainder") +
#     autolayer(weather_temperature_hourly_decomp$seasadj_temperature, series = "Seasonally Adjusted") +
#     autolayer(weather_temperature_hourly_decomp$trend_temperature, series = "Trend") +
#     xlab("Date") + ylab("Trips") +
#     scale_colour_manual(
#         values = c("gray70", "red", "black"),
#         breaks = c("Data", "Seasonally Adjusted", "Trend")
#     ) + 
#     theme_bw()


#-----------------------------------------------------------#
# Trips - Multi-Seasonal Time Series ----
#-----------------------------------------------------------#

## Creating a multi-seasonal time series with seasonal periods of
##  24 hours, and 7 days (168 hours), with observations every
##  1 hour (freq = 24)


# trips_msts <- 
#     trips_hourly %$%
#     msts(
#         trips,
#         seasonal.periods = c(24, 24 * 7),
#         start = c(1, 1),
#         ts.frequency = 24)


### Adding msts column into data frame ###

# trips_hourly_msts <- 
#     trips_hourly %>% 
#     as_tsibble(key = id(), index = start_time_hourly) %>%
#     mutate(trips_msts = trips_msts)


### Fitting multi-seasonal STL decomposition ###

# trips_hourly_mstl <- 
#     trips_hourly_msts %$%
#     mstl(
#         trips_msts,
#         iterate = 5,
#         lambda = NULL,
#         # s.window = c(24 * .5, 24 * 3),
#         s.window = c(24 * 3, 24 * 21),
#         # s.window = c(24, 24*7),
#         s.degree = 1, 
#         # t.window = 24 * 7 * 1,
#         t.window = 24 * 7 * 6,
#         t.degree = 1,
#         robust = TRUE
#     )


### Plotting MSTL ###

# trips_hourly_mstl %>%
#     autoplot() +
#     theme_bw()


### Forecast ###

# forecast(trips_hourly_mstl,
#          h = 24 * 7,
#          level = 80) %>%
#     autoplot() +
#     theme_bw() +
#     # coord_cartesian(xlim = c(13, 15.125))
#     coord_cartesian(xlim = c(89, 100))


### Extracting seasonally adjusted data ###

# trips_hourly_decomp <-
#     trips_hourly_msts %>% 
#     mutate(
#         seasonal24_trips  = seasonal(trips_hourly_mstl)[,1],
#         seasonal168_trips = seasonal(trips_hourly_mstl)[,2],
#         trend_trips       = trendcycle(trips_hourly_mstl),
#         remainder_trips   = remainder(trips_hourly_mstl),
#         seasadj_trips     = seasadj(trips_hourly_mstl)
#     )


### Plotting seasonally adjected data ###

# autoplot(trips_hourly_msts$trips_msts, series = "Data") +
#     # autolayer(trips_hourly_temperature_decomp$remainder, series = "Remainder") +
#     autolayer(trips_hourly_temperature_decomp$seasadj_trips, series = "Seasonally Adjusted") +
#     autolayer(trips_hourly_temperature_decomp$trend_trips, series = "Trend") +
#     xlab("Date") + ylab("Trips") +
#     scale_colour_manual(
#         values = c("gray70", "red", "black"),
#         breaks = c("Data", "Seasonally Adjusted", "Trend")
#     ) + theme_bw()



#===========================================================#
# Playing with plots ----
#===========================================================#


trips_weather_data %>% 
    filter(average_trip_duration < 3600 & month(date_time_hourly) == 4) %>% 
    ggplot(aes(average_temperature, average_trip_duration)) + 
    geom_point() + 
    geom_smooth(method = "lm")

trips_weather_data %>% 
    filter(average_trip_duration < 3600 & month(date_time_hourly) == 4) %>% 
    ggplot(aes(date_time_hourly, average_trip_duration)) + 
    geom_line()

trips_weather_data %$% cor.test(average_temperature, trips)
trips_weather_data %$% cor.test(remainder_temperature, remainder_trips)

trips_weather_data %$% cor.test(seasadj_temperature, seasadj_trips)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

no_zero <- function(x) {sub("^(-?)0.", "\\1.", sprintf("%.1f", x))}

#-----------------------------------------------------------#
# Raw Data ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# faceted by hour ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

data_hour_coefs <- 
    trips_weather_data %>% 
    as_tibble() %>%
    group_by(hour) %>% 
    do(lm(trips ~ average_temperature, data = .) %>% tidy(.)) %>% 
    filter(str_detect(term, "average_temperature")) %>% 
    select(hour, estimate)

temp_trips_plot_hour <- 
    trips_weather_data %>% 
    ggplot(aes(average_temperature, trips)) + 
    geom_point(shape = 1, size = 1) + 
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    geom_label(
        data = data_hour_coefs,
        aes(
            label = paste("b =", round(estimate, 0)),
            x = 5,
            y = 7000
        ),
        color = "red2",
        label.size = NA,
        alpha = .5
    ) +
    labs(
        x = "Average hourly temperature",
        y = "Average hourly trips"
    ) +
    facet_wrap(~ hour, ncol = 6, labeller = "label_both") +
    theme_bw()

ggsave(
    plot = temp_trips_plot_hour,
    filename = "./plots/time_series/temp_trips_plot_hour.png",
    width = 10,
    height = 6,
    dpi = 250,
    scale = 1.25
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# faceted by weekday ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

data_wday_coefs <- 
    trips_weather_data %>% 
    as_tibble() %>%
    group_by(wday) %>% 
    do(lm(trips ~ average_temperature, data = .) %>% tidy(.)) %>% 
    filter(str_detect(term, "average_temperature")) %>% 
    select(wday, estimate)

temp_trips_plot_wday <- 
    trips_weather_data %>% 
    ggplot(aes(average_temperature, trips)) + 
    geom_point(shape = 1, size = 1) + 
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    geom_label(
        data = data_wday_coefs,
        aes(
            label = paste("b =", round(estimate, 0)),
            x = 0,
            y = 7000
        ),
        color = "red2",
        label.size = NA,
        alpha = .5
    ) +
    labs(
        x = "Average hourly temperature",
        y = "Average hourly trips"
    ) +
    facet_wrap(~ wday, ncol = 3, labeller = "label_both") +
    theme_bw()

ggsave(
    plot = temp_trips_plot_wday,
    filename = "./plots/time_series/temp_trips_plot_wday.png",
    width = 10,
    height = 6,
    dpi = 250,
    scale = 1.25
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# faceted by month ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

data_month_coefs <- 
    trips_weather_data %>% 
    as_tibble() %>%
    group_by(month) %>% 
    do(lm(trips ~ average_temperature, data = .) %>% tidy(.)) %>% 
    filter(str_detect(term, "average_temperature")) %>% 
    select(month, estimate)

temp_trips_plot_month <- 
    trips_weather_data %>% 
    ggplot(aes(average_temperature, trips)) + 
    geom_point(shape = 1, size = 1) + 
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    geom_label(
        data = data_wday_coefs,
        aes(
            label = paste("b =", round(estimate, 0)),
            x = 0,
            y = 7000
        ),
        color = "red2",
        label.size = NA,
        alpha = .5
    ) +
    labs(
        x = "Average hourly temperature",
        y = "Average hourly trips"
    ) +
    facet_wrap(~ month, ncol = 4, labeller = "label_both") +
    theme_bw()

ggsave(
    plot = temp_trips_plot_month,
    filename = "./plots/time_series/temp_trips_plot_month.png",
    width = 10,
    height = 3,
    dpi = 250,
    scale = 1.25
)

#-----------------------------------------------------------#
# Remainder ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# faceted by hour ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

remainder_hour_coefs <- 
    trips_weather_data %>% 
    as_tibble() %>%
    group_by(hour) %>% 
    do(lm(remainder_trips ~ remainder_temperature, data = .) %>% tidy(.)) %>% 
    filter(str_detect(term, "remainder_temperature")) %>% 
    select(hour, estimate)

remainder_plot_hour <- 
    trips_weather_data %>% 
    ggplot(aes(remainder_temperature, remainder_trips)) + 
    geom_point(shape = 1, size = 1) + 
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    geom_label(
        data = remainder_hour_coefs,
        aes(
            label = paste("b =", round(estimate, 0)),
            x = -6.25,
            y = 1800
        ),
        color = "red2",
        label.size = NA,
        alpha = .5
    ) +
    labs(
        x = "Average hourly temperature (msts remainder)",
        y = "Average hourly trips (msts remainder)"
        ) +
    facet_wrap(~ hour, ncol = 6, labeller = "label_both") +
    theme_bw()

ggsave(
    plot = remainder_plot_hour,
    filename = "./plots/time_series/remainder_plot_hour_t-window=1wk.png",
    width = 10,
    height = 6,
    dpi = 250,
    scale = 1.25
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# faceted by weekday ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

remainder_wday_coefs <- 
    trips_weather_data %>% 
    as_tibble() %>%
    group_by(wday) %>% 
    do(lm(remainder_trips ~ remainder_temperature, data = .) %>% tidy(.)) %>% 
    filter(str_detect(term, "remainder_temperature")) %>% 
    select(wday, estimate)

remainder_plot_wday <- 
    trips_weather_data %>% 
    ggplot(aes(remainder_temperature, remainder_trips)) + 
    geom_point(shape = 1, size = 1) + 
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    geom_label(
        data = remainder_wday_coefs,
        aes(
            label = paste("b =", round(estimate, 0)),
            x = -6.25,
            y = 2000
        ),
        color = "red2",
        label.size = NA,
        alpha = .5
    ) +
    labs(
        x = "Average hourly temperature (msts remainder)",
        y = "Average hourly trips (msts remainder)"
    ) +
    facet_wrap(~ wday, ncol = 4, labeller = "label_both") +
    theme_bw()

ggsave(
    plot = remainder_plot_wday,
    filename = "./plots/time_series/remainder_plot_wday_t-window=6wk.png",
    width = 10,
    height = 6,
    dpi = 250,
    scale = 1.25
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# faceted by month ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

remainder_month_coefs <- 
    trips_weather_data %>% 
    as_tibble() %>%
    group_by(month) %>% 
    do(lm(remainder_trips ~ remainder_temperature, data = .) %>% tidy(.)) %>% 
    filter(str_detect(term, "remainder_temperature")) %>% 
    select(month, estimate)

remainder_plot_month <- 
    trips_weather_data %>% 
    ggplot(aes(remainder_temperature, remainder_trips)) + 
    geom_point(shape = 1, size = 1) + 
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    geom_label(
        data = remainder_month_coefs,
        aes(
            label = paste("b =", round(estimate, 0)),
            x = -7.5,
            y = 2000
        ),
        color = "red2",
        label.size = NA,
        alpha = .5
    ) +
    labs(
        x = "Average hourly temperature (msts remainder)",
        y = "Average hourly trips (msts remainder)"
    ) +
    facet_wrap(~ month, ncol = 4, labeller = "label_both") +
    theme_bw()

ggsave(
    plot = remainder_plot_month,
    filename = "./plots/time_series/remainder_plot_month_t-window=6wk.png",
    width = 10,
    height = 3,
    dpi = 250,
    scale = 1.25
)

#-----------------------------------------------------------#
# Seasonally adjusted data ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# faceted by hour ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

seasadj_hour_coefs <- 
    trips_weather_data %>% 
    as_tibble() %>%
    group_by(hour) %>% 
    do(lm(seasadj_trips ~ seasadj_temperature, data = .) %>% tidy(.)) %>% 
    filter(str_detect(term, "seasadj_temperature")) %>% 
    select(hour, estimate)

seasadj_plot_hour <- 
    trips_weather_data %>% 
    ggplot(aes(seasadj_temperature, seasadj_trips)) + 
    geom_point(shape = 1, size = 1) + 
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    geom_label(
        data = seasadj_hour_coefs,
        aes(
            label = paste("b =", round(estimate, 0)),
            x = 2.5,
            y = 4000
        ),
        color = "red2",
        label.size = NA,
        alpha = .5
    ) +
    labs(
        x = "Average hourly temperature (msts seasonal adjustment)",
        y = "Average hourly trips (msts seasonal adjustment)"
    ) +
    facet_wrap(~ hour, ncol = 6, labeller = "label_both") +
    theme_bw()

ggsave(
    plot = seasadj_plot_hour,
    filename = "./plots/time_series/seasadj_plot_hour_t-window=6wk.png",
    width = 10,
    height = 6,
    dpi = 250,
    scale = 1.25
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# faceted by weekday ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

seasadj_wday_coefs <- 
    trips_weather_data %>% 
    as_tibble() %>%
    group_by(wday) %>% 
    do(lm(seasadj_trips ~ seasadj_temperature, data = .) %>% tidy(.)) %>% 
    filter(str_detect(term, "seasadj_temperature")) %>% 
    select(wday, estimate)

seasadj_plot_wday <- 
    trips_weather_data %>% 
    ggplot(aes(seasadj_temperature, seasadj_trips)) + 
    geom_point(shape = 1, size = 1) + 
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    geom_label(
        data = seasadj_wday_coefs,
        aes(
            label = paste("b =", round(estimate, 0)),
            x = 0,
            y = -4000
        ),
        color = "red2",
        label.size = NA,
        alpha = .5
    ) +
    labs(
        x = "Average hourly temperature (msts seasonal adjustment)",
        y = "Average hourly trips (msts seasonal adjustment)"
    ) +
    facet_wrap(~ wday, ncol = 4, labeller = "label_both") +
    theme_bw()

ggsave(
    plot = seasadj_plot_wday,
    filename = "./plots/time_series/seasadj_plot_wday_t-window=6wk.png",
    width = 10,
    height = 6,
    dpi = 250,
    scale = 1.25
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# faceted by month ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

seasadj_month_coefs <- 
    trips_weather_data %>% 
    as_tibble() %>%
    group_by(month) %>% 
    do(lm(seasadj_trips ~ seasadj_temperature, data = .) %>% tidy(.)) %>% 
    filter(str_detect(term, "seasadj_temperature")) %>% 
    select(month, estimate)

seasadj_plot_month <- 
    trips_weather_data %>% 
    ggplot(aes(seasadj_temperature, seasadj_trips)) + 
    geom_point(shape = 1, size = 1) + 
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    geom_label(
        data = seasadj_month_coefs,
        aes(
            label = paste("b =", round(estimate, 0)),
            x = 0,
            y = 5000
        ),
        color = "red2",
        label.size = NA,
        alpha = .5
    ) +
    labs(
        x = "Average hourly temperature (msts seasonal adjustment)",
        y = "Average hourly trips (msts seasonal adjustment)"
    ) +
    facet_wrap(~ month, ncol = 4, labeller = "label_both") +
    theme_bw()

ggsave(
    plot = seasadj_plot_month,
    filename = "./plots/time_series/seasadj_plot_month_t-window=6wk.png",
    width = 10,
    height = 3,
    dpi = 250,
    scale = 1.25
)


#===========================================================#
# Grouping by weekday vs. weekend ----
#===========================================================#

#-----------------------------------------------------------#
# Remainder ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# faceted by hour ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trips_weather_data_2 <- 
    trips_weather_data %>% 
    mutate(
        weekend = case_when(
            wday %in% c("Mon", "Tue", "Wed", "Thu", "Fri") ~ "Weekday",
            wday %in% c("Sun", "Sat") ~ "Weekend"
        )
    )

remainder_plot_hour_wday <- 
    trips_weather_data_2 %>% 
    ggplot(aes(remainder_temperature, remainder_trips)) + 
    geom_point(shape = 1, size = 1) + 
    geom_smooth(aes(color = weekend), method = "lm", se = TRUE) +
    labs(
        x = "Average hourly temperature (msts remainder)",
        y = "Average hourly trips (msts remainder)"
    ) +
    facet_wrap(~ hour, ncol = 6, labeller = "label_both", scales = "fixed") +
    scale_color_manual(values = c("red", "blue")) +
    theme_bw()

ggsave(
    plot = remainder_plot_hour_wday,
    filename = "./plots/time_series/remainder_plot_hour_weekend_t-window=6wk_fixed.png",
    width = 10,
    height = 5,
    dpi = 250,
    scale = 1.5
)

##################################################################
##################################################################

# Seasonally adjusted data shows that the trend swamps the true
#   relationship between temperature and trips
# 
# Looking at the remainder, which has both seasonality and trend
#   removed, shows the relationship.
# 
# Further breaking this down by hour shows that the relationship
#   varies by hour, even after removing daily seasonality and the
#   overall trend from both the trips and temperature series

##################################################################
##################################################################




#===========================================================#
# Mixed effects models ----
#===========================================================#


trwx_data <-
    trips_weather_data %>% 
    select(
        .rownames,
        date_time_hourly,
        year,
        month,
        day,
        hour,
        wday,
        trips,
        trips_msts,
        temperature_c,
        humidity_c,
        wind_speed_c,
        dew_point_c,
        dew_point_c,
        hourly_uv_index_c,
        daily_uv_index_c,
        daily_ozone_c,
        trend1,
        trend24,
        trend168)


trwx_data_complete <-
    trwx_data %>% 
    filter(complete.cases(.))


#-----------------------------------------------------------#
# Intercepts for multiple seasonalities ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_0 <- 
    lmer(
        
        trips ~ 
            1 + 
            (1 | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_0) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_0), "./output/trwx_fit_0_tidy.csv")

glance(trwx_fit_0)

# merTools::plotREsim(merTools::REsim(trwx_fit_0), labs = TRUE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_0)$trips, plot = FALSE, lag.max = 36))

trwx_fit_0_acf <- tidy((acf(residuals(trwx_fit_0), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_0_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_0_df <-
    augment(trwx_fit_0) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_0_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_0_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_0_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_0_plot <- 
    trwx_fit_0_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_0_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_0_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_0)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_0_plot.png",
    plot = trwx_fit_0_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_0_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_0_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding trend ----
#-----------------------------------------------------------#

# The trend represents the underlying increase in level across the time series. The 
#   coefficient for the trend term is the increase in level for each unit increase
#   in the trend variable. `trend168` is scaled so that 1 unit is 168 hours (i.e., 
#   a week), so the interpretation is how much the outcome level increases on average
#   for each week.
# 
# Because the trend is constrained to be linear, the slope of the trend never changes.
# 
# When trend is a fixed effect only, all discrete time intervals contribute equally to
#   the estimate of the trend. So, for example, any increase in level seen across Saturdays 
#   is weighted equally compared to an increase seen across Mondays. Likewise, any increase
#   seen across the 8AM hour is weighted equally compared to the 12PM hour.
# 
# When trend is allowed to be a random effect, the contribution of each interval group is first
#   estimated independently, then its contribution to the overall trend is weighted by
#   the group estimate's standard error. This means that the trend is estimated by a 
#   weighted averaging, across time groupings, of the level changes seen within each grouping
# 
# Because there are an equal number of hours in each discrete time grouping, differences
#   in standard errors will be due to differences in variance, not differences in sample size.
#   This means that time groupings with big changes in the level across time will contribute
#   less to the level estimate. 
# 
# If the time groupings are meaningfully different in terms of the data generating process,
#   then estimating the trend by averaging across them, allowing each process to
#   contribute on its own terms, is a valid way of estimating the trend.
# 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_1.1 <- 
    lmer(
        
        trips ~ 
            # trend1 +
            trend24 +
            # trend168 + 
            (1 | wday:hour),
        
        REML = FALSE,
        data = trwx_data_complete
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_1.1) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_1.1), "./output/trwx_fit_1.1_tidy.csv")

glance(trwx_fit_1.1)

# merTools::plotREsim(
#     merTools::REsim(trwx_fit_0),
#     labs = TRUE,
#     facet = list(groupFctr = "wday:hour", term = "(Intercept)")
# )
# merTools::plotREsim(merTools::REsim(trwx_fit_1.1), labs = TRUE)

anova(trwx_fit_0, trwx_fit_1.1)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_1.1)$trips, plot = FALSE, lag.max = 36))

trwx_fit_1.1_acf <- tidy((acf(residuals(trwx_fit_1.1), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_1.1_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_1.1_df <-
    augment(trwx_fit_1.1) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_1.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_1.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_1.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_1.1_plot <- 
    trwx_fit_1.1_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_1.1_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_1.1_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_1.1)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_1.1_plot.png",
    plot = trwx_fit_1.1_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_1.1_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_1.1_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_1.2 <- 
    lmer(
        
        trips ~ 
            # trend1 +
            trend24 +
            # trend168 + 
            # (trend1 | wday:hour),
            (trend24 | wday:hour),
            # (trend168 | wday:hour),
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_1.2) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_1.2), "./output/trwx_fit_1.2_tidy.csv")

glance(trwx_fit_1.2)

# merTools::plotREsim(merTools::REsim(trwx_fit_1.2), labs = TRUE)

anova(trwx_fit_0, trwx_fit_1.2)
anova(trwx_fit_1.1, trwx_fit_1.2)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_1.2)$trips, plot = FALSE, lag.max = 36))

trwx_fit_1.2_acf <- tidy((acf(residuals(trwx_fit_1.2), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_1.2_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_1.2_df <-
    augment(trwx_fit_1.2) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_1.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_1.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_1.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_1.2_plot <- 
    trwx_fit_1.2_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_1.2_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_1.2_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_1.2)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_1.2_plot.png",
    plot = trwx_fit_1.2_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_1.2_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_1.2_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding temperature ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_2.1 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 + 
            # trend168 + 
            average_temperature_c + 
            (1 | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_2.1) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_2.1), "./output/trwx_fit_2.1_tidy.csv")

glance(trwx_fit_2.1)

# merTools::plotREsim(merTools::REsim(trwx_fit_2.1), labs = TRUE)

## Nested ##

anova(trwx_fit_0, trwx_fit_2.1)
anova(trwx_fit_1.1, trwx_fit_2.1)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_2.1)$trips, plot = FALSE))

trwx_fit_2.1_acf <- tidy((acf(residuals(trwx_fit_2.1), plot = FALSE)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_2.1_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_2.1_df <-
    augment(trwx_fit_2.1) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_2.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_2.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_2.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_2.1_plot <- 
    trwx_fit_2.1_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_2.1_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_2.1_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_2.1)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_2.1_plot.png",
    plot = trwx_fit_2.1_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_2.1_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_2.1_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_2.2 <- 
    lmer(
        
        trips ~ 
            # trend1 +
            trend24 +
            # trend168 +
            average_temperature_c + 
            # (trend1 | wday:hour),
            (trend24 | wday:hour),
            # (trend168 | wday:hour),
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_2.2) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_2.2), "./output/trwx_fit_2.2_tidy.csv")

glance(trwx_fit_2.2)

# merTools::plotREsim(merTools::REsim(trwx_fit_2.2), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_2.2)
anova(trwx_fit_1.2, trwx_fit_2.2)
anova(trwx_fit_2.1, trwx_fit_2.2)

## Non-Nested ##



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_2.2)$trips, plot = FALSE))

trwx_fit_2.2_acf <- tidy((acf(residuals(trwx_fit_2.2), plot = FALSE)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_2.2_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_2.2_df <-
    augment(trwx_fit_2.2) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_2.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_2.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_2.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_2.2_plot <- 
    trwx_fit_2.2_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_2.2_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_2.2_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_2.2)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_2.2_plot.png",
    plot = trwx_fit_2.2_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_2.2_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_2.2_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_2.3 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            (average_temperature_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_2.3) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_2.3), "./output/trwx_fit_2.3_tidy.csv")

glance(trwx_fit_2.3)

# merTools::plotREsim(merTools::REsim(trwx_fit_2.3), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_2.3)
anova(trwx_fit_1.2, trwx_fit_2.3)
anova(trwx_fit_2.1, trwx_fit_2.3)

## Non-Nested ##

anova(trwx_fit_2.2, trwx_fit_2.3)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_2.3)$trips, plot = FALSE))

trwx_fit_2.3_acf <- tidy((acf(residuals(trwx_fit_2.3), plot = FALSE)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_2.3_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_2.3_df <-
    augment(trwx_fit_2.3) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_2.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_2.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_2.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_2.3_plot <- 
    trwx_fit_2.3_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_2.3_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_2.3_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_2.3)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_2.3_plot.png",
    plot = trwx_fit_2.3_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_2.3_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_2.3_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_2.4 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 + 
            # trend168 + 
            average_temperature_c + 
            # (trend1 + average_temperature_c | wday:hour), 
            (trend24 + average_temperature_c | wday:hour), 
            # (trend168 + average_temperature_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_2.4) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_2.4), "./output/trwx_fit_2.4_tidy.csv")

glance(trwx_fit_2.4)

# merTools::plotREsim(merTools::REsim(trwx_fit_2.4), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_2.4)
anova(trwx_fit_1.2, trwx_fit_2.4)
anova(trwx_fit_2.1, trwx_fit_2.4)
anova(trwx_fit_2.3, trwx_fit_2.4)

## Non-Nested ##

anova(trwx_fit_2.2, trwx_fit_2.4)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_2.4)$trips, plot = FALSE))

trwx_fit_2.4_acf <- tidy((acf(residuals(trwx_fit_2.4), plot = FALSE)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_2.4_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_2.4_df <-
    augment(trwx_fit_2.4) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_2.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_2.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_2.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_2.4_plot <- 
    trwx_fit_2.4_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_2.4_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_2.4_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_2.4)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_2.4_plot.png",
    plot = trwx_fit_2.4_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_2.4_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_2.4_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding interaction between trend and temperature ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_3.1 <- 
    lmer(
        
        trips ~ 
            # trend1 * 
            trend24 *
            # trend168 * 
            average_temperature_c + 
            (1 | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_3.1) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_3.1), "./output/trwx_fit_3.1_tidy.csv")

glance(trwx_fit_3.1)

# merTools::plotREsim(merTools::REsim(trwx_fit_3.1), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_3.1)
anova(trwx_fit_1, trwx_fit_3.1)
anova(trwx_fit_2.1, trwx_fit_3.1)

## Non-Nested ##



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_3.1)$trips, plot = FALSE))

trwx_fit_3.1_acf <- tidy((acf(residuals(trwx_fit_3.1), plot = FALSE)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_3.1_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_3.1_df <-
    augment(trwx_fit_3.1) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_3.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_3.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_3.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_3.1_plot <- 
    trwx_fit_3.1_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_3.1_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_3.1_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_3.1)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_3.1_plot.png",
    plot = trwx_fit_3.1_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_3.1_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_3.1_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding interaction between trend and temperature ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_3.2 <- 
    lmer(
        
        trips ~ 
            # trend1 * 
            trend24 * 
            # trend168 * 
            average_temperature_c + 
            # (trend1 | wday:hour), 
            (trend24 | wday:hour), 
            # (trend168 | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_3.2) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_3.2), "./output/trwx_fit_3.2_tidy.csv")

glance(trwx_fit_3.2)

# merTools::plotREsim(merTools::REsim(trwx_fit_3.2), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_3.2)
anova(trwx_fit_1, trwx_fit_3.2)
anova(trwx_fit_2.1, trwx_fit_3.2)

## Non-Nested ##



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_3.2)$trips, plot = FALSE))

trwx_fit_3.2_acf <- tidy((acf(residuals(trwx_fit_3.2), plot = FALSE)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_3.2_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_3.2_df <-
    augment(trwx_fit_3.2) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_3.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_3.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_3.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_3.2_plot <- 
    trwx_fit_3.2_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_3.2_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_3.2_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_3.2)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_3.2_plot.png",
    plot = trwx_fit_3.2_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_3.2_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_3.2_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding interaction between trend and temperature ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_3.3 <- 
    lmer(
        
        trips ~ 
            # trend1 * 
            trend24 * 
            # trend168 * 
            average_temperature_c + 
            (average_temperature_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_3.3) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_3.3), "./output/trwx_fit_3.3_tidy.csv")

glance(trwx_fit_3.3)

# merTools::plotREsim(merTools::REsim(trwx_fit_3.3), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_3.3)
anova(trwx_fit_1, trwx_fit_3.3)
anova(trwx_fit_2.1, trwx_fit_3.3)

## Non-Nested ##



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_3.3)$trips, plot = FALSE))

trwx_fit_3.3_acf <- tidy((acf(residuals(trwx_fit_3.3), plot = FALSE)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_3.3_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_3.3_df <-
    augment(trwx_fit_3.3) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_3.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_3.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_3.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_3.3_plot <- 
    trwx_fit_3.3_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_3.3_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_3.3_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_3.3)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_3.3_plot.png",
    plot = trwx_fit_3.3_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_3.3_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_3.3_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding interaction between trend and temperature ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_3.4 <- 
    lmer(
        
        trips ~ 
            # trend1 * 
            trend24 *
            # trend168 * 
            average_temperature_c + 
            # (trend1 * average_temperature_c | wday:hour), 
            (trend24 * average_temperature_c | wday:hour), 
            # (trend168 * average_temperature_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_3.4) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_3.4), "./output/trwx_fit_3.4_tidy.csv")

glance(trwx_fit_3.4)

# merTools::plotREsim(merTools::REsim(trwx_fit_3.4), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_3.4)
anova(trwx_fit_1, trwx_fit_3.4)
anova(trwx_fit_2.1, trwx_fit_3.4)

## Non-Nested ##



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_3.4)$trips, plot = FALSE))

trwx_fit_3.4_acf <- tidy((acf(residuals(trwx_fit_3.4), plot = FALSE)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_3.4_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_3.4_df <-
    augment(trwx_fit_3.4) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_3.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_3.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_3.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_3.4_plot <- 
    trwx_fit_3.4_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_3.4_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_3.4_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_3.4)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_3.4_plot.png",
    plot = trwx_fit_3.4_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_3.4_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_3.4_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Switching out temp for humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_4.1 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 + 
            # trend168 + 
            average_humidity_c + 
            (1 | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_4.1) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_4.1), "./output/trwx_fit_4.1_tidy.csv")

glance(trwx_fit_4.1)

# # merTools::plotREsim(merTools::REsim(trwx_fit_4.1), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_4.1)
anova(trwx_fit_1, trwx_fit_4.1)
anova(trwx_fit_2.1, trwx_fit_4.1)

## Non-Nested ##



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_4.1)$trips, plot = FALSE))

trwx_fit_4.1_acf <- tidy((acf(residuals(trwx_fit_4.1), plot = FALSE)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_4.1_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_4.1_df <-
    augment(trwx_fit_4.1) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_4.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_4.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_4.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_4.1_plot <- 
    trwx_fit_4.1_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_4.1_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_4.1_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_4.1)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_4.1_plot.png",
    plot = trwx_fit_4.1_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_4.1_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_4.1_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding interaction between trend and temperature ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_4.2 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 + 
            # trend168 + 
            average_humidity_c + 
            # (trend1 | wday:hour), 
            (trend24 | wday:hour), 
            # (trend168 | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_4.2) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_4.2), "./output/trwx_fit_4.2_tidy.csv")

glance(trwx_fit_4.2)

# merTools::plotREsim(merTools::REsim(trwx_fit_4.2), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_4.2)
anova(trwx_fit_1, trwx_fit_4.2)
anova(trwx_fit_2.1, trwx_fit_4.2)

## Non-Nested ##



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_4.2)$trips, plot = FALSE))

trwx_fit_4.2_acf <- tidy((acf(residuals(trwx_fit_4.2), plot = FALSE)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_4.2_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_4.2_df <-
    augment(trwx_fit_4.2) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_4.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_4.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_4.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_4.2_plot <- 
    trwx_fit_4.2_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_4.2_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_4.2_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_4.2)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_4.2_plot.png",
    plot = trwx_fit_4.2_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_4.2_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_4.2_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding interaction between trend and temperature ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_4.3 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_humidity_c + 
            (average_humidity_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_4.3) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_4.3), "./output/trwx_fit_4.3_tidy.csv")

glance(trwx_fit_4.3)

# merTools::plotREsim(merTools::REsim(trwx_fit_4.3), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_4.3)
anova(trwx_fit_1, trwx_fit_4.3)
anova(trwx_fit_2.1, trwx_fit_4.3)

## Non-Nested ##



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_4.3)$trips, plot = FALSE))

trwx_fit_4.3_acf <- tidy((acf(residuals(trwx_fit_4.3), plot = FALSE)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_4.3_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_4.3_df <-
    augment(trwx_fit_4.3) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_4.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_4.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_4.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_4.3_plot <- 
    trwx_fit_4.3_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_4.3_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_4.3_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_4.3)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_4.3_plot.png",
    plot = trwx_fit_4.3_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_4.3_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_4.3_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding interaction between trend and temperature ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_4.4 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 + 
            # trend168 + 
            average_humidity_c + 
            # (trend1 + average_humidity_c | wday:hour), 
            (trend24 + average_humidity_c | wday:hour),
            # (trend168 + average_humidity_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_4.4) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_4.4), "./output/trwx_fit_4.4_tidy.csv")

glance(trwx_fit_4.4)

# merTools::plotREsim(merTools::REsim(trwx_fit_4.4), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_4.4)
anova(trwx_fit_1, trwx_fit_4.4)
anova(trwx_fit_2.1, trwx_fit_4.4)

## Non-Nested ##



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_4.4)$trips, plot = FALSE))

trwx_fit_4.4_acf <- tidy((acf(residuals(trwx_fit_4.4), plot = FALSE)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_4.4_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_4.4_df <-
    augment(trwx_fit_4.4) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_4.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_4.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_4.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_4.4_plot <- 
    trwx_fit_4.4_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_4.4_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_4.4_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_4.4)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_4.4_plot.png",
    plot = trwx_fit_4.4_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_4.4_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_4.4_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Switching out temp for humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_5.1 <- 
    lmer(
        
        trips ~ 
            # trend1 * 
            trend24 *
            # trend168 * 
            average_humidity_c + 
            (1 | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_5.1) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_5.1), "./output/trwx_fit_5.1_tidy.csv")

glance(trwx_fit_5.1)

# merTools::plotREsim(merTools::REsim(trwx_fit_5.1), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_5.1)
anova(trwx_fit_1, trwx_fit_5.1)
anova(trwx_fit_2.1, trwx_fit_5.1)

## Non-Nested ##



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_5.1)$trips, plot = FALSE))

trwx_fit_5.1_acf <- tidy((acf(residuals(trwx_fit_5.1), plot = FALSE)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_5.1_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_5.1_df <-
    augment(trwx_fit_5.1) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_5.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_5.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_5.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_5.1_plot <- 
    trwx_fit_5.1_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_5.1_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_5.1_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_5.1)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_5.1_plot.png",
    plot = trwx_fit_5.1_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_5.1_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_5.1_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding interaction between trend and temperature ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_5.2 <- 
    lmer(
        
        trips ~ 
            # trend1 * 
            trend24 *
            # trend168 * 
            average_humidity_c + 
            # (trend1 | wday:hour), 
            (trend24 | wday:hour),
            # (trend168 | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_5.2) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_5.2), "./output/trwx_fit_5.2_tidy.csv")

glance(trwx_fit_5.2)

# merTools::plotREsim(merTools::REsim(trwx_fit_5.2), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_5.2)
anova(trwx_fit_1, trwx_fit_5.2)
anova(trwx_fit_2.1, trwx_fit_5.2)

## Non-Nested ##



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_5.2)$trips, plot = FALSE))

trwx_fit_5.2_acf <- tidy((acf(residuals(trwx_fit_5.2), plot = FALSE)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_5.2_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_5.2_df <-
    augment(trwx_fit_5.2) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_5.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_5.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_5.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_5.2_plot <- 
    trwx_fit_5.2_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_5.2_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_5.2_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_5.2)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_5.2_plot.png",
    plot = trwx_fit_5.2_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_5.2_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_5.2_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding interaction between trend and temperature ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_5.3 <- 
    lmer(
        
        trips ~ 
            # trend1 * 
            trend24 *
            # trend168 * 
            average_humidity_c + 
            (average_humidity_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_5.3) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_5.3), "./output/trwx_fit_5.3_tidy.csv")

glance(trwx_fit_5.3)

# merTools::plotREsim(merTools::REsim(trwx_fit_5.3), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_5.3)
anova(trwx_fit_1, trwx_fit_5.3)
anova(trwx_fit_2.1, trwx_fit_5.3)

## Non-Nested ##



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_5.3)$trips, plot = FALSE))

trwx_fit_5.3_acf <- tidy((acf(residuals(trwx_fit_5.3), plot = FALSE)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_5.3_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_5.3_df <-
    augment(trwx_fit_5.3) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_5.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_5.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_5.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_5.3_plot <- 
    trwx_fit_5.3_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_5.3_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_5.3_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_5.3)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_5.3_plot.png",
    plot = trwx_fit_5.3_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_5.3_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_5.3_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding interaction between trend and temperature ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_5.4 <- 
    lmer(
        
        trips ~ 
            # trend1 * 
            trend24 *
            # trend168 * 
            average_humidity_c + 
            # (trend1 * average_humidity_c | wday:hour), 
            (trend24 * average_humidity_c | wday:hour),
            # (trend168 * average_humidity_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_5.4) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_5.4), "./output/trwx_fit_5.4_tidy.csv")

glance(trwx_fit_5.4)

# merTools::plotREsim(merTools::REsim(trwx_fit_5.4), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_5.4)
anova(trwx_fit_1, trwx_fit_5.4)
anova(trwx_fit_2.1, trwx_fit_5.4)

## Non-Nested ##



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_5.4)$trips, plot = FALSE))

trwx_fit_5.4_acf <- tidy((acf(residuals(trwx_fit_5.4), plot = FALSE)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_5.4_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_5.4_df <-
    augment(trwx_fit_5.4) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_5.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_5.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_5.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_5.4_plot <- 
    trwx_fit_5.4_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_5.4_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_5.4_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_5.4)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_5.4_plot.png",
    plot = trwx_fit_5.4_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_5.4_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_5.4_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_6.1 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            (1 | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_6.1) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_6.1), "./output/trwx_fit_6.1_tidy.csv")

glance(trwx_fit_6.1)

# merTools::plotREsim(merTools::REsim(trwx_fit_6.1), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_6.1)
anova(trwx_fit_1, trwx_fit_6.1)
anova(trwx_fit_2, trwx_fit_6.1)
anova(trwx_fit_3, trwx_fit_6.1)
anova(trwx_fit_4, trwx_fit_6.1)
anova(trwx_fit_5, trwx_fit_6.1)
anova(trwx_fit_6, trwx_fit_6.1)
anova(trwx_fit_7, trwx_fit_6.1)
anova(trwx_fit_8, trwx_fit_6.1)
anova(trwx_fit_9, trwx_fit_6.1)
anova(trwx_fit_10, trwx_fit_6.1)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_6.1)$trips, plot = FALSE, lag.max = 36))

trwx_fit_6.1_acf <- tidy((acf(residuals(trwx_fit_6.1), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_6.1_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_6.1_df <-
    augment(trwx_fit_6.1) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_6.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_6.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_6.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_6.1_plot <- 
    trwx_fit_6.1_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_6.1_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_6.1_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_6.1)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_6.1_plot.png",
    plot = trwx_fit_6.1_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_6.1_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_6.1_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_6.2 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            # (trend1 | wday:hour), 
            (trend24 | wday:hour),
            # (trend168 | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_6.2) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_6.2), "./output/trwx_fit_6.2_tidy.csv")

glance(trwx_fit_6.2)

# merTools::plotREsim(merTools::REsim(trwx_fit_6.2), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_6.2)
anova(trwx_fit_1, trwx_fit_6.2)
anova(trwx_fit_2, trwx_fit_6.2)
anova(trwx_fit_3, trwx_fit_6.2)
anova(trwx_fit_4, trwx_fit_6.2)
anova(trwx_fit_5, trwx_fit_6.2)
anova(trwx_fit_6, trwx_fit_6.2)
anova(trwx_fit_7, trwx_fit_6.2)
anova(trwx_fit_8, trwx_fit_6.2)
anova(trwx_fit_9, trwx_fit_6.2)
anova(trwx_fit_10, trwx_fit_6.2)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_6.2)$trips, plot = FALSE, lag.max = 36))

trwx_fit_6.2_acf <- tidy((acf(residuals(trwx_fit_6.2), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_6.2_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_6.2_df <-
    augment(trwx_fit_6.2) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_6.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_6.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_6.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_6.2_plot <- 
    trwx_fit_6.2_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_6.2_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_6.2_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_6.2)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_6.2_plot.png",
    plot = trwx_fit_6.2_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_6.2_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_6.2_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_6.3 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 + 
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            (average_temperature_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_6.3) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_6.3), "./output/trwx_fit_6.3_tidy.csv")

glance(trwx_fit_6.3)

# merTools::plotREsim(merTools::REsim(trwx_fit_6.3), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_6.3)
anova(trwx_fit_1, trwx_fit_6.3)
anova(trwx_fit_2, trwx_fit_6.3)
anova(trwx_fit_3, trwx_fit_6.3)
anova(trwx_fit_4, trwx_fit_6.3)
anova(trwx_fit_5, trwx_fit_6.3)
anova(trwx_fit_6, trwx_fit_6.3)
anova(trwx_fit_7, trwx_fit_6.3)
anova(trwx_fit_8, trwx_fit_6.3)
anova(trwx_fit_9, trwx_fit_6.3)
anova(trwx_fit_10, trwx_fit_6.3)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_6.3)$trips, plot = FALSE, lag.max = 36))

trwx_fit_6.3_acf <- tidy((acf(residuals(trwx_fit_6.3), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_6.3_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_6.3_df <-
    augment(trwx_fit_6.3) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_6.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_6.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_6.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_6.3_plot <- 
    trwx_fit_6.3_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_6.3_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_6.3_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_6.3)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_6.3_plot.png",
    plot = trwx_fit_6.3_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_6.3_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_6.3_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_6.4 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            (average_humidity_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_6.4) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_6.4), "./output/trwx_fit_6.4_tidy.csv")

glance(trwx_fit_6.4)

# merTools::plotREsim(merTools::REsim(trwx_fit_6.4), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_6.4)
anova(trwx_fit_1, trwx_fit_6.4)
anova(trwx_fit_2, trwx_fit_6.4)
anova(trwx_fit_3, trwx_fit_6.4)
anova(trwx_fit_4, trwx_fit_6.4)
anova(trwx_fit_5, trwx_fit_6.4)
anova(trwx_fit_6, trwx_fit_6.4)
anova(trwx_fit_7, trwx_fit_6.4)
anova(trwx_fit_8, trwx_fit_6.4)
anova(trwx_fit_9, trwx_fit_6.4)
anova(trwx_fit_10, trwx_fit_6.4)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_6.4)$trips, plot = FALSE, lag.max = 36))

trwx_fit_6.4_acf <- tidy((acf(residuals(trwx_fit_6.4), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_6.4_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_6.4_df <-
    augment(trwx_fit_6.4) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_6.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_6.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_6.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_6.4_plot <- 
    trwx_fit_6.4_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_6.4_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_6.4_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_6.4)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_6.4_plot.png",
    plot = trwx_fit_6.4_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_6.4_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_6.4_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_6.5 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            # (trend1 + average_temperature_c + average_humidity_c | wday:hour), 
            (trend24 + average_temperature_c + average_humidity_c | wday:hour),
            # (trend168 + average_temperature_c + average_humidity_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_6.5) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_6.5), "./output/trwx_fit_6.5_tidy.csv")

glance(trwx_fit_6.5)

# merTools::plotREsim(merTools::REsim(trwx_fit_6.5), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_6.5)
anova(trwx_fit_1, trwx_fit_6.5)
anova(trwx_fit_2, trwx_fit_6.5)
anova(trwx_fit_3, trwx_fit_6.5)
anova(trwx_fit_4, trwx_fit_6.5)
anova(trwx_fit_5, trwx_fit_6.5)
anova(trwx_fit_6, trwx_fit_6.5)
anova(trwx_fit_7, trwx_fit_6.5)
anova(trwx_fit_8, trwx_fit_6.5)
anova(trwx_fit_9, trwx_fit_6.5)
anova(trwx_fit_10, trwx_fit_6.5)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_6.5)$trips, plot = FALSE, lag.max = 36))

trwx_fit_6.5_acf <- tidy((acf(residuals(trwx_fit_6.5), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_6.5_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_6.5_df <-
    augment(trwx_fit_6.5) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_6.5_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_6.5_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_6.5_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_6.5_plot <- 
    trwx_fit_6.5_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_6.5_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_6.5_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_6.5)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_6.5_plot.png",
    plot = trwx_fit_6.5_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_6.5_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_6.5_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_7.1 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            # trend1:average_temperature_c + 
            trend24:average_temperature_c +
            # trend168:average_temperature_c + 
            (1 | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_7.1) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_7.1), "./output/trwx_fit_7.1_tidy.csv")

glance(trwx_fit_7.1)

# merTools::plotREsim(merTools::REsim(trwx_fit_7.1), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_7.1)
anova(trwx_fit_1, trwx_fit_7.1)
anova(trwx_fit_2, trwx_fit_7.1)
anova(trwx_fit_3, trwx_fit_7.1)
anova(trwx_fit_4, trwx_fit_7.1)
anova(trwx_fit_5, trwx_fit_7.1)
anova(trwx_fit_6, trwx_fit_7.1)
anova(trwx_fit_7, trwx_fit_7.1)
anova(trwx_fit_8, trwx_fit_7.1)
anova(trwx_fit_9, trwx_fit_7.1)
anova(trwx_fit_10, trwx_fit_7.1)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_7.1)$trips, plot = FALSE, lag.max = 36))

trwx_fit_7.1_acf <- tidy((acf(residuals(trwx_fit_7.1), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_7.1_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_7.1_df <-
    augment(trwx_fit_7.1) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_7.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_7.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_7.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_7.1_plot <- 
    trwx_fit_7.1_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_7.1_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_7.1_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_7.1)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_7.1_plot.png",
    plot = trwx_fit_7.1_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_7.1_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_7.1_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_7.2 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            # trend1:average_temperature_c + 
            trend24:average_temperature_c +
            # trend168:average_temperature_c + 
            # (trend1 | wday:hour), 
            (trend24 | wday:hour),
            # (trend168 | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_7.2) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_7.2), "./output/trwx_fit_7.2_tidy.csv")

glance(trwx_fit_7.2)

# merTools::plotREsim(merTools::REsim(trwx_fit_7.2), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_7.2)
anova(trwx_fit_1, trwx_fit_7.2)
anova(trwx_fit_2, trwx_fit_7.2)
anova(trwx_fit_3, trwx_fit_7.2)
anova(trwx_fit_4, trwx_fit_7.2)
anova(trwx_fit_5, trwx_fit_7.2)
anova(trwx_fit_6, trwx_fit_7.2)
anova(trwx_fit_7, trwx_fit_7.2)
anova(trwx_fit_8, trwx_fit_7.2)
anova(trwx_fit_9, trwx_fit_7.2)
anova(trwx_fit_10, trwx_fit_7.2)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_7.2)$trips, plot = FALSE, lag.max = 36))

trwx_fit_7.2_acf <- tidy((acf(residuals(trwx_fit_7.2), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_7.2_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_7.2_df <-
    augment(trwx_fit_7.2) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_7.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_7.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_7.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_7.2_plot <- 
    trwx_fit_7.2_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_7.2_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_7.2_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_7.2)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_7.2_plot.png",
    plot = trwx_fit_7.2_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_7.2_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_7.2_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_7.3 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            # trend1:average_temperature_c + 
            trend24:average_temperature_c +
            # trend168:average_temperature_c + 
            (average_temperature_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_7.3) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_7.3), "./output/trwx_fit_7.3_tidy.csv")

glance(trwx_fit_7.3)

# merTools::plotREsim(merTools::REsim(trwx_fit_7.3), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_7.3)
anova(trwx_fit_1, trwx_fit_7.3)
anova(trwx_fit_2, trwx_fit_7.3)
anova(trwx_fit_3, trwx_fit_7.3)
anova(trwx_fit_4, trwx_fit_7.3)
anova(trwx_fit_5, trwx_fit_7.3)
anova(trwx_fit_6, trwx_fit_7.3)
anova(trwx_fit_7, trwx_fit_7.3)
anova(trwx_fit_8, trwx_fit_7.3)
anova(trwx_fit_9, trwx_fit_7.3)
anova(trwx_fit_10, trwx_fit_7.3)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_7.3)$trips, plot = FALSE, lag.max = 36))

trwx_fit_7.3_acf <- tidy((acf(residuals(trwx_fit_7.3), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_7.3_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_7.3_df <-
    augment(trwx_fit_7.3) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_7.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_7.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_7.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_7.3_plot <- 
    trwx_fit_7.3_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_7.3_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_7.3_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_7.3)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_7.3_plot.png",
    plot = trwx_fit_7.3_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_7.3_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_7.3_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_7.4 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            # trend1:average_temperature_c + 
            trend24:average_temperature_c +
            # trend168:average_temperature_c + 
            (average_humidity_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_7.4) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_7.4), "./output/trwx_fit_7.4_tidy.csv")

glance(trwx_fit_7.4)

# merTools::plotREsim(merTools::REsim(trwx_fit_7.4), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_7.4)
anova(trwx_fit_1, trwx_fit_7.4)
anova(trwx_fit_2, trwx_fit_7.4)
anova(trwx_fit_3, trwx_fit_7.4)
anova(trwx_fit_4, trwx_fit_7.4)
anova(trwx_fit_5, trwx_fit_7.4)
anova(trwx_fit_6, trwx_fit_7.4)
anova(trwx_fit_7, trwx_fit_7.4)
anova(trwx_fit_8, trwx_fit_7.4)
anova(trwx_fit_9, trwx_fit_7.4)
anova(trwx_fit_10, trwx_fit_7.4)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_7.4)$trips, plot = FALSE, lag.max = 36))

trwx_fit_7.4_acf <- tidy((acf(residuals(trwx_fit_7.4), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_7.4_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_7.4_df <-
    augment(trwx_fit_7.4) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_7.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_7.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_7.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_7.4_plot <- 
    trwx_fit_7.4_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_7.4_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_7.4_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_7.4)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_7.4_plot.png",
    plot = trwx_fit_7.4_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_7.4_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_7.4_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_7.5 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            # trend1:average_temperature_c + 
            trend24:average_temperature_c +
            # trend168:average_temperature_c + 
            # (trend1 + average_temperature_c + average_humidity_c | wday:hour), 
            (trend24 + average_temperature_c + average_humidity_c | wday:hour),
            # (trend168 + average_temperature_c + average_humidity_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_7.5) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_7.5), "./output/trwx_fit_7.5_tidy.csv")

glance(trwx_fit_7.5)

# merTools::plotREsim(merTools::REsim(trwx_fit_7.5), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_7.5)
anova(trwx_fit_1, trwx_fit_7.5)
anova(trwx_fit_2, trwx_fit_7.5)
anova(trwx_fit_3, trwx_fit_7.5)
anova(trwx_fit_4, trwx_fit_7.5)
anova(trwx_fit_5, trwx_fit_7.5)
anova(trwx_fit_6, trwx_fit_7.5)
anova(trwx_fit_7, trwx_fit_7.5)
anova(trwx_fit_8, trwx_fit_7.5)
anova(trwx_fit_9, trwx_fit_7.5)
anova(trwx_fit_10, trwx_fit_7.5)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_7.5)$trips, plot = FALSE, lag.max = 36))

trwx_fit_7.5_acf <- tidy((acf(residuals(trwx_fit_7.5), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_7.5_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_7.5_df <-
    augment(trwx_fit_7.5) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_7.5_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_7.5_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_7.5_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_7.5_plot <- 
    trwx_fit_7.5_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_7.5_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_7.5_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_7.5)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_7.5_plot.png",
    plot = trwx_fit_7.5_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_7.5_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_7.5_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_7.6 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            # trend1:average_temperature_c + 
            trend24:average_temperature_c +
            # trend168:average_temperature_c + 
            (
                # trend1 + 
                trend24 +
                # trend168 + 
                    average_temperature_c + 
                    average_humidity_c + 
                    # trend1:average_temperature_c | wday:hour), 
                    trend24:average_temperature_c | wday:hour),
                    # trend168:average_temperature_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_7.6) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_7.6), "./output/trwx_fit_7.6_tidy.csv")

glance(trwx_fit_7.6)

# merTools::plotREsim(merTools::REsim(trwx_fit_7.6), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_7.6)
anova(trwx_fit_1, trwx_fit_7.6)
anova(trwx_fit_2, trwx_fit_7.6)
anova(trwx_fit_3, trwx_fit_7.6)
anova(trwx_fit_4, trwx_fit_7.6)
anova(trwx_fit_5, trwx_fit_7.6)
anova(trwx_fit_6, trwx_fit_7.6)
anova(trwx_fit_7, trwx_fit_7.6)
anova(trwx_fit_8, trwx_fit_7.6)
anova(trwx_fit_9, trwx_fit_7.6)
anova(trwx_fit_10, trwx_fit_7.6)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_7.6)$trips, plot = FALSE, lag.max = 36))

trwx_fit_7.6_acf <- tidy((acf(residuals(trwx_fit_7.6), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_7.6_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_7.6_df <-
    augment(trwx_fit_7.6) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_7.6_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_7.6_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_7.6_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_7.6_plot <- 
    trwx_fit_7.6_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_7.6_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_7.6_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_7.6)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_7.6_plot.png",
    plot = trwx_fit_7.6_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_7.6_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_7.6_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_8.1 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            # trend1:average_humidity_c + 
            trend24:average_humidity_c +
            # trend168:average_humidity_c + 
            (1 | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_8.1) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_8.1), "./output/trwx_fit_8.1_tidy.csv")

glance(trwx_fit_8.1)

# merTools::plotREsim(merTools::REsim(trwx_fit_8.1), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_8.1)
anova(trwx_fit_1, trwx_fit_8.1)
anova(trwx_fit_2, trwx_fit_8.1)
anova(trwx_fit_3, trwx_fit_8.1)
anova(trwx_fit_4, trwx_fit_8.1)
anova(trwx_fit_5, trwx_fit_8.1)
anova(trwx_fit_6, trwx_fit_8.1)
anova(trwx_fit_7, trwx_fit_8.1)
anova(trwx_fit_8, trwx_fit_8.1)
anova(trwx_fit_9, trwx_fit_8.1)
anova(trwx_fit_10, trwx_fit_8.1)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_8.1)$trips, plot = FALSE, lag.max = 36))

trwx_fit_8.1_acf <- tidy((acf(residuals(trwx_fit_8.1), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_8.1_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_8.1_df <-
    augment(trwx_fit_8.1) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_8.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_8.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_8.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_8.1_plot <- 
    trwx_fit_8.1_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_8.1_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_8.1_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_8.1)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_8.1_plot.png",
    plot = trwx_fit_8.1_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_8.1_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_8.1_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_8.2 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 + 
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            # trend1:average_humidity_c + 
            trend24:average_humidity_c + 
            # trend168:average_humidity_c + 
            # (trend1 | wday:hour), 
            (trend24 | wday:hour), 
            # (trend168 | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
)

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_8.2) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_8.2), "./output/trwx_fit_8.2_tidy.csv")

glance(trwx_fit_8.2)

# merTools::plotREsim(merTools::REsim(trwx_fit_8.2), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_8.2)
anova(trwx_fit_1, trwx_fit_8.2)
anova(trwx_fit_2, trwx_fit_8.2)
anova(trwx_fit_3, trwx_fit_8.2)
anova(trwx_fit_4, trwx_fit_8.2)
anova(trwx_fit_5, trwx_fit_8.2)
anova(trwx_fit_6, trwx_fit_8.2)
anova(trwx_fit_7, trwx_fit_8.2)
anova(trwx_fit_8, trwx_fit_8.2)
anova(trwx_fit_9, trwx_fit_8.2)
anova(trwx_fit_10, trwx_fit_8.2)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_8.2)$trips, plot = FALSE, lag.max = 36))

trwx_fit_8.2_acf <- tidy((acf(residuals(trwx_fit_8.2), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_8.2_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_8.2_df <-
    augment(trwx_fit_8.2) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_8.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_8.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_8.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_8.2_plot <- 
    trwx_fit_8.2_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_8.2_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_8.2_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_8.2)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_8.2_plot.png",
    plot = trwx_fit_8.2_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_8.2_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_8.2_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_8.3 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 + 
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            # trend1:average_humidity_c + 
            trend24:average_humidity_c + 
            # trend168:average_humidity_c + 
            (average_temperature_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_8.3) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_8.3), "./output/trwx_fit_8.3_tidy.csv")

glance(trwx_fit_8.3)

# merTools::plotREsim(merTools::REsim(trwx_fit_8.3), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_8.3)
anova(trwx_fit_1, trwx_fit_8.3)
anova(trwx_fit_2, trwx_fit_8.3)
anova(trwx_fit_3, trwx_fit_8.3)
anova(trwx_fit_4, trwx_fit_8.3)
anova(trwx_fit_5, trwx_fit_8.3)
anova(trwx_fit_6, trwx_fit_8.3)
anova(trwx_fit_7, trwx_fit_8.3)
anova(trwx_fit_8, trwx_fit_8.3)
anova(trwx_fit_9, trwx_fit_8.3)
anova(trwx_fit_10, trwx_fit_8.3)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_8.3)$trips, plot = FALSE, lag.max = 36))

trwx_fit_8.3_acf <- tidy((acf(residuals(trwx_fit_8.3), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_8.3_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_8.3_df <-
    augment(trwx_fit_8.3) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_8.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_8.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_8.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_8.3_plot <- 
    trwx_fit_8.3_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_8.3_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_8.3_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_8.3)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_8.3_plot.png",
    plot = trwx_fit_8.3_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_8.3_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_8.3_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_8.4 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            # trend1:average_humidity_c + 
            trend24:average_humidity_c +
            # trend168:average_humidity_c + 
            (average_humidity_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_8.4) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_8.4), "./output/trwx_fit_8.4_tidy.csv")

glance(trwx_fit_8.4)

# merTools::plotREsim(merTools::REsim(trwx_fit_8.4), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_8.4)
anova(trwx_fit_1, trwx_fit_8.4)
anova(trwx_fit_2, trwx_fit_8.4)
anova(trwx_fit_3, trwx_fit_8.4)
anova(trwx_fit_4, trwx_fit_8.4)
anova(trwx_fit_5, trwx_fit_8.4)
anova(trwx_fit_6, trwx_fit_8.4)
anova(trwx_fit_7, trwx_fit_8.4)
anova(trwx_fit_8, trwx_fit_8.4)
anova(trwx_fit_9, trwx_fit_8.4)
anova(trwx_fit_10, trwx_fit_8.4)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_8.4)$trips, plot = FALSE, lag.max = 36))

trwx_fit_8.4_acf <- tidy((acf(residuals(trwx_fit_8.4), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_8.4_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_8.4_df <-
    augment(trwx_fit_8.4) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_8.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_8.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_8.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_8.4_plot <- 
    trwx_fit_8.4_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_8.4_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_8.4_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_8.4)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_8.4_plot.png",
    plot = trwx_fit_8.4_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_8.4_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_8.4_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_8.5 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            # trend1:average_humidity_c + 
            trend24:average_humidity_c +
            # trend168:average_humidity_c + 
            # (trend1 + average_temperature_c + average_humidity_c | wday:hour), 
            (trend24 + average_temperature_c + average_humidity_c | wday:hour),
            # (trend168 + average_temperature_c + average_humidity_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_8.5) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_8.5), "./output/trwx_fit_8.5_tidy.csv")

glance(trwx_fit_8.5)

# merTools::plotREsim(merTools::REsim(trwx_fit_8.5), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_8.5)
anova(trwx_fit_1, trwx_fit_8.5)
anova(trwx_fit_2, trwx_fit_8.5)
anova(trwx_fit_3, trwx_fit_8.5)
anova(trwx_fit_4, trwx_fit_8.5)
anova(trwx_fit_5, trwx_fit_8.5)
anova(trwx_fit_6, trwx_fit_8.5)
anova(trwx_fit_7, trwx_fit_8.5)
anova(trwx_fit_8, trwx_fit_8.5)
anova(trwx_fit_9, trwx_fit_8.5)
anova(trwx_fit_10, trwx_fit_8.5)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_8.5)$trips, plot = FALSE, lag.max = 36))

trwx_fit_8.5_acf <- tidy((acf(residuals(trwx_fit_8.5), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_8.5_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_8.5_df <-
    augment(trwx_fit_8.5) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_8.5_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_8.5_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_8.5_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_8.5_plot <- 
    trwx_fit_8.5_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_8.5_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_8.5_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_8.5)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_8.5_plot.png",
    plot = trwx_fit_8.5_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_8.5_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_8.5_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_8.6 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            # trend1:average_humidity_c + 
            trend24:average_humidity_c +
            # trend168:average_humidity_c + 
            (
                # trend1 + 
                trend24 +
                # trend168 + 
                    average_temperature_c + 
                    average_humidity_c + 
                    # trend1:average_humidity_c | wday:hour), 
                    trend24:average_humidity_c | wday:hour),
                    # trend168:average_humidity_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_8.6) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_8.6), "./output/trwx_fit_8.6_tidy.csv")

glance(trwx_fit_8.6)

# merTools::plotREsim(merTools::REsim(trwx_fit_8.6), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_8.6)
anova(trwx_fit_1, trwx_fit_8.6)
anova(trwx_fit_2, trwx_fit_8.6)
anova(trwx_fit_3, trwx_fit_8.6)
anova(trwx_fit_4, trwx_fit_8.6)
anova(trwx_fit_5, trwx_fit_8.6)
anova(trwx_fit_6, trwx_fit_8.6)
anova(trwx_fit_7, trwx_fit_8.6)
anova(trwx_fit_8, trwx_fit_8.6)
anova(trwx_fit_9, trwx_fit_8.6)
anova(trwx_fit_10, trwx_fit_8.6)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_8.6)$trips, plot = FALSE, lag.max = 36))

trwx_fit_8.6_acf <- tidy((acf(residuals(trwx_fit_8.6), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_8.6_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_8.6_df <-
    augment(trwx_fit_8.6) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_8.6_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_8.6_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_8.6_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_8.6_plot <- 
    trwx_fit_8.6_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_8.6_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_8.6_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_8.6)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_8.6_plot.png",
    plot = trwx_fit_8.6_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_8.6_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_8.6_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_9.1 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            # trend1:average_temperature_c + 
            trend24:average_temperature_c +
            # trend168:average_temperature_c + 
            # trend1:average_humidity_c + 
            trend24:average_humidity_c +
            # trend168:average_humidity_c + 
            (1 | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_9.1) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_9.1), "./output/trwx_fit_9.1_tidy.csv")

glance(trwx_fit_9.1)

# merTools::plotREsim(merTools::REsim(trwx_fit_9.1), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_9.1)
anova(trwx_fit_1, trwx_fit_9.1)
anova(trwx_fit_2, trwx_fit_9.1)
anova(trwx_fit_3, trwx_fit_9.1)
anova(trwx_fit_4, trwx_fit_9.1)
anova(trwx_fit_5, trwx_fit_9.1)
anova(trwx_fit_6, trwx_fit_9.1)
anova(trwx_fit_7, trwx_fit_9.1)
anova(trwx_fit_8, trwx_fit_9.1)
anova(trwx_fit_9, trwx_fit_9.1)
anova(trwx_fit_10, trwx_fit_9.1)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_9.1)$trips, plot = FALSE, lag.max = 36))

trwx_fit_9.1_acf <- tidy((acf(residuals(trwx_fit_9.1), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_9.1_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_9.1_df <-
    augment(trwx_fit_9.1) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_9.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_9.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_9.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_9.1_plot <- 
    trwx_fit_9.1_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_9.1_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_9.1_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_9.1)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_9.1_plot.png",
    plot = trwx_fit_9.1_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_9.1_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_9.1_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_9.2 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            # trend1:average_temperature_c + 
            trend24:average_temperature_c +
            # trend168:average_temperature_c + 
            # trend1:average_humidity_c + 
            trend24:average_humidity_c +
            # trend168:average_humidity_c + 
            # (trend1 | wday:hour), 
            (trend24 | wday:hour),
            # (trend168 | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_9.2) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_9.2), "./output/trwx_fit_9.2_tidy.csv")

glance(trwx_fit_9.2)

# merTools::plotREsim(merTools::REsim(trwx_fit_9.2), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_9.2)
anova(trwx_fit_1, trwx_fit_9.2)
anova(trwx_fit_2, trwx_fit_9.2)
anova(trwx_fit_3, trwx_fit_9.2)
anova(trwx_fit_4, trwx_fit_9.2)
anova(trwx_fit_5, trwx_fit_9.2)
anova(trwx_fit_6, trwx_fit_9.2)
anova(trwx_fit_7, trwx_fit_9.2)
anova(trwx_fit_8, trwx_fit_9.2)
anova(trwx_fit_9, trwx_fit_9.2)
anova(trwx_fit_10, trwx_fit_9.2)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_9.2)$trips, plot = FALSE, lag.max = 36))

trwx_fit_9.2_acf <- tidy((acf(residuals(trwx_fit_9.2), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_9.2_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_9.2_df <-
    augment(trwx_fit_9.2) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_9.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_9.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_9.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_9.2_plot <- 
    trwx_fit_9.2_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_9.2_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_9.2_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_9.2)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_9.2_plot.png",
    plot = trwx_fit_9.2_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_9.2_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_9.2_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_9.3 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            # trend1:average_temperature_c + 
            trend24:average_temperature_c +
            # trend168:average_temperature_c + 
            # trend1:average_humidity_c + 
            trend24:average_humidity_c +
            # trend168:average_humidity_c + 
            (average_temperature_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_9.3) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_9.3), "./output/trwx_fit_9.3_tidy.csv")

glance(trwx_fit_9.3)

# merTools::plotREsim(merTools::REsim(trwx_fit_9.3), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_9.3)
anova(trwx_fit_1, trwx_fit_9.3)
anova(trwx_fit_2, trwx_fit_9.3)
anova(trwx_fit_3, trwx_fit_9.3)
anova(trwx_fit_4, trwx_fit_9.3)
anova(trwx_fit_5, trwx_fit_9.3)
anova(trwx_fit_6, trwx_fit_9.3)
anova(trwx_fit_7, trwx_fit_9.3)
anova(trwx_fit_8, trwx_fit_9.3)
anova(trwx_fit_9, trwx_fit_9.3)
anova(trwx_fit_10, trwx_fit_9.3)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_9.3)$trips, plot = FALSE, lag.max = 36))

trwx_fit_9.3_acf <- tidy((acf(residuals(trwx_fit_9.3), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_9.3_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_9.3_df <-
    augment(trwx_fit_9.3) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_9.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_9.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_9.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_9.3_plot <- 
    trwx_fit_9.3_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_9.3_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_9.3_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_9.3)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_9.3_plot.png",
    plot = trwx_fit_9.3_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_9.3_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_9.3_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_9.4 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            # trend1:average_temperature_c + 
            trend24:average_temperature_c +
            # trend168:average_temperature_c + 
            # trend1:average_humidity_c + 
            trend24:average_humidity_c +
            # trend168:average_humidity_c + 
            (average_humidity_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_9.4) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_9.4), "./output/trwx_fit_9.4_tidy.csv")

glance(trwx_fit_9.4)

# merTools::plotREsim(merTools::REsim(trwx_fit_9.4), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_9.4)
anova(trwx_fit_1, trwx_fit_9.4)
anova(trwx_fit_2, trwx_fit_9.4)
anova(trwx_fit_3, trwx_fit_9.4)
anova(trwx_fit_4, trwx_fit_9.4)
anova(trwx_fit_5, trwx_fit_9.4)
anova(trwx_fit_6, trwx_fit_9.4)
anova(trwx_fit_7, trwx_fit_9.4)
anova(trwx_fit_8, trwx_fit_9.4)
anova(trwx_fit_9, trwx_fit_9.4)
anova(trwx_fit_10, trwx_fit_9.4)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_9.4)$trips, plot = FALSE, lag.max = 36))

trwx_fit_9.4_acf <- tidy((acf(residuals(trwx_fit_9.4), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_9.4_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_9.4_df <-
    augment(trwx_fit_9.4) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_9.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_9.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_9.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_9.4_plot <- 
    trwx_fit_9.4_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_9.4_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_9.4_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_9.4)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_9.4_plot.png",
    plot = trwx_fit_9.4_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_9.4_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_9.4_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_9.5 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            # trend1:average_temperature_c + 
            trend24:average_temperature_c +
            # trend168:average_temperature_c + 
            # trend1:average_humidity_c + 
            trend24:average_humidity_c +
            # trend168:average_humidity_c + 
            # (trend1 + average_temperature_c + average_humidity_c | wday:hour), 
            (trend24 + average_temperature_c + average_humidity_c | wday:hour),
            # (trend168 + average_temperature_c + average_humidity_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_9.5) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_9.5), "./output/trwx_fit_9.5_tidy.csv")

glance(trwx_fit_9.5)

# merTools::plotREsim(merTools::REsim(trwx_fit_9.5), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_9.5)
anova(trwx_fit_1, trwx_fit_9.5)
anova(trwx_fit_2, trwx_fit_9.5)
anova(trwx_fit_3, trwx_fit_9.5)
anova(trwx_fit_4, trwx_fit_9.5)
anova(trwx_fit_5, trwx_fit_9.5)
anova(trwx_fit_6, trwx_fit_9.5)
anova(trwx_fit_7, trwx_fit_9.5)
anova(trwx_fit_8, trwx_fit_9.5)
anova(trwx_fit_9, trwx_fit_9.5)
anova(trwx_fit_10, trwx_fit_9.5)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_9.5)$trips, plot = FALSE, lag.max = 36))

trwx_fit_9.5_acf <- tidy((acf(residuals(trwx_fit_9.5), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_9.5_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_9.5_df <-
    augment(trwx_fit_9.5) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_9.5_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_9.5_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_9.5_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_9.5_plot <- 
    trwx_fit_9.5_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_9.5_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_9.5_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_9.5)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 6)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_9.5_plot.png",
    plot = trwx_fit_9.5_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_9.5_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_9.5_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()


#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_9.6 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            # trend1:average_temperature_c + 
            trend24:average_temperature_c +
            # trend168:average_temperature_c + 
            # trend1:average_humidity_c + 
            trend24:average_humidity_c +
            # trend168:average_humidity_c + 
            (
                # trend1 + 
                trend24 +
                # trend168 + 
                    average_temperature_c + 
                    average_humidity_c + 
                    # trend1:average_temperature_c + 
                    trend24:average_temperature_c +
                    # trend168:average_temperature_c + 
                    # trend1:average_humidity_c | wday:hour), 
                    trend24:average_humidity_c | wday:hour),
                    # trend168:average_humidity_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_9.6) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_9.6), "./output/trwx_fit_9.6_tidy.csv")

glance(trwx_fit_9.6)

# merTools::plotREsim(merTools::REsim(trwx_fit_9.6), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_9.6)
anova(trwx_fit_1, trwx_fit_9.6)
anova(trwx_fit_2, trwx_fit_9.6)
anova(trwx_fit_3, trwx_fit_9.6)
anova(trwx_fit_4, trwx_fit_9.6)
anova(trwx_fit_5, trwx_fit_9.6)
anova(trwx_fit_6, trwx_fit_9.6)
anova(trwx_fit_7, trwx_fit_9.6)
anova(trwx_fit_8, trwx_fit_9.6)
anova(trwx_fit_9, trwx_fit_9.6)
anova(trwx_fit_10, trwx_fit_9.6)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_9.6)$trips, plot = FALSE, lag.max = 36))

trwx_fit_9.6_acf <- tidy((acf(residuals(trwx_fit_9.6), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_9.6_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_9.6_df <-
    augment(trwx_fit_9.6) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_9.6_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_9.6_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_9.6_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_9.6_plot <- 
    trwx_fit_9.6_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_9.6_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_9.6_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_9.6)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_9.6_plot.png",
    plot = trwx_fit_9.6_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_9.6_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_9.6_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_10.1 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            # trend1:average_temperature_c + 
            trend24:average_temperature_c +
            # trend168:average_temperature_c + 
            # trend1:average_humidity_c + 
            trend24:average_humidity_c +
            # trend168:average_humidity_c + 
            average_temperature_c:average_humidity_c + 
            (1 | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_10.1) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_10.1), "./output/trwx_fit_10.1_tidy.csv")

glance(trwx_fit_10.1)

# merTools::plotREsim(merTools::REsim(trwx_fit_10.1), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_10.1)
anova(trwx_fit_1, trwx_fit_10.1)
anova(trwx_fit_2, trwx_fit_10.1)
anova(trwx_fit_3, trwx_fit_10.1)
anova(trwx_fit_4, trwx_fit_10.1)
anova(trwx_fit_5, trwx_fit_10.1)
anova(trwx_fit_6, trwx_fit_10.1)
anova(trwx_fit_7, trwx_fit_10.1)
anova(trwx_fit_8, trwx_fit_10.1)
anova(trwx_fit_9, trwx_fit_10.1)
anova(trwx_fit_10, trwx_fit_10.1)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_10.1)$trips, plot = FALSE, lag.max = 36))

trwx_fit_10.1_acf <- tidy((acf(residuals(trwx_fit_10.1), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_10.1_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_10.1_df <-
    augment(trwx_fit_10.1) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_10.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_10.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_10.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_10.1_plot <- 
    trwx_fit_10.1_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_10.1_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_10.1_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_10.1)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_10.1_plot.png",
    plot = trwx_fit_10.1_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_10.1_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_10.1_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_10.2 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            # trend1:average_temperature_c + 
            trend24:average_temperature_c +
            # trend168:average_temperature_c + 
            # trend1:average_humidity_c + 
            trend24:average_humidity_c +
            # trend168:average_humidity_c + 
            average_temperature_c:average_humidity_c + 
            # (trend1 | wday:hour), 
            (trend24 | wday:hour),
            # (trend168 | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_10.2) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_10.2), "./output/trwx_fit_10.2_tidy.csv")

glance(trwx_fit_10.2)

# merTools::plotREsim(merTools::REsim(trwx_fit_10.2), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_10.2)
anova(trwx_fit_1, trwx_fit_10.2)
anova(trwx_fit_2, trwx_fit_10.2)
anova(trwx_fit_3, trwx_fit_10.2)
anova(trwx_fit_4, trwx_fit_10.2)
anova(trwx_fit_5, trwx_fit_10.2)
anova(trwx_fit_6, trwx_fit_10.2)
anova(trwx_fit_7, trwx_fit_10.2)
anova(trwx_fit_8, trwx_fit_10.2)
anova(trwx_fit_9, trwx_fit_10.2)
anova(trwx_fit_10, trwx_fit_10.2)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_10.2)$trips, plot = FALSE, lag.max = 36))

trwx_fit_10.2_acf <- tidy((acf(residuals(trwx_fit_10.2), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_10.2_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_10.2_df <-
    augment(trwx_fit_10.2) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_10.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_10.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_10.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_10.2_plot <- 
    trwx_fit_10.2_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_10.2_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_10.2_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_10.2)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_10.2_plot.png",
    plot = trwx_fit_10.2_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_10.2_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_10.2_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_10.3 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            # trend1:average_temperature_c + 
            trend24:average_temperature_c +
            # trend168:average_temperature_c + 
            # trend1:average_humidity_c + 
            trend24:average_humidity_c +
            # trend168:average_humidity_c + 
            average_temperature_c:average_humidity_c + 
            (average_temperature_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_10.3) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_10.3), "./output/trwx_fit_10.3_tidy.csv")

glance(trwx_fit_10.3)

# merTools::plotREsim(merTools::REsim(trwx_fit_10.3), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_10.3)
anova(trwx_fit_1, trwx_fit_10.3)
anova(trwx_fit_2, trwx_fit_10.3)
anova(trwx_fit_3, trwx_fit_10.3)
anova(trwx_fit_4, trwx_fit_10.3)
anova(trwx_fit_5, trwx_fit_10.3)
anova(trwx_fit_6, trwx_fit_10.3)
anova(trwx_fit_7, trwx_fit_10.3)
anova(trwx_fit_8, trwx_fit_10.3)
anova(trwx_fit_9, trwx_fit_10.3)
anova(trwx_fit_10, trwx_fit_10.3)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_10.3)$trips, plot = FALSE, lag.max = 36))

trwx_fit_10.3_acf <- tidy((acf(residuals(trwx_fit_10.3), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_10.3_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_10.3_df <-
    augment(trwx_fit_10.3) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_10.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_10.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_10.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_10.3_plot <- 
    trwx_fit_10.3_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_10.3_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_10.3_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_10.3)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_10.3_plot.png",
    plot = trwx_fit_10.3_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_10.3_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_10.3_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_10.4 <- 
    lmer(
        
        trips ~ 
            # trend1 + 
            trend24 +
            # trend168 + 
            average_temperature_c + 
            average_humidity_c + 
            # trend1:average_temperature_c + 
            trend24:average_temperature_c +
            # trend168:average_temperature_c + 
            # trend1:average_humidity_c + 
            trend24:average_humidity_c +
            # trend168:average_humidity_c + 
            average_temperature_c:average_humidity_c + 
            (average_humidity_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_10.4) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_10.4), "./output/trwx_fit_10.4_tidy.csv")

glance(trwx_fit_10.4)

# merTools::plotREsim(merTools::REsim(trwx_fit_10.4), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_10.4)
anova(trwx_fit_1, trwx_fit_10.4)
anova(trwx_fit_2, trwx_fit_10.4)
anova(trwx_fit_3, trwx_fit_10.4)
anova(trwx_fit_4, trwx_fit_10.4)
anova(trwx_fit_5, trwx_fit_10.4)
anova(trwx_fit_6, trwx_fit_10.4)
anova(trwx_fit_7, trwx_fit_10.4)
anova(trwx_fit_8, trwx_fit_10.4)
anova(trwx_fit_9, trwx_fit_10.4)
anova(trwx_fit_10, trwx_fit_10.4)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_10.4)$trips, plot = FALSE, lag.max = 36))

trwx_fit_10.4_acf <- tidy((acf(residuals(trwx_fit_10.4), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_10.4_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_10.4_df <-
    augment(trwx_fit_10.4) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_10.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_10.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_10.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_10.4_plot <- 
    trwx_fit_10.4_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_10.4_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_10.4_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_10.4)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_10.4_plot.png",
    plot = trwx_fit_10.4_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_10.4_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_10.4_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()


#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_10.5 <- 
    lmer(
        
        trips ~
            # trend1 +
            trend24 +
            # trend168 +
            average_temperature_c +
            average_humidity_c +
            # trend1:average_temperature_c +
            trend24:average_temperature_c +
            # trend168:average_temperature_c +
            # trend1:average_humidity_c +
            trend24:average_humidity_c +
            # trend168:average_humidity_c +
            average_temperature_c:average_humidity_c +
            # (trend1 + average_temperature_c + average_humidity_c | wday:hour), 
            (trend24 + average_temperature_c + average_humidity_c | wday:hour),
            # (trend168 + average_temperature_c + average_humidity_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_10.5) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_10.5), "./output/trwx_fit_10.5_tidy.csv")

glance(trwx_fit_10.5)

# merTools::plotREsim(merTools::REsim(trwx_fit_10.5), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_10.5)
anova(trwx_fit_1, trwx_fit_10.5)
anova(trwx_fit_2, trwx_fit_10.5)
anova(trwx_fit_3, trwx_fit_10.5)
anova(trwx_fit_4, trwx_fit_10.5)
anova(trwx_fit_5, trwx_fit_10.5)
anova(trwx_fit_6, trwx_fit_10.5)
anova(trwx_fit_7, trwx_fit_10.5)
anova(trwx_fit_8, trwx_fit_10.5)
anova(trwx_fit_9, trwx_fit_10.5)
anova(trwx_fit_10, trwx_fit_10.5)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_10.5)$trips, plot = FALSE, lag.max = 36))

trwx_fit_10.5_acf <- tidy((acf(residuals(trwx_fit_10.5), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_10.5_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_10.5_df <-
    augment(trwx_fit_10.5) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_10.5_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_10.5_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_10.5_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_10.5_plot <- 
    trwx_fit_10.5_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_10.5_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_10.5_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_10.5)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_10.5_plot.png",
    plot = trwx_fit_10.5_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_10.5_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_10.5_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_10.6 <- 
    lmer(
        
        trips ~
            # trend1 +
            trend24 +
            # trend168 +
            average_temperature_c +
            average_humidity_c +
            # trend1:average_temperature_c +
            trend24:average_temperature_c +
            # trend168:average_temperature_c +
            # trend1:average_humidity_c +
            trend24:average_humidity_c +
            # trend168:average_humidity_c +
            average_temperature_c:average_humidity_c +
            (
                # trend1 + 
                trend24 +
                # trend168 + 
                    average_temperature_c + 
                    average_humidity_c + 
                    # trend1:average_temperature_c + 
                    trend24:average_temperature_c +
                    # trend168:average_temperature_c + 
                    # trend1:average_humidity_c + 
                    trend24:average_humidity_c +
                    # trend168:average_humidity_c + 
                    average_temperature_c:average_humidity_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_10.6) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_10.6), "./output/trwx_fit_10.6_tidy.csv")

glance(trwx_fit_10.6)

# merTools::plotREsim(merTools::REsim(trwx_fit_10.6), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_10.6)
anova(trwx_fit_1, trwx_fit_10.6)
anova(trwx_fit_2, trwx_fit_10.6)
anova(trwx_fit_3, trwx_fit_10.6)
anova(trwx_fit_4, trwx_fit_10.6)
anova(trwx_fit_5, trwx_fit_10.6)
anova(trwx_fit_6, trwx_fit_10.6)
anova(trwx_fit_7, trwx_fit_10.6)
anova(trwx_fit_8, trwx_fit_10.6)
anova(trwx_fit_9, trwx_fit_10.6)
anova(trwx_fit_10, trwx_fit_10.6)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_10.6)$trips, plot = FALSE, lag.max = 36))

trwx_fit_10.6_acf <- tidy((acf(residuals(trwx_fit_10.6), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_10.6_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_10.6_df <-
    augment(trwx_fit_10.6) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_10.6_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_10.6_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_10.6_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_10.6_plot <- 
    trwx_fit_10.6_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_10.6_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_10.6_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_10.6)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_10.6_plot.png",
    plot = trwx_fit_10.6_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_10.6_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_10.6_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_11.1 <- 
    lmer(
        
        trips ~ 
            # trend1 * 
            trend24 * 
            # trend168 * 
            average_temperature_c * 
            average_humidity_c *
            (1 | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_11.1) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_11.1), "./output/trwx_fit_11.1_tidy.csv")

glance(trwx_fit_11.1)

# merTools::plotREsim(merTools::REsim(trwx_fit_11.1), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_11.1)
anova(trwx_fit_1, trwx_fit_11.1)
anova(trwx_fit_2, trwx_fit_11.1)
anova(trwx_fit_3, trwx_fit_11.1)
anova(trwx_fit_4, trwx_fit_11.1)
anova(trwx_fit_5, trwx_fit_11.1)
anova(trwx_fit_6, trwx_fit_11.1)
anova(trwx_fit_7, trwx_fit_11.1)
anova(trwx_fit_8, trwx_fit_11.1)
anova(trwx_fit_9, trwx_fit_11.1)
anova(trwx_fit_10, trwx_fit_11.1)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_11.1)$trips, plot = FALSE, lag.max = 36))

trwx_fit_11.1_acf <- tidy((acf(residuals(trwx_fit_11.1), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_11.1_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_11.1_df <-
    augment(trwx_fit_11.1) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_11.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_11.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_11.1_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_11.1_plot <- 
    trwx_fit_11.1_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_11.1_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_11.1_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_11.1)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_11.1_plot.png",
    plot = trwx_fit_11.1_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_11.1_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_11.1_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()


#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_11.2 <- 
    lmer(
        
        trips ~ 
            # trend1 * 
            trend24 *
            # trend168 * 
            average_temperature_c * 
            average_humidity_c +
            # (trend1 | wday:hour), 
            (trend24 | wday:hour),
            # (trend168 | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete,
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
        )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_11.2) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_11.2), "./output/trwx_fit_11.2_tidy.csv")

glance(trwx_fit_11.2)

# merTools::plotREsim(merTools::REsim(trwx_fit_11.2), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_11.2)
anova(trwx_fit_1, trwx_fit_11.2)
anova(trwx_fit_2, trwx_fit_11.2)
anova(trwx_fit_3, trwx_fit_11.2)
anova(trwx_fit_4, trwx_fit_11.2)
anova(trwx_fit_5, trwx_fit_11.2)
anova(trwx_fit_6, trwx_fit_11.2)
anova(trwx_fit_7, trwx_fit_11.2)
anova(trwx_fit_8, trwx_fit_11.2)
anova(trwx_fit_9, trwx_fit_11.2)
anova(trwx_fit_10, trwx_fit_11.2)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_11.2)$trips, plot = FALSE, lag.max = 36))

trwx_fit_11.2_acf <- tidy((acf(residuals(trwx_fit_11.2), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_11.2_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_11.2_df <-
    augment(trwx_fit_11.2) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_11.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_11.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_11.2_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_11.2_plot <- 
    trwx_fit_11.2_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_11.2_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_11.2_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_11.2)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_11.2_plot.png",
    plot = trwx_fit_11.2_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_11.2_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_11.2_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()


#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_11.3 <- 
    lmer(
        
        trips ~ 
            # trend1 * 
            trend24 *
            # trend168 * 
            average_temperature_c * 
            average_humidity_c *
            (average_temperature_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_11.3) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_11.3), "./output/trwx_fit_11.3_tidy.csv")

glance(trwx_fit_11.3)

# merTools::plotREsim(merTools::REsim(trwx_fit_11.3), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_11.3)
anova(trwx_fit_1, trwx_fit_11.3)
anova(trwx_fit_2, trwx_fit_11.3)
anova(trwx_fit_3, trwx_fit_11.3)
anova(trwx_fit_4, trwx_fit_11.3)
anova(trwx_fit_5, trwx_fit_11.3)
anova(trwx_fit_6, trwx_fit_11.3)
anova(trwx_fit_7, trwx_fit_11.3)
anova(trwx_fit_8, trwx_fit_11.3)
anova(trwx_fit_9, trwx_fit_11.3)
anova(trwx_fit_10, trwx_fit_11.3)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_11.3)$trips, plot = FALSE, lag.max = 36))

trwx_fit_11.3_acf <- tidy((acf(residuals(trwx_fit_11.3), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_11.3_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_11.3_df <-
    augment(trwx_fit_11.3) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_11.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_11.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_11.3_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_11.3_plot <- 
    trwx_fit_11.3_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_11.3_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_11.3_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_11.3)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_11.3_plot.png",
    plot = trwx_fit_11.3_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_11.3_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_11.3_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()


#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_11.4 <- 
    lmer(
        
        trips ~ 
            # trend1 * 
            trend24 *
            # trend168 * 
            average_temperature_c * 
            average_humidity_c *
            (average_humidity_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_11.4) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_11.4), "./output/trwx_fit_11.4_tidy.csv")

glance(trwx_fit_11.4)

# merTools::plotREsim(merTools::REsim(trwx_fit_11.4), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_11.4)
anova(trwx_fit_1, trwx_fit_11.4)
anova(trwx_fit_2, trwx_fit_11.4)
anova(trwx_fit_3, trwx_fit_11.4)
anova(trwx_fit_4, trwx_fit_11.4)
anova(trwx_fit_5, trwx_fit_11.4)
anova(trwx_fit_6, trwx_fit_11.4)
anova(trwx_fit_7, trwx_fit_11.4)
anova(trwx_fit_8, trwx_fit_11.4)
anova(trwx_fit_9, trwx_fit_11.4)
anova(trwx_fit_10, trwx_fit_11.4)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_11.4)$trips, plot = FALSE, lag.max = 36))

trwx_fit_11.4_acf <- tidy((acf(residuals(trwx_fit_11.4), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_11.4_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_11.4_df <-
    augment(trwx_fit_11.4) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_11.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_11.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_11.4_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_11.4_plot <- 
    trwx_fit_11.4_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_11.4_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_11.4_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_11.4)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_11.4_plot.png",
    plot = trwx_fit_11.4_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_11.4_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_11.4_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()



#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_11.5 <- 
    lmer(
        
        trips ~ 
            # trend1 *
            trend24 *
            # trend168 * 
            average_temperature_c * 
            average_humidity_c *
            # (trend1 + average_temperature_c + average_humidity_c | wday:hour),
            (trend24 + average_temperature_c + average_humidity_c | wday:hour),
            # (trend168 + average_temperature_c + average_humidity_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete, 
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
            )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_11.5) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_11.5), "./output/trwx_fit_11.5_tidy.csv")

glance(trwx_fit_11.5)

# merTools::plotREsim(merTools::REsim(trwx_fit_11.5), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_11.5)
anova(trwx_fit_1, trwx_fit_11.5)
anova(trwx_fit_2, trwx_fit_11.5)
anova(trwx_fit_3, trwx_fit_11.5)
anova(trwx_fit_4, trwx_fit_11.5)
anova(trwx_fit_5, trwx_fit_11.5)
anova(trwx_fit_6.5, trwx_fit_11.5)
anova(trwx_fit_6.5, trwx_fit_10.5)
anova(trwx_fit_7.5, trwx_fit_11.5)
anova(trwx_fit_8.5, trwx_fit_11.5)
anova(trwx_fit_9.5, trwx_fit_11.5)
anova(trwx_fit_9.5, trwx_fit_11.5)
anova(trwx_fit_10.5, trwx_fit_11.5)
anova(trwx_fit_9.5, trwx_fit_10.5, trwx_fit_11.5)
anova(trwx_fit_6.5, trwx_fit_9.5, trwx_fit_10.5, trwx_fit_11.5)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_11.5)$trips, plot = FALSE, lag.max = 36))

trwx_fit_11.5_acf <- tidy((acf(residuals(trwx_fit_11.5), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_11.5_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_11.5_df <-
    augment(trwx_fit_11.5) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_11.5_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_11.5_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_11.5_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_11.5_plot <- 
    trwx_fit_11.5_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_11.5_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_11.5_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_11.5)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_11.5_plot.png",
    plot = trwx_fit_11.5_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_11.5_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_11.5_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()


#-----------------------------------------------------------#
# Adding both temperature and humidity ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_11.6 <- 
    lmer(
        
        trips ~ 
            # trend1 * 
            trend24 *
            # trend168 * 
            average_temperature_c * 
            average_humidity_c *
            # (trend1 * average_temperature_c * average_humidity_c | wday:hour), 
            (trend24 * average_temperature_c * average_humidity_c | wday:hour),
            # (trend168 * average_temperature_c * average_humidity_c | wday:hour), 
        
        REML = FALSE,
        data = trwx_data_complete, 
        control = lmerControl(
            calc.derivs = FALSE,
            optCtrl = list(maxfun = 500000)
            )
    )

#----#=#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#==#=#----

tidy(trwx_fit_11.6) %>% mutate_if(is.numeric, round, digits = 2)

write_csv(tidy(trwx_fit_11.6), "./output/trwx_fit_11.6_tidy.csv")

glance(trwx_fit_11.6)

AIC(trwx_fit_11.6)

# merTools::plotREsim(merTools::REsim(trwx_fit_11.6), labs = TRUE)


## Nested ##

anova(trwx_fit_0, trwx_fit_11.6)
anova(trwx_fit_1, trwx_fit_11.6)
anova(trwx_fit_2, trwx_fit_11.6)
anova(trwx_fit_3, trwx_fit_11.6)
anova(trwx_fit_4, trwx_fit_11.6)
anova(trwx_fit_5, trwx_fit_11.6)
anova(trwx_fit_6, trwx_fit_11.6)
anova(trwx_fit_7, trwx_fit_11.6)
anova(trwx_fit_8, trwx_fit_11.6)
anova(trwx_fit_9, trwx_fit_11.6)
anova(trwx_fit_11.1, trwx_fit_11.6)

## Non-Nested ##


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# autocorrelation plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_acf <- tidy(acf(model.frame(trwx_fit_11.6)$trips, plot = FALSE, lag.max = 36))

trwx_fit_11.6_acf <- tidy((acf(residuals(trwx_fit_11.6), plot = FALSE, lag.max = 36)))

ggplot() +
    geom_col(
        data = trwx_data_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "antiquewhite1",
        alpha = 1
    ) +
    geom_col(
        data = trwx_fit_11.6_acf,
        aes(x = lag, y = acf),
        width = .5,
        fill = "red2",
        alpha = .5
    ) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    theme_dark()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# fitted values plot ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_11.6_df <-
    augment(trwx_fit_11.6) %>% 
    select(.fitted:.mu) %>% 
    bind_cols(trwx_data_complete, .)

month_breaks <-
    trwx_fit_11.6_df %>%
    as_tibble() %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(
        first_day = min(as_date(date_time_hourly)) %>% as.character(),
        ten_days = (min(as_date(date_time_hourly)) + days(9)) %>% as.character(),
        twenty_days = (min(as_date(date_time_hourly)) + days(19)) %>% as.character(),
        thirty_days = (min(as_date(date_time_hourly)) + days(29)) %>% as.character()
    ) %>% 
    mutate(breaks = str_c(
        first_day,
        ten_days,
        twenty_days,
        thirty_days,
        sep = ",",
        collapse = ","
    )) %>%
    pull(breaks) %>%
    extract2(1) %>%
    str_split(",") %>%
    unlist() %>%
    as_datetime(tz = "US/Eastern")

month_nums <-
    trwx_fit_11.6_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = FALSE) %>% 
    unique() %>% 
    as.numeric()

month_names <-
    trwx_fit_11.6_df %>%
    as_tibble() %>%
    ungroup() %>%
    pull(date_time_hourly) %>% 
    month(label = TRUE, abbr = FALSE) %>% 
    unique() %>% 
    droplevels() %>% 
    as.character() %>% 
    set_names(month_nums)


trwx_fit_11.6_plot <- 
    trwx_fit_11.6_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = trips, ymin = 0),
        fill = "gray15",
        alpha = 1,
        color = NA
    ) +
    geom_ribbon(
        aes(ymax = .fitted, ymin = 0),
        fill = "cyan",
        alpha = .6,
        color = NA
    ) +
    # geom_line(
    #     aes(y = .fixed),
    #     color = "red",
    #     size = 0.5
    # ) +
    scale_x_datetime(breaks = month_breaks, labels = month_breaks, date_labels = "%a %x") +
    labs(
        x = "Date",
        y = "Hourly trips",
        title = glue(
            "Fitted values vs. data for Citi Bike hourly trips, ",
            "{as_date(min(trwx_fit_11.6_df$date_time_hourly)) %>% format('%x')}",
            " to ",
            "{as_date(max(trwx_fit_11.6_df$date_time_hourly)) %>% format('%x')}"),
        subtitle = str_wrap(
            glue(
                "Fitted model: {deparse(as.formula(getCall(trwx_fit_11.6)), width.cutoff = 500L)}"
            ),
            width = 300L
        ),
        caption = "Note: cyan = model over-predictions, black = model under-predictions"
    ) +
    facet_wrap(~month, ncol = 1, scales = "free_x", labeller = as_labeller(month_names)) +
    theme_dark() +
    theme(plot.subtitle = element_text(size = 5)) +
    coord_cartesian(ylim = c(0, 9000), expand = FALSE)

ggsave(
    "./plots/time_series/trwx_fit_11.6_plot.png",
    plot = trwx_fit_11.6_plot,
    width = 12,
    height = 4,
    dpi = 300,
    scale = 1.2
)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_11.6_df %>% 
    ggplot(aes(x = date_time_hourly)) +
    geom_line(
        aes(y = .resid),
        color = "red3"
    )

trwx_fit_11.6_df %>% 
    ggplot(aes(x = trips, y = .resid)) +
    geom_point(
        color = "red3",
        shape = 1
    ) +
    geom_smooth()


#===========================================================#
# Outputting model calls ----
#===========================================================#

.ls <- ls()

ls_df <- data_frame(ls = .ls)

fit_list <- 
    ls_df %>% 
    filter(
        str_detect(ls, "trwx_fit") &
            !(str_detect(ls, "acf") |
                  str_detect(ls, "df") |
                  str_detect(ls, "plot"))
    )

call_list <-
    plyr::llply(
        fit_list$ls,
        .fun = function(x)
            deparse(
                as.formula(
                    getCall(
                        get(x)
                        )
                    ), 
                width.cutoff = 500L)
    )

names(call_list) <- fit_list$ls

write_lines(str_c(names(call_list), ":\n\n", call_list, "\n\n", "----", "\n"), "./output/call_list.txt")


###############################################################################################


.ls <- ls()

ls_df <- data_frame(ls = .ls)

fit_list <- 
    ls_df %>% 
    filter(
        str_detect(ls, "trwx_fit") &
            !(str_detect(ls, "acf") |
                  str_detect(ls, "df") |
                  str_detect(ls, "plot"))
    )

glance_list <-
    plyr::llply(
        fit_list$ls,
        .fun = function(x)
            glance(get(x))
    )

names(glance_list) <- fit_list$ls


call_list_df <- bind_rows(unlist(call_list)) %>% t() %>% as.data.frame() %>% rownames_to_column(var = "model") %>% as_tibble() %>% rename(call = V1)

glance_list_df <- bind_rows(glance_list) %>% add_column(model = fit_list$ls, .before = 1)

write_csv(call_list_df, "./output/call_list_df.csv")

call_glance_df <- left_join(glance_list_df, call_list_df) %>% as_tibble()

write_csv(call_glance_df, "./output/call_glance_df.csv")

###############################################################################################
###############################################################################################
