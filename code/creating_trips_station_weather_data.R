############################################################-
############################################################-
##
## Creating combined trips and weather data ----
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

#-----------------------------------------------------------#
# Connecting to databases ----
#-----------------------------------------------------------#

citibike_trip_db <- dbConnect(SQLite(), "./data/citibike_trip_db.sqlite3")
nyc_weather_db <- dbConnect(SQLite(),   "./data/weather/nyc_weather_db.sqlite3")

#===========================================================#
# Weather ----
#===========================================================#

#-----------------------------------------------------------#
# Extracting main data ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Pulling data from database ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

weather <-
    nyc_weather_db %>%
    tbl("nyc_weather") %>%
    filter(
        year == 2018 &
            month %in% 3:5 &
            usaf_wban %in% c(72503014732, 72505394728) &
            report_type == "FM-15"
    ) %>%
    collect() %>%
    mutate(
        date_time = as_datetime(date_time, tz = "US/Eastern"),
        date_time_hourly = as_datetime(date_time_hourly,  tz = "US/Eastern")
    ) %>%
    complete(usaf_wban, date_time_hourly)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Writing RDS ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

write_rds(
    weather,
    "./data/march_april_may/weather.rds")


#-----------------------------------------------------------#
# Summarizing by hour ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Within stations ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

## Averaging multiple observations if present

weather_hourly <-
    weather %>%
    group_by(usaf_wban, date_time_hourly) %>%
    summarise(
        temperature = mean(temperature, na.rm = TRUE),
        dew_point   = mean(dew_point, na.rm = TRUE),
        humidity    = mean(humidity, na.rm = TRUE),
        wind_speed  = mean(wind_speed, na.rm = TRUE),
        sky_cover   = mean(sky_cover, na.rm = TRUE),
        precip      = mean(precip, na.rm = TRUE),
        precip_yn   = mean(precip_yn, na.rm = TRUE),
        heat_index  = mean(heat_index, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(callsign = recode(usaf_wban, "72503014732" = "KLGA", "72505394728" = "KNYC")) %>%
    select(-usaf_wban) %>%
    arrange(callsign, date_time_hourly)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Replacing missing ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

## If an hourly observation (t-0) is missing at KNYC, first try to replace it with
##  the KNYC t-1 value plus the change at KLGA from t-1 to t-0
##
## If there is no KNYC t-1 value, then simply replace the KNYC t-0 value with
##  the KLGA t-0 value
##
## Do this for each separate weather variable


####---- temperature ----####

weather_hourly <-
    weather_hourly %>%

    select(date_time_hourly, callsign, temperature) %>%
    spread(key = callsign, value = temperature) %>%
    mutate(KNYC = case_when(is.na(KNYC) ~ lag(KNYC) + (KLGA - lag(KLGA)), TRUE ~ identity(KNYC))) %>%
    mutate(KNYC = case_when(is.na(KNYC) ~ KLGA, TRUE ~ identity(KNYC))) %>%
    gather(key = "callsign", value = "temperature", KLGA, KNYC) %>%
    left_join(weather_hourly, ., by = c("callsign", "date_time_hourly"), suffix = c("", "_new"))


####---- dew_point ----####

weather_hourly <-
    weather_hourly %>%

    select(date_time_hourly, callsign, dew_point) %>%
    spread(key = callsign, value = dew_point) %>%
    mutate(KNYC = case_when(is.na(KNYC) ~ lag(KNYC) + (KLGA - lag(KLGA)), TRUE ~ identity(KNYC))) %>%
    mutate(KNYC = case_when(is.na(KNYC) ~ KLGA, TRUE ~ identity(KNYC))) %>%
    gather(key = "callsign", value = "dew_point", KLGA, KNYC) %>%
    left_join(weather_hourly, ., by = c("callsign", "date_time_hourly"), suffix = c("", "_new"))


####---- humidity ----####

weather_hourly <-
    weather_hourly %>%

    select(date_time_hourly, callsign, humidity) %>%
    spread(key = callsign, value = humidity) %>%
    mutate(KNYC = case_when(is.na(KNYC) ~ lag(KNYC) + (KLGA - lag(KLGA)), TRUE ~ identity(KNYC))) %>%
    mutate(KNYC = case_when(is.na(KNYC) ~ KLGA, TRUE ~ identity(KNYC))) %>%
    gather(key = "callsign", value = "humidity", KLGA, KNYC) %>%
    left_join(weather_hourly, ., by = c("callsign", "date_time_hourly"), suffix = c("", "_new"))


####---- wind_speed ----####

weather_hourly <-
    weather_hourly %>%

    select(date_time_hourly, callsign, wind_speed) %>%
    spread(key = callsign, value = wind_speed) %>%
    mutate(KNYC = case_when(is.na(KNYC) ~ lag(KNYC) + (KLGA - lag(KLGA)), TRUE ~ identity(KNYC))) %>%
    mutate(KNYC = case_when(is.na(KNYC) ~ KLGA, TRUE ~ identity(KNYC))) %>%
    gather(key = "callsign", value = "wind_speed", KLGA, KNYC) %>%
    left_join(weather_hourly, ., by = c("callsign", "date_time_hourly"), suffix = c("", "_new"))


####---- sky_cover ----####

weather_hourly <-
    weather_hourly %>%

    select(date_time_hourly, callsign, sky_cover) %>%
    spread(key = callsign, value = sky_cover) %>%
    mutate(KNYC = case_when(is.na(KNYC) ~ KLGA,      TRUE ~ identity(KNYC))) %>%
    mutate(KNYC = case_when(is.na(KNYC) ~ lag(KNYC), TRUE ~ identity(KNYC))) %>%
    mutate(KNYC = case_when(is.na(KNYC) ~ lag(KLGA), TRUE ~ identity(KNYC))) %>%
    gather(key = "callsign", value = "sky_cover", KLGA, KNYC) %>%
    left_join(weather_hourly, ., by = c("callsign", "date_time_hourly"), suffix = c("", "_new"))


####---- precip ----####

weather_hourly <-
    weather_hourly %>%

    select(date_time_hourly, callsign, precip) %>%
    spread(key = callsign, value = precip) %>%
    mutate(KNYC = case_when(is.na(KNYC) ~ lag(KNYC) + (KLGA - lag(KLGA)), TRUE ~ identity(KNYC))) %>%
    mutate(KNYC = case_when(is.na(KNYC) ~ KLGA, TRUE ~ identity(KNYC))) %>%
    gather(key = "callsign", value = "precip", KLGA, KNYC) %>%
    left_join(weather_hourly, ., by = c("callsign", "date_time_hourly"), suffix = c("", "_new"))


####---- precip_yn (replacing, b/c categorical) ----####

weather_hourly <-
    weather_hourly %>%

    select(date_time_hourly, callsign, precip_yn) %>%
    spread(key = callsign, value = precip_yn) %>%
    mutate(KNYC = case_when(is.na(KNYC) ~ KLGA, TRUE ~ identity(KNYC))) %>%
    mutate(KNYC = case_when(is.na(KNYC) ~ lag(KNYC), TRUE ~ identity(KNYC))) %>%
    mutate(KNYC = case_when(is.na(KNYC) ~ lag(KLGA), TRUE ~ identity(KNYC))) %>%
    gather(key = "callsign", value = "precip_yn", KLGA, KNYC) %>%
    left_join(weather_hourly, ., by = c("callsign", "date_time_hourly"), suffix = c("", "_new"))


####---- heat_index ----####

weather_hourly <-
    weather_hourly %>%

    select(date_time_hourly, callsign, heat_index) %>%
    spread(key = callsign, value = heat_index) %>%
    mutate(KNYC = case_when(is.na(KNYC) ~ lag(KNYC) + (KLGA - lag(KLGA)), TRUE ~ identity(KNYC))) %>%
    mutate(KNYC = case_when(is.na(KNYC) ~ KLGA, TRUE ~ identity(KNYC))) %>%
    gather(key = "callsign", value = "heat_index", KLGA, KNYC) %>%
    left_join(weather_hourly, ., by = c("callsign", "date_time_hourly"), suffix = c("", "_new"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Writing RDS ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

write_rds(
    weather_hourly,
    "./data/march_april_may/weather_hourly.rds")


#-----------------------------------------------------------#
# Keeping only new variables from KNYC ----
#-----------------------------------------------------------#

weather_hourly_new <-
    weather_hourly %>%
    filter(callsign == "KNYC") %>%
    select(-temperature:-heat_index) %>%
    rename_at(
        vars(temperature_new:heat_index_new),
        funs(str_remove(string = ., pattern = "_new"))
    ) %>%
    mutate(
        date   = as_date(date_time_hourly),
        year   = year(date_time_hourly)  %>% as.integer(),
        month  = month(date_time_hourly) %>% as.integer(),
        day    = day(date_time_hourly)   %>% as.integer(),
        hour   = hour(date_time_hourly)  %>% as.integer()
    )


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Writing RDS ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

write_rds(
    weather_hourly_new,
    "./data/march_april_may/weather_hourly_new.rds")


#-----------------------------------------------------------#
# Extracting daily UV Index -----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Pulling data from database ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

daily_uv <-
    nyc_weather_db %>%
    tbl("daily_uv") %>%
    filter(year == 2018 & month %in% 3:5) %>%
    select(date, year, month, day, uv_index, ozone) %>%
    collect() %>%
    mutate(date = as_date(date)) %>%
    rename(
        daily_uv_index = uv_index,
        daily_ozone = ozone
    )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Writing RDS ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

write_rds(
    daily_uv,
    "./data/march_april_may/daily_uv.rds")


#-----------------------------------------------------------#
# Extracting 3-minute UV Index ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Pulling data from database ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

three_min_uv <-
    nyc_weather_db %>%
    tbl("three_min_uv") %>%
    filter(year == 2018 & month %in% 3:5) %>%
    select(date, date_time, date_time_hourly, year:minute, uv_index) %>%
    collect() %>%
    mutate(
        date = as_date(date),
        date_time = as_datetime(date_time, tz = "US/Eastern"),
        date_time_hourly = as_datetime(date_time_hourly,  tz = "US/Eastern")
    )


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Writing RDS ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

write_rds(
    three_min_uv,
    "./data/march_april_may/three_min_uv.rds")


#-----------------------------------------------------------#
# Summarizing by hour ----
#-----------------------------------------------------------#

date_time_seq_df <-
    three_min_uv %>%
    summarise(
        date_time_min = min(date_time_hourly, na.rm = TRUE),
        date_time_max = max(date_time_hourly, na.rm = TRUE)) %$%
    seq(date_time_min, date_time_max, by = "1 hour") %>%
    data_frame(date_time_seq = .)


three_min_uv_hourly <-
    three_min_uv %>%
    group_by(date_time_hourly) %>%
    summarise(hourly_uv_index  = mean(uv_index, na.rm = TRUE)) %>%
    left_join(., three_min_uv %>% select(date_time_hourly, year, month, day, hour) %>% distinct()) %>%
    right_join(., date_time_seq_df, by = c("date_time_hourly" = "date_time_seq"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Writing RDS ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

write_rds(
    three_min_uv_hourly,
    "./data/march_april_may/three_min_uv_hourly.rds")


#-----------------------------------------------------------#
# Combining all weather data ----
#-----------------------------------------------------------#

weather_hourly_all <-
    weather_hourly_new %>%
    left_join(., three_min_uv_hourly) %>%
    left_join(., daily_uv)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Writing RDS ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

write_rds(
    weather_hourly_all,
    "./data/march_april_may/weather_hourly_all.rds")


#===========================================================#
# Trips ----
#===========================================================#

#-----------------------------------------------------------#
# Pulling out of database ----
#-----------------------------------------------------------#

trips <-
    citibike_trip_db %>%
    tbl("citibike_trips") %>%
    filter(year == 2018 & month %in% 3:5) %>%
    select(
        bike_id,
        trip_duration,
        start_time,
        start_station_id,
        start_station_name,
        stop_time,
        end_station_id,
        end_station_name) %>%
    collect() %>%
    mutate(
        start_time = as_datetime(start_time, tz = "US/Eastern"),
        stop_time  = as_datetime(stop_time,  tz = "US/Eastern")) %>%
    mutate(
        start_time_hourly = floor_date(start_time, "hours"),
        stop_time_hourly  = floor_date(stop_time,  "hours"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# writing RDS ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

write_rds(
    trips,
    "./data/march_april_may/trips.rds",
    compress = "gz",
    compression = 9
)


#-----------------------------------------------------------#
# Grouping by stations ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# hours with trips ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

## At each station, hours without trips are implicitly missing

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
    )


####---- Writing RDS ----####

write_rds(
    trips_station_hourly,
    "./data/march_april_may/trips_station_hourly.rds")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# all stations & all hours ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

## At each station, hours without trips are explicitly = `0`

start_time_seq_df <-
    trips_station_hourly %>%
    summarise(
        start_time_min = min(start_time_hourly, na.rm = TRUE),
        start_time_max = max(start_time_hourly, na.rm = TRUE)) %$%
    seq(start_time_min, start_time_max, by = "1 hour") %>%
    data_frame(start_time_hourly = .)

stations <- trips_station_hourly %>% select(start_station_id, start_station_name) %>% distinct()

trips_station_hourly_with_0s <-
    trips_station_hourly %>%
    group_by(start_time_hourly, start_station_id) %>%
    full_join(start_time_seq_df, ., by = "start_time_hourly") %>%
    complete(
        start_station_id,
        start_time_hourly,
        fill = list(trips = 0, average_trip_duration = NA_real_)
    ) %>%
    drop_na(start_station_id)


####---- Writing RDS ----####

write_rds(
    trips_station_hourly_with_0s,
    "./data/march_april_may/trips_station_hourly_with_0s.rds")


#===========================================================#
# Combining trips and weather ----
#===========================================================#

#-----------------------------------------------------------#
# hours with trips ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Adding recoded weather variables to trips data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trips_station_weather_data <-
    weather_hourly %>%

    mutate(
        trend24   = 1:nrow(.)/24,
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


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Writing RDS ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

write_rds(
    trips_station_weather_data,
    "./data/march_april_may/trips_station_weather_data.rds", compress = "gz")


#-----------------------------------------------------------#
# all stations & all hours ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Adding recoded weather variables to trips data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trips_station_weather_data_with_0s <-
    weather_hourly %>%

    mutate(
        trend24   = 1:nrow(.)/24,
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

    left_join(trips_station_hourly_with_0s,
              .,
              by = c("start_time_hourly" = "date_time_hourly")) %>%

    rename(date_time_hourly = start_time_hourly) %>%
    arrange(date_time_hourly, start_station_id)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Writing RDS ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

write_rds(
    trips_station_weather_data_with_0s,
    "./data/march_april_may/trips_station_weather_data_with_0s.rds", compress = "gz")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #             ---- THIS IS THE END ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
