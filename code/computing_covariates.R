###########################################################################################-
###########################################################################################-
##
## Computing covariates ----
##
###########################################################################################-
###########################################################################################-

# Creating extra predictors for model fitting

#=========================================================================================#
# Setting up ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Loading libraries
#-----------------------------------------------------------------------------------------#

library(jsonlite)
library(tidyverse)
library(data.table)
library(lubridate)
library(DBI)
library(RSQLite)
library(dbplyr)
library(scales)
library(prophet)
library(fs)

#-----------------------------------------------------------------------------------------#
# Specifying directories
#-----------------------------------------------------------------------------------------#

data_dir <- "data/2018"

dir_create(data_dir)


#=========================================================================================#
# Main data ----
#=========================================================================================#

trips_station_weather_data_orig <- 
    
    read_rds(path(data_dir, "trips_station_weather_data_with_0s_2018.rds")) %>% 
    
    mutate(
        
        date  = as_date(date_time_hourly),
        trips = as.integer(trips),
        
        # rescaling predictors
        
        sin_year_r_2               = sin_year/2,
        cos_year_r_2               = cos_year/2,
        
        temperature_45_c15         = (temperature - 15)/45,
        dew_point_45_c7            = (dew_point - 7)/45,
        heat_index_45_c15          = (heat_index - 15)/45,
        humidity_100_c60           = (humidity - 60)/100,
        daily_ozone_300_c300       = (daily_ozone - 300)/300,
        daily_uv_index_10_c5       = (daily_uv_index - 5)/10,
        
        precip_yn = as.factor(precip_yn),
        sky_cover = as.factor(sky_cover),
        
        # computing categorical time predictors
        
        wday_wend = case_when(wday %in% c("Sun", "Sat") ~ "weekend", TRUE ~ "weekday") %>% as.factor(),
        rush_hour = case_when(hour %in% c(6:9, 16:19) ~ TRUE, TRUE ~ FALSE) %>% as.factor() %>% fct_rev(),
        day_period = 
            case_when(
                hour %in% c(6:10)       ~ "AM rush", 
                hour %in% c(11:15)      ~ "midday", 
                hour %in% c(16:20)      ~ "PM rush", 
                hour %in% c(21:23, 0:5) ~ "overnight", 
                TRUE ~ NA_character_
            ) %>% 
            as_factor() %>% 
            fct_relevel(levels = c("AM rush", "midday", "PM rush", "overnight")),
    ) %>% 
    
    # dropping original variables
    
    select(
        -trend24,
        -temperature,
        -dew_point,
        -heat_index,
        -humidity,
        -daily_ozone,
        -daily_uv_index,
        -sin_day,
        -cos_day
    )

gc()


#=========================================================================================#
# Station data ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Previously saved
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Station neighborhoods
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

stations_neighborhoods <- read_rds("data/stations_neighborhoods.rds")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Station distances
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

stations_entrances_route_count <- read_rds("data/stations_entrances_route_count.rds")

#-----------------------------------------------------------------------------------------#
# Getting stations from GBFS
#-----------------------------------------------------------------------------------------#

stations_from_gbfs <- 
    fromJSON("https://gbfs.citibikenyc.com/gbfs/en/station_information.json")$data$stations %>% 
    as_tibble() %>% 
    select(
        station_id, 
        capacity
    ) %>% 
    mutate(station_id = as.integer(station_id))


#-----------------------------------------------------------------------------------------#
# Pulling station status data from database
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Connecting
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

citibike_trip_db <- dbConnect(SQLite(), "data/citibike_trip_db.sqlite3")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Overall median capacity
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

stations_from_trips <- 
    citibike_trip_db %>% 
    tbl("station_status") %>% 
    mutate(capacity = num_bikes_available + num_docks_available) %>% 
    distinct(station_id, capacity) %>% 
    filter(!is.na(station_id)) %>% 
    select(station_id, capacity) %>% 
    collect() %>% 
    arrange(station_id, desc(capacity)) %>% 
    distinct(station_id, .keep_all = TRUE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Status over time
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

station_status <- 
    citibike_trip_db %>% 
    tbl("station_status") %>% 
    filter(year == 2018, month %in% 1:12) %>% 
    select(
        station_id,
        station_name,
        station_longitude,
        station_latitude,
        date,
        hour,
        num_bikes_available,
        num_docks_available,
        station_status,
        is_renting
    ) %>% 
    mutate(capacity = num_bikes_available + num_docks_available) %>% 
    collect() %>% 
    mutate(date = as_date(date))

dbDisconnect(citibike_trip_db)

gc()


#-----------------------------------------------------------------------------------------#
# Combining station datasets ----
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Combined capacity
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

station_capacity <- 
    bind_rows(
        stations_from_trips,
        stations_from_gbfs
    ) %>% 
    arrange(station_id, -capacity) %>% 
    distinct(station_id, .keep_all = TRUE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Calculating median capacity for stations
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

station_median_capacity <- 
    station_capacity %>% 
    group_by(station_id) %>% 
    summarise(median_capacity = capacity %>% median(na.rm = TRUE) %>% as.integer())

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Data frame of every hour
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

station_date_hour_df <- 
    expand_grid(
        station_id = unique(trips_station_weather_data_orig$start_station_id),
        date = 
            seq(
                min(trips_station_weather_data_orig$date), 
                max(trips_station_weather_data_orig$date), 
                by = "1 day"
            ),
        hour = 0:23
    )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Station list
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

stations <- read_rds("data/stations.rds")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Combining
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

station_status_hourly <-
    
    station_status %>%
    
    select(
        -station_name,
        -station_longitude,
        -station_latitude
    ) %>% 
    
    left_join(., station_date_hour_df, by = c("station_id", "date", "hour")) %>%
    
    complete(station_id, date, hour) %>% 
    
    left_join(., stations, by = "station_id") %>%
    
    left_join(., station_median_capacity, by = "station_id") %>% 
    
    # dropping status observations with no station_id
    
    drop_na(station_id) %>% 
    
    group_by(station_id) %>% 
    
    # replacing capacity NAs with station's median
    
    mutate(capacity = if_else(!is.finite(capacity), median_capacity, capacity) %>% as.integer()) %>% 
    
    ungroup() %>% 
    
    # replacing remaining capacity NAs with system median
    #   (because these are stations in the trip data that aren't in the status data, and 
    #   so there's no data at all on their capacity)
    
    mutate(
        capacity = replace_na(capacity, median(capacity, na.rm = TRUE) %>% round() %>% as.integer()),
        capacity = if_else(station_status == "out_of_service", 0L, capacity)
    )


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Replacing missing values
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

station_status_hourly_complete <- 
    
    # using data.table because it's much faster
    
    as.data.table(station_status_hourly)[, .(
        capacity = median(capacity, na.rm = TRUE),
        station_status = min(station_status, na.rm = TRUE)
    ), 
    keyby = .(station_id, date, hour)
    ] %>%
    
    mutate(capacity = as.integer(capacity)) %>% 
    
    left_join(., station_median_capacity, by = "station_id") %>%
    left_join(., stations, by = "station_id") %>%
    
    as_tibble() %>% 
    
    group_by(station_id) %>% 
    
    # replacing NAs with nearest complete previous observation
    #   (`is.finite` is a generalization of `ia.na` -- returns FALSE for NA, NaN, Inf)
    
    mutate(
        capacity = 
            if_else(
                !is.finite(capacity),
                detect(capacity, ~ is.finite(.x)),
                capacity,
                median_capacity
            )
    ) %>% 
    
    ungroup() %>% 
    
    drop_na(station_id, station_name)


#=========================================================================================#
# Creating weather residual variables ----
#=========================================================================================#

# Each of these variables follow sinusoidal yearly or daily patterns, or both, so their 
#   residuals represent deviations from those seasonal patterns

#-----------------------------------------------------------------------------------------#
# Creating weather dataset
#-----------------------------------------------------------------------------------------#

# weather_data <- 
#     trips_station_weather_data_orig %>% 
#     distinct(date_time_hourly, .keep_all = TRUE) %>% 
#     select(
#         date_time_hourly,
#         sin_year_r_01_c,
#         cos_year_r_01_c,
#         sin_day_r_01_c,
#         cos_day_r_01_c,
#         temperature_45_c,
#         dew_point_45_c,
#         daily_uv_index_10_c,
#         humidity_100_c
#     )


#-----------------------------------------------------------------------------------------#
# Fitting models
#-----------------------------------------------------------------------------------------#

# temperature_45_c_resid <- 
#     residuals(
#         lm(temperature_45_c ~ sin_year_r_01_c + cos_year_r_01_c + sin_day_r_01_c + cos_day_r_01_c, weather_data)
#     )
# 
# dew_point_45_c_resid <- 
#     residuals(
#         lm(dew_point_45_c ~ sin_year_r_01_c + cos_year_r_01_c + sin_day_r_01_c + cos_day_r_01_c, weather_data)
#     )
# 
# daily_uv_index_10_c_resid <- 
#     residuals(
#         lm(daily_uv_index_10_c ~ sin_year_r_01_c + cos_year_r_01_c, weather_data)
#     )
# 
# humidity_100_c_resid <- 
#     residuals(
#         lm(humidity_100_c ~ sin_year_r_01_c + cos_year_r_01_c + sin_day_r_01_c + cos_day_r_01_c, weather_data)
#     )


#-----------------------------------------------------------------------------------------#
# Joining
#-----------------------------------------------------------------------------------------#

# weather_residuals <- 
#     tibble(
#         date_time_hourly          = weather_data$date_time_hourly,
#         temperature_45_c_resid    = temperature_45_c_resid,
#         dew_point_45_c_resid      = dew_point_45_c_resid,
#         daily_uv_index_10_c_resid = daily_uv_index_10_c_resid,
#         humidity_100_c_resid      = humidity_100_c_resid
#     )


#=========================================================================================#
# Joining trips, weather, and station datasets ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Getting US holidays
#-----------------------------------------------------------------------------------------#

us_holidays <- 
    generated_holidays %>% 
    as_tibble() %>% 
    filter(country == "US") %>% 
    mutate(date = as_date(ds))


#-----------------------------------------------------------------------------------------#
# Joining
#-----------------------------------------------------------------------------------------#

trips_station_weather_data <- 
    
    # trips data
    
    trips_station_weather_data_orig %>% 
    
    # station data
    
    left_join(
        .,
        station_status_hourly_complete %>% select(-station_name),
        by = c("date", "start_station_id" = "station_id", "hour")
    ) %>% 
    
    left_join(
        .,
        station_capacity %>% rename(capacity_now = capacity),
        by = c("start_station_id" = "station_id")
    ) %>% 
    
    # replacing remaining capacity NAs with system median
    #   (because these are stations in the trip data that aren't in the status data, and 
    #   so there's no data at all on their capacity)
    
    mutate(
        
        # if capacity is missing, assign it the station's current capacity
        
        capacity = if_else(!is.finite(capacity), capacity_now %>% as.integer(), capacity),
        
        # if capacity is missing, assign it the station's median capacity
        
        capacity = if_else(!is.finite(capacity), median_capacity %>% as.integer(), capacity),
        
        # if capacity is still missing, assign it the system's median capacity
        
        capacity = replace_na(capacity, median(capacity, na.rm = TRUE) %>% round() %>% as.integer())
    ) %>% 
    
    # computing one over median capacity for each station
    
    group_by(start_station_name) %>% 
    mutate(capacity_med = median(capacity)) %>% 
    ungroup() %>% 
    
    # rescaling this capscity median to match opther rescaled covariates
    
    mutate(capacity_med_30_c30 = (capacity_med - 30)/30) %>% 
    
    # transit data for stations
    
    left_join(
        ., 
        stations_entrances_route_count %>% select(-start_station_name),
        by = "start_station_id"
    ) %>% 
    
    # neighborhood and borough names for station
    
    left_join(
        ., 
        stations_neighborhoods %>% select(station_id, boro = BoroName, neighborhood = NTAName),
        by = c("start_station_id" = "station_id")
    ) %>% 
    
    # weather residuals
    
    # left_join(
    #     .,
    #     weather_residuals,
    #     by = "date_time_hourly"
    # ) %>% 
    
    # holidays during time period
    
    left_join(
        ., 
        us_holidays %>% mutate(isholiday = TRUE) %>% select(date, isholiday),
        by = "date"
    ) %>% 
    
    # creating additional variables
    
    mutate(
        isholiday = replace_na(isholiday, FALSE),
        workday = 
            case_when(
                wday_wend == "weekday" & isholiday == FALSE ~ TRUE, 
                TRUE ~ FALSE
            ) %>% 
            as.factor() %>% 
            fct_rev()
    ) %>% 
    
    group_by(start_station_name, date) %>% 
    mutate(
        no_trips_hours = sum(trips == 0, na.rm = TRUE),
        n_hours = length(trips)
    ) %>% 
    
    ungroup() %>% 
    
    mutate(
        in_service_trips = if_else(no_trips_hours == n_hours, 0L, 1L),
        in_service = 
            case_when(
                (!is.finite(station_status) & in_service_trips == 0L) | station_status == "out_of_service" ~ 0L,
                (!is.finite(station_status) & in_service_trips == 1L) | station_status == "active" ~ 1L
            ),
        in_service_fct = as_factor(in_service),
        hour = as_factor(hour),
        ts_yday = as_factor(yday(date_time_hourly))
    ) %>% 
    
    select(
        -in_service,
        -in_service_trips,
        -station_status,
        -no_trips_hours,
        -n_hours,
        -isholiday,
        -median_capacity,
        # -capacity_med,
        -capacity_now,
        -station_longitude,
        -station_latitude
    )


#-----------------------------------------------------------------------------------------#
# Setting contrasts
#-----------------------------------------------------------------------------------------#

contrasts(trips_station_weather_data$hour)           <- "contr.Sum"
contrasts(trips_station_weather_data$workday)        <- "contr.Sum"
contrasts(trips_station_weather_data$in_service_fct) <- "contr.Sum"
contrasts(trips_station_weather_data$precip_yn)      <- "contr.Sum"

#-----------------------------------------------------------------------------------------#
# Saving
#-----------------------------------------------------------------------------------------#

write_rds(
    trips_station_weather_data, 
    path(
        data_dir, 
        "trips_station_weather_data_extra_2018.rds"
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
