##################################################-
##################################################-
##
## Citibike trips + weather ----
##
##################################################-
##################################################-


#========================================#
# Setting up ----
#========================================#

#---------------------------------#
# Loading libraries ----
#---------------------------------#


library(mgcv)
library(readr)
library(tibble)
library(lubridate)
library(DBI)
library(stringr)
library(cowplot)
library(ggplot2)
library(ggthemes)
library(viridis)
library(maps)
library(ggmap)
library(maptools)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(magrittr)
library(dplyr)
library(dbplyr)
library(scales)
library(broom)
library(sf)
library(ggforce)
library(dplyr)
library(geosphere)
library(ggrepel)
library(tidyquant)
library(TTR)


#---------------------------------#
# Loading functions 
#---------------------------------#

source("./code/functions/se_funs.R")

invlogit <- function(x) exp(x) / (1 + exp(x))


#========================================#
# Loading data ----
#========================================#

#---------------------------------#
# Citibike Trips ----
#---------------------------------#

# *** If previously saved *** ----

citibike_trips_2016 <- read_rds("./data/citibike_trips_2016.rds")


# *** If pulling from database *** ----

# citibike_trip_db <- dbConnect(RSQLite::SQLite(), "./data/citibike_trip_db.sqlite3")


#---------------------------------#
# Weather data ----
#---------------------------------#

nyc_weather_clean <- read_rds("./data/weather/nyc_weather_2016_clean.rds")

#========================================#
# Getting only 2016 data ----
#========================================#


# citibike_trips_2016 <-
#     tbl(citibike_trip_db, "citibike_trips") %>%
#     filter(year == 2016) %>%
#     select(
#         month,
#         day,
#         year,
#         bike_id,
#         start_time,
#         start_station_id,
#         start_station_name,
#         start_station_longitude,
#         start_station_latitude,
#         stop_time,
#         end_station_id,
#         end_station_name,
#         end_station_longitude,
#         end_station_latitude,
#         trip_duration
#     ) %>%
#     collect() %>%
#     mutate(
#         start_time = as_datetime(start_time, tz = "US/Eastern"),
#         stop_time  = as_datetime(stop_time,  tz = "US/Eastern"),
#         start_time_hourly = floor_date(start_time, "hours"),
#         stop_time_hourly  = floor_date(stop_time, "hours")
#     )
# 
# ### Saving as RDS ###
# 
# write_rds(citibike_trips_2016, "./data/citibike_trips_2016.rds")


#========================================#
# Combining bike and weather data ----
#========================================#


### Aggregating trips by hour ###

citibike_trips_2016_2 <- 
    citibike_trips_2016 %>% 
    select(
        # bike_id, 
        start_time_hourly, 
        start_station_id
    ) %>% 
    count(
        start_time_hourly, 
        # bike_id           # Bike-specific traffic for hour
        start_station_id    # Station-specific traffic for hour
        )


### Aggregating weather by hour ###

nyc_weather_clean_2 <- 
    nyc_weather_clean %>% 
    select(
        date_time_hourly, 
        temperature
    ) %>% 
    group_by(date_time_hourly) %>% 
    summarise(temp_mean = mean(temperature, na.rm = TRUE)) # Average if >1 obs


### Making sure that missing hours are explicitly so in the data ###

all_hours <- 
    data_frame(
        date_time_hourly =
            seq(
                as_datetime("2016-01-01 00:00:00", tz = "US/Eastern"),
                as_datetime("2016-12-31 23:00:00", tz = "US/Eastern"),
                "hour"
            ))

nyc_weather_clean_3 <- 
    left_join(
        all_hours, 
        nyc_weather_clean_2
    ) %>% 
    arrange(date_time_hourly)


### Combining ###

bikes_weather <-
    left_join(
        nyc_weather_clean_3,
        citibike_trips_2016_2,
        by = c("date_time_hourly" = "start_time_hourly")
    )


#========================================#
# Summarizing weather and count data ----
#========================================#


# with(bikes_weather, cor.test(temp_mean, n))
# 
# 
# bikes_weather %>% ggplot(aes(x = date_time_hourly, y = n)) + geom_line()
# bikes_weather %>% ggplot(aes(x = date_time_hourly, y = temp_mean)) + geom_line()
# 
# 
# bikes_weather %>% 
#     mutate(date_time_daily = floor_date(date_time_hourly, "days")) %>% 
#     filter(is.na(n)) %>% 
#     count(date_time_daily) %>% 
#     arrange(-nn)
# 
# 
# bikes_weather %>% ggplot(aes(temp_mean, n)) + geom_point() + geom_smooth(method = "loess")


#========================================#
# Aggregating by time period ----
#========================================#

#----------------------------------#
# Across the whole system ----
#----------------------------------#


nyc_hourly <- 
    bikes_weather %>% 
    select(date_time_hourly,
           temp_mean_hourly = temp_mean,
           n_hourly = n)


nyc_daily <- 
    bikes_weather %>% 
    mutate(date_time_daily = floor_date(date_time_hourly, "days")) %>% 
    group_by(date_time_daily) %>% 
    summarise(
        temp_mean_daily = mean(temp_mean, na.rm = TRUE),
        temp_se_daily   = se(temp_mean, na.rm = TRUE),
        n_daily         = mean(n, na.rm = TRUE)
        )


nyc_weekly <- 
    bikes_weather %>% 
    mutate(date_time_weekly = floor_date(date_time_hourly, "weeks")) %>% 
    group_by(date_time_weekly) %>% 
    summarise(
        temp_mean_weekly = mean(temp_mean, na.rm = TRUE),
        temp_se_weekly   = se(temp_mean, na.rm = TRUE),
        n_weekly         = mean(n, na.rm = TRUE)
    )


nyc_monthly <- 
    bikes_weather %>% 
    mutate(date_time_monthly = floor_date(date_time_hourly, "months")) %>% 
    group_by(date_time_monthly) %>% 
    summarise(
        temp_mean_monthly = mean(temp_mean, na.rm = TRUE),
        temp_se_monthly   = se(temp_mean, na.rm = TRUE),
        n_monthly         = mean(n, na.rm = TRUE)
    )


#----------------------------------#
# Broken down by station ----
#----------------------------------#


nyc_stations_hourly <- 
    bikes_weather %>% 
    rename(temp_mean_hourly = temp_mean,
           n_hourly = n)


nyc_stations_daily <- 
    bikes_weather %>% 
    mutate(date_time_daily = floor_date(date_time_hourly, "days")) %>% 
    group_by(date_time_daily, start_station_id) %>% 
    summarise(
        temp_mean_daily = mean(temp_mean, na.rm = TRUE),
        temp_se_daily   = se(temp_mean, na.rm = TRUE),
        n_daily         = mean(n, na.rm = TRUE)
        )


nyc_stations_weekly <- 
    bikes_weather %>% 
    mutate(date_time_weekly = floor_date(date_time_hourly, "weeks")) %>% 
    group_by(date_time_weekly, start_station_id) %>% 
    summarise(
        temp_mean_weekly = mean(temp_mean, na.rm = TRUE),
        temp_se_weekly   = se(temp_mean, na.rm = TRUE),
        n_weekly         = mean(n, na.rm = TRUE)
    )


nyc_stations_monthly <- 
    bikes_weather %>% 
    mutate(date_time_monthly = floor_date(date_time_hourly, "months")) %>% 
    group_by(date_time_monthly, start_station_id) %>% 
    summarise(
        temp_mean_monthly = mean(temp_mean, na.rm = TRUE),
        temp_se_monthly   = se(temp_mean, na.rm = TRUE),
        n_monthly         = mean(n, na.rm = TRUE)
    )


#-------------------------------------------#
# Plotting trends by time period ----
#-------------------------------------------#

#### Trips by temperature scatter ####

hourly_temp_n_plot <- 
    nyc_hourly %>% 
    ggplot(aes(temp_mean_hourly, n_hourly)) + 
    geom_point() + 
    geom_smooth(method = "loess") +
    # geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
    theme_bw()

daily_temp_n_plot <- 
    nyc_daily %>% 
    ggplot(aes(temp_mean_daily, n_daily)) + 
    geom_point() + 
    geom_smooth(method = "loess") +
    # geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
    theme_bw()

weekly_temp_n_plot <- 
    nyc_weekly %>% 
    ggplot(aes(temp_mean_weekly, n_weekly)) + 
    geom_point() + 
    geom_smooth(method = "loess") +
    # geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
    theme_bw()

monthly_temp_n_plot <- 
    nyc_monthly %>% 
    ggplot(aes(temp_mean_monthly, n_monthly)) + 
    geom_point() + 
    geom_smooth(method = "loess") +
    # geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
    theme_bw()


plot_grid(
    hourly_temp_n_plot,
    daily_temp_n_plot,
    weekly_temp_n_plot,
    monthly_temp_n_plot,
    nrow = 4,
    ncol = 4
)

# hourly_temp_n_plot
# daily_temp_n_plot
# weekly_temp_n_plot
# monthly_temp_n_plot


#### Temperature ####

hourly_temp_plot <- 
    nyc_hourly %>% 
    ggplot(aes(date_time_hourly, temp_mean_hourly)) + 
    geom_line() + 
    geom_smooth(method = "loess") +
    # geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
    theme_bw()

daily_temp_plot <- 
    nyc_daily %>% 
    ggplot(aes(date_time_daily, temp_mean_daily)) + 
    geom_line() + 
    geom_smooth(method = "loess") +
    # geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
    theme_bw()

weekly_temp_plot <- 
    nyc_weekly %>% 
    ggplot(aes(date_time_weekly, temp_mean_weekly)) + 
    geom_line() + 
    geom_smooth(method = "loess") +
    # geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
    theme_bw()

monthly_temp_plot <- 
    nyc_monthly %>% 
    ggplot(aes(date_time_monthly, temp_mean_monthly)) + 
    geom_line() + 
    geom_smooth(method = "loess") +
    # geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
    theme_bw()


plot_grid(
    hourly_temp_plot,
    daily_temp_plot,
    weekly_temp_plot,
    monthly_temp_plot,
    nrow = 4,
    ncol = 4
)

# hourly_temp_plot
# daily_temp_plot
# weekly_temp_plot
# monthly_temp_plot


#### Bike trips ####

hourly_n_plot <- 
    nyc_hourly %>% 
    ggplot(aes(date_time_hourly, n_hourly)) + 
    geom_line() + 
    geom_smooth(method = "loess") +
    # geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
    theme_bw()

daily_n_plot <- 
    nyc_daily %>% 
    ggplot(aes(date_time_daily, n_daily)) + 
    geom_line() + 
    geom_smooth(method = "loess") +
    # geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
    theme_bw()

weekly_n_plot <- 
    nyc_weekly %>% 
    ggplot(aes(date_time_weekly, n_weekly)) + 
    geom_line() + 
    geom_smooth(method = "loess") +
    # geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
    theme_bw()

monthly_n_plot <- 
    nyc_monthly %>% 
    ggplot(aes(date_time_monthly, n_monthly)) + 
    geom_line() + 
    geom_smooth(method = "loess") +
    # geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
    theme_bw()


plot_grid(
    hourly_n_plot,
    daily_n_plot,
    weekly_n_plot,
    monthly_n_plot,
    nrow = 4,
    ncol = 4
)

# hourly_n_plot
# daily_n_plot
# weekly_n_plot
# monthly_n_plot


#######################################################################################
#######################################################################################
