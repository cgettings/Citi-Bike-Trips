##################################################-
##################################################-
##
## Creating time series for April ----
##
##################################################-
##################################################-

#===========================================================#
# Setting up ----
#===========================================================#

#-----------------------------------------------------------#
# Loading libraries ----
#-----------------------------------------------------------#

library(lubridate)
library(tidyverse)
library(forecast)
library(DBI)
library(RSQLite)
library(tibbletime)
library(magrittr)

#-----------------------------------------------------------#
# Loading data from RDS ----
#-----------------------------------------------------------#

### Trips ###

march_april_may_trips <- read_rds("./data/march_april_may/march_april_may_trips.rds")

### Hourly ###

# march_april_may_trips_hourly <- read_rds("./data/march_april_may/march_april_may_trips_hourly.rds")

#-----------------------------------------------------------#
# Connecting to database ----
#-----------------------------------------------------------#

citibike_trip_db <- dbConnect(SQLite(), "./data/citibike_trip_db.sqlite3")

#===========================================================#
# Summarizing by hour ----
#===========================================================#

#-----------------------------------------------------------#
# march_april_may ----
#-----------------------------------------------------------#


### Pulling out of database ###

march_april_may_trips <- 
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


### Writing RDS ###

write_rds(
    march_april_may_trips,
    "./data/march_april_may/march_april_may_trips.rds",
    compress = "gz",
    compression = 9
)


### Summarizing ###

march_april_may_trips_hourly_count <- 
    march_april_may_trips %>% 
    count(start_time_hourly) %>% 
    rename(trips = n)

write_rds(march_april_may_trips_hourly_count, "./data/march_april_may/march_april_may_trips_hourly_count.rds")


march_april_may_trips_hourly_duration <- 
    march_april_may_trips %>% 
    group_by(start_time_hourly) %>% 
    summarise(average_trip_duration = mean(trip_duration))

write_rds(march_april_may_trips_hourly_duration, "./data/march_april_may/march_april_may_trips_hourly_duration.rds")


march_april_may_trips_hourly <- 
    full_join(
        march_april_may_trips_hourly_count, 
        march_april_may_trips_hourly_duration,
        by = "start_time_hourly")


write_rds(march_april_may_trips_hourly, "./data/march_april_may/march_april_may_trips_hourly.rds")

rm(march_april_may_trips)
rm(march_april_may_trips_hourly_count)
rm(march_april_may_trips_hourly_duration)
gc()


#===========================================================#
# Playing with time series ----
#===========================================================#

#-----------------------------------------------------------#
# Plotting data ----
#-----------------------------------------------------------#


#### First week of April ####

first_day <- as_datetime("2018-04-01 00:00:00", tz = "US/Eastern")
last_day  <- as_datetime("2018-04-07 24:00:00", tz = "US/Eastern")

date_breaks <- as_datetime(seq(first_day, last_day, 6*60*60), tz = "US/Eastern")

march_april_may_trips_hourly %>% 
    filter(day(start_time_hourly) %in% 1:7) %>%
    ggplot(aes(x = start_time_hourly, y = trips)) + 
    geom_vline(
        xintercept = seq(first_day, last_day, "day"),
        color = "gray50") +
    geom_point(color = "blue", shape = 16) +
    geom_line() +
    scale_x_datetime(
        breaks = date_breaks,
        date_labels = "%a-%H:%M"
        ) +
    coord_cartesian(
        expand = FALSE, 
        xlim = c(first_day - minutes(60), 
                 last_day + minutes(60)),
        ylim = c(0, 5000)
    ) +
    theme_bw() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12)
        )


#### First week of April ####

march_april_may_trips_hourly %>% 
    # filter(day(start_time_hourly) %in% 1:7) %>%
    ggplot(aes(x = start_time_hourly, y = trips)) + 
    # geom_vline(
    #     xintercept = seq(first_day, last_day, "day"),
    #     color = "gray50") +
    geom_point(color = "blue", shape = 16) +
    geom_line() +
    # scale_x_datetime(
    #     breaks = date_breaks,
    #     date_labels = "%a-%H:%M"
    #     ) +
    # coord_cartesian(
    #     expand = FALSE, 
    #     xlim = c(first_day - minutes(60), 
    #              last_day + minutes(60)),
    #     ylim = c(0, 5000)
    # ) +
    theme_bw() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12)
        )

#-----------------------------------------------------------#
# Creating Multi-Seasonal Time Series ----
#-----------------------------------------------------------#

## Creating a multi-seasonal time series with seasonal periods of
##  24 hours, and 7 days (168 hours), with observations every
##  1 hour (freq = 24)


march_april_may_trips_msts <- 
    march_april_may_trips_hourly %$%
    msts(
        trips,
        seasonal.periods = c(24, 24 * 7),
        start = c(0, 0),
        ts.frequency = 24)


### Adding msts column into data frame ###

march_april_may_trips_hourly_msts <- 
    march_april_may_trips_hourly %>% 
    as_tbl_time(index = start_time_hourly) %>% 
    add_column(msts = march_april_may_trips_msts)


### Fitting TBATS model ###

# march_april_may_trips_tbats <- 
#     march_april_may_trips_hourly_msts %$%
#     tbats(msts,
#           use.trend = TRUE,
#           use.damped.trend = FALSE)


### Plotting TBATS model ###

# march_april_may_trips_tbats %>% plot()


### Fitting multi-seasonal STL decomposition ###

march_april_may_trips_hourly_mstl_7 <- 
    march_april_may_trips_hourly_msts %$%
    mstl(msts, iterate = 5, lambda = NULL, s.window = 7)


### Plotting MSTL ###

march_april_may_trips_hourly_mstl_7 %>%
    autoplot(facet = TRUE) +
    theme_bw()


### Forecast ###

mstl_7 <- forecast(march_april_may_trips_hourly_mstl_7, h = 24*7)
mstl_7 %>% autoplot()

mstl_7_2 <- dshw(march_april_may_trips_hourly_mstl_7, h = 24*7)
mstl_7_2 %>% autoplot()



##################################################################
##################################################################


march_april_may_trips_hourly_msts %$% ggtsdisplay(x = msts, plot.type = "partial", theme = theme_bw())

fit <- march_april_may_trips_hourly_msts %$% tslm(msts ~ trend + season)

summary(fit)

march_april_may_trips_hourly_msts %$% seasonaldummy(x = msts)


march_april_may_trips_hourly_msts %$% HoltWinters(x = msts)


