#######################################################-
#######################################################-
##
## Downloading Weather based on cities ----
##
#######################################################-
#######################################################-

#========================================#
#### Setting up ####
#========================================#

#---------------------------------#
# Loading libraries ----
#---------------------------------#


library(jsonlite)
library(lubridate)
library(dplyr)
library(tibble)
library(readr)
library(purrr)
library(DBI)
library(tidyr)
library(magrittr)
library(glue)
library(rvest)
library(stringr)
library(gepaf)
library(dbplyr)
library(V8)
library(httr)
library(countyweather)
library(rnoaa)
library(weathermetrics)


#---------------------------------#
# General parameters ----
#---------------------------------#

base_url   <- "https://www.ncdc.noaa.gov/access-data-service/api/v1/data"
dataset    <- "global-hourly"
cities     <- "New York"
noaa_token <- read_lines("./noaa_token.txt")

#========================================#
## Downloading ----
#========================================#


### Load tbl of NOAA ISD/ISH stations ###

stations_tbl <- isd_stations(refresh = FALSE)


### Selecting Central Park station ###

central_park_station <- 
    stations_tbl %>% 
    filter(icao == "KNYC" & str_sub(end, 1, 4) == 2018) %>% 
    mutate(station_code = str_c(usaf, wban))

station_code <- central_park_station %>% pull(station_code)

#---------------------------------------#
# Getting data from selected cities ----
#---------------------------------------#

### Specify the time and date range ###

start_date <- as_date("2016/01/01") %>% format("%Y-%m-%d")
start_time <- "00:00:00"
start_offset <- "-05" # Time zone
end_date <- as_date("2016/12/31") %>% format("%Y-%m-%d")
end_time <- "23:59:59"
end_offset <- "-05" # Time zone


### Query the API ###

GET(
    glue(
        "{base_url}",
        "?dataset={dataset}&",
        "startDate={start_date}T{start_time}{start_offset}:00&",
        "endDate={end_date}T{end_time}{end_offset}:00&",
        "stations={station_code}&",
        "includeAttributes=true&includeStationName=true&includeStationLocation=true"), 
    timeout(120), add_headers(token = noaa_token)
) %>%
    extract2("content") %>%
    write_file("./data/weather/nyc_weather_2016.csv")


#========================================#
## Cleaning ----
#========================================#


### Read in saved file ###

nyc_weather <- 
    read_csv("./data/weather/nyc_weather_2016.csv") %>% 
    
    # Drop variables with fewer than 3 letters in their name
    select_if(str_detect(names(.), regex("[:alpha:]{3,}"))) %>% 
    
    # Remove duplicate columns
    select_if(!str_detect(names(.), "_1"))


### Extract measurements and put into new variables ###

nyc_weather_clean <- 
    nyc_weather %>% 
    mutate(
        temperature = (str_sub(TMP, start = 1L, end = 5L) %>% as.numeric())/10, # Celsius
        dew_point   = (str_sub(DEW, start = 1L, end = 5L) %>% as.numeric())/10, # Celsius
        pressure    =  str_sub(SLP, start = 1L, end = 5L) %>% as.numeric(),     # hectopascals
        wind_speed  = (str_sub(WND, start = 9L, end = 12L) %>% as.numeric())/10 # meters per second
        ) %>% 
    mutate(
        temperature = na_if(temperature, 999.9),
        pressure    = na_if(pressure, 99999),
        wind_speed  = na_if(wind_speed, 999.9),
        dew_point   = na_if(dew_point, 999.9),
        humidity    = dewpoint.to.humidity(dew_point, temperature, "celsius")
        ) %>% 
    select(
        date_time = DATE,
        name      = NAME,
        usaf_wban = STATION,
        callsign  = CALL_SIGN,
        lat       = LATITUDE,
        lon       = LONGITUDE,
        elevation = ELEVATION,
        temperature,
        dew_point,
        humidity,
        pressure,
        wind_speed
    ) %>% 
    mutate(
        date_time        = force_tz(date_time, tz = "US/Eastern"),
        date_time_hourly = floor_date(date_time, "hours")
        )

### Save cleaned data ###

write_csv(nyc_weather_clean, "./data/weather/nyc_weather_2016_clean.csv")
write_rds(nyc_weather_clean, "./data/weather/nyc_weather_2016_clean.rds")


############################################################
############################################################

