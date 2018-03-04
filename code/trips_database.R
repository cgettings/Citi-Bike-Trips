##################################################-
##################################################-
##
## Creating Citi bike trips database ----
##
##################################################-
##################################################-


#========================================#
# Setting up ----
#========================================#

#---------------------------------#
# Loading libraries ----
#---------------------------------#


library(readr)
library(tibble)
library(lubridate)
library(DBI)
library(stringr)
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

#---------------------------------#
# Initializing database
#---------------------------------#

citibike_trip_db <- dbConnect(RSQLite::SQLite(), "./data/citibike_trip_db.sqlite3")


#========================================#
# Finding missing dates and times ----
#========================================#


# all_hours <-
#     data_frame(
#         start_time_hourly =
#             seq(
#                 as_datetime("2017-01-01 00:00:00", tz = "US/Eastern"),
#                 as_datetime("2017-12-31 23:00:00", tz = "US/Eastern"),
#                 "hour"
#             ))
# 
# 
# missing_hours <- 
#     tbl(citibike_trip_db, "citibike_trips") %>%
#     filter(year == 2017) %>%
#     select(start_time) %>%
#     collect() %>%
#     mutate(
#         start_time            = as_datetime(start_time, tz = "US/Eastern"),
#         start_time_hourly     = floor_date(start_time, "hours")
#     ) %>% 
#     count(start_time_hourly) %>% 
#     left_join(all_hours, .) %>% 
#     filter(is.na(n)) %>% 
#     mutate(start_time_hourly_num = as.numeric(start_time_hourly))
# 
# 
# missing_months <- 
#     missing_hours %>% 
#     mutate(
#         start_time_month = floor_date(start_time_hourly, "months") %>% 
#             as_date() %>% 
#             format.Date(x = ., format = "%Y%m")
#     ) %>% 
#     distinct(start_time_month)


#====================================================#
# Getting data frame of file names to extract ----
#====================================================#


# file_dates <- 
#     missing_months %>% 
#     pull(start_time_month) %>% 
#     str_c(., collapse = "|")

trip_files <- list.files("./data/raw/", full.names = TRUE)

trip_files.2 <- 
    trip_files %>% 
    .[str_detect(., "\\.zip")] %>% 
    # .[str_detect(., regex(file_dates, ignore_case = TRUE))] %>% 
    .[!str_detect(., "JC-")] %>% 
    data_frame(trip_files = .)


#====================================================#
# Extracting trip data and saving to database ----
#====================================================#


for (i in 1:nrow(trip_files.2)) {
    
    
    ### Unzipping station files ###
    
    trip_csv <- 
        unzip(
            zipfile = trip_files.2$trip_files[i],
            list = TRUE,
            exdir = "./data/raw/Unzipped") %>%
        extract2("Name")
    
    unzip(
        zipfile = trip_files.2$trip_files[i],
        exdir = "./data/raw/Unzipped"
    )
    
    
    ## cleaning the data ##
    
    trip_unzip <- 
        
        read_csv(
            paste0(
                "./data/raw/Unzipped/", 
                trip_csv),
            col_types = "iccicddicddicci") %>%
        `attr<-`("problems", NULL) %>% 
        
        as_tibble() %>% 
        rename_all(.funs = "tolower") %>% 
        rename_all(.funs = "str_replace_all", pattern = " ", replacement = "_") %>% 
        rename_all(.funs = "str_replace_all", pattern = "_", replacement = "") %>% 
        
        transmute(
            trip_duration = tripduration,
            
            start_time = starttime %>% 
                parse_date_time(
                    orders = c("ymd HMS", "mdy HMS"), 
                    tz     = "US/Eastern"),
            
            stop_time = stoptime %>% 
                parse_date_time(
                    orders = c("ymd HMS", "mdy HMS"), 
                    tz     = "US/Eastern"),
            
            year  = start_time %>% year(),
            month = start_time %>% month(),
            day   = start_time %>% day(),
            
            start_station_id        = as.integer(startstationid),
            start_station_name      = as.character(startstationname),
            start_station_latitude  = as.double(startstationlatitude),
            start_station_longitude = as.double(startstationlongitude),
            end_station_id          = as.integer(endstationid),
            end_station_name        = as.character(endstationname),
            end_station_latitude    = as.double(endstationlatitude),
            end_station_longitude   = as.double(endstationlongitude),
            bike_id                 = bikeid,
            user_type               = usertype,
            birth_year              = as.integer(birthyear),
            gender                  = gender
        )
    
    
    ## selecting missing hours ##
    
    
    trip_unzip_0 <- 
        trip_unzip %>% 
        mutate(
            start_time_hourly     = floor_date(start_time, "hours"),
            start_time_hourly_num = as.numeric(start_time_hourly)
        ) %>% 
        inner_join(., missing_hours, by = "start_time_hourly_num")
    
    inner_join(trip_unzip_0, missing_hours, by = "start_time_hourly_num") %>% glimpse()
    
    missing_hours
    
    
    ## writing to database ##
    
    dbWriteTable(
        citibike_trip_db,
        "citibike_trips",
        value = trip_unzip,
        append = TRUE,
        temporary = FALSE
    )
    
    
    ### Print some pretty details of your progress ###
    
    cat("===============================\n")
    cat("Loop", i, "\n")
    cat(nrow(trip_unzip), "rows added")
    cat("\n-------------------------------\n")
    cat("|-- ")
    cat(
        trip_unzip$start_time %>% year() %>% unique(), "-", 
        trip_unzip$start_time %>% month() %>% unique(), sep = "")
    cat(" --|")
    cat("\n-------------------------------\n")  
    
    ## removing the previous data from memory to avoid duplicates ##
    
    rm(trip_unzip)
    
    ## removing the unzipped files ##
    
    file.remove(paste0("./data/raw/Unzipped/", trip_csv))
    
    gc()
    
}


#---------------------------------#
# ---- Creating indexes ----
#---------------------------------#

db_create_index(citibike_trip_db, "citibike_trips", "year")
db_create_index(citibike_trip_db, "citibike_trips", "month")
db_create_index(citibike_trip_db, "citibike_trips", "day")
db_create_index(citibike_trip_db, "citibike_trips", "start_station_id")
db_create_index(citibike_trip_db, "citibike_trips", "end_station_id")
db_create_index(citibike_trip_db, "citibike_trips", "bike_id")
db_create_index(citibike_trip_db, "citibike_trips", "user_type")
db_create_index(citibike_trip_db, "citibike_trips", "birth_year")
db_create_index(citibike_trip_db, "citibike_trips", "gender")

db_list_tables(citibike_trip_db)

### Checking indexes ### 

dbGetQuery(citibike_trip_db, "SELECT * FROM sqlite_master WHERE type = 'index'")

#######################

dbDisconnect(citibike_trip_db)

############################################################
############################################################
