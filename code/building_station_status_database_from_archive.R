###########################################################################################-
###########################################################################################-
##
## Building station status database from Google Drive archive ---
##
###########################################################################################-
###########################################################################################-

# This script takes data downloaded from Google Drive archive created by Justin Tyndall,
#   and available here:
#   https://drive.google.com/drive/u/0/folders/0B6H9nKo1G98uS3kxQ1VrNGt5SjA
#   https://drive.google.com/drive/u/0/folders/1aLRu3GYHTVFG9BBxPUQM4N7xhbZNMxPV

#=========================================================================================#
# Setting up ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Loading libraries ----
#-----------------------------------------------------------------------------------------#

library(jsonlite)
library(tidyverse)
library(glue)
library(lubridate)
library(fs)
library(DBI)
library(dbplyr)
library(RSQLite)
library(here)

#-----------------------------------------------------------------------------------------#
# Connecting to database
#-----------------------------------------------------------------------------------------#

citibike_trip_db <- dbConnect(SQLite(), "data/citibike_trip_db.sqlite3")

#-----------------------------------------------------------------------------------------#
# Listing raw data files
#-----------------------------------------------------------------------------------------#

local_dir <- here("data/station_status")

file_list <- 
    dir_info(
        local_dir,
        recurse = TRUE,
        regexp = "bikeshare_nyc_raw"
    ) %>% 
    as_tibble()


#=========================================================================================#
# Extracting data and saving to database ----
#=========================================================================================#

for (i in 1:nrow(file_list)) {
    
    #-----------------------------------------------------------------------------------------#
    # Print some pretty details of your progress
    #-----------------------------------------------------------------------------------------#
    
    cat("===============================\n")
    cat("Loop", i)
    cat("\n-------------------------------\n")
    cat("|-- ")
    cat(file_list$path[i] %>% str_split("/") %>% map_chr(~nth(.x, -2L)))
    cat(" --|")
    
    #-----------------------------------------------------------------------------------------#
    # Reading in data
    #-----------------------------------------------------------------------------------------#
    
    station_status <- 
        
        suppressWarnings(
            read_tsv(
                file_list$path[i], 
                col_types = cols(.default = col_guess()),
                progress = FALSE
            )
        ) %>% 
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        # cleaning the data
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        
        # renaming variables to fit with my naming scheme
        
        rename(
            station_id          = dock_id,
            station_name        = dock_name,
            station_longitude   = `_long`,
            station_latitude    = `_lat`,
            num_bikes_available = avail_bikes,
            num_docks_available = avail_docks
        ) %>% 
        
        select(-matches("\\d")) %>% 
        select(1:13) %>% 
        
        mutate(
            
            station_id          = as.integer(station_id),
            num_bikes_available = as.integer(num_bikes_available),
            num_docks_available = as.integer(num_docks_available),
            
            date = date %>% fast_strptime(format = "%y-%m-%d", tz = "US/Eastern") %>% as_date(),  
            
            hour          = as.integer(hour),
            hour          = if_else(pm == 0L, hour - 1L, hour + 11L),
            year          = year(date)  %>% as.integer(),
            month         = month(date) %>% as.integer(),
            day           = day(date)   %>% as.integer(),
            minute        = as.integer(minute),
            last_reported = make_datetime(year, month, day, hour, minute, tz = "US/Eastern"),
            
            station_status = if_else(in_service == 1, "active", "out_of_service")
            
        ) %>% 
        
        select(-c(in_service, pm, status_key, tot_docks))
        
        
    #-----------------------------------------------------------------------------------------#
    # writing to database
    #-----------------------------------------------------------------------------------------#
    
    dbWriteTable(
        citibike_trip_db,
        "station_status",
        value = station_status,
        append = TRUE,
        temporary = FALSE
    )
    
    #-----------------------------------------------------------------------------------------#
    # Print some pretty details of your progress
    #-----------------------------------------------------------------------------------------#
    
    cat("\n-------------------------------\n")
    cat(nrow(station_status), "rows added")
    cat("\n")
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # removing the previous data from memory to avoid duplicates
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    rm(station_status)
    
}

#=========================================================================================#
# Creating indexes ----
#=========================================================================================#

index_tbl <- 
    dbGetQuery(
        citibike_trip_db, 
        "SELECT * FROM sqlite_master WHERE type = 'index'"
    ) %>% 
    as_tibble()


if (any(!index_tbl$tbl_name == "station_status")) {
    
    db_create_index(citibike_trip_db, "station_status", "station_id")
    db_create_index(citibike_trip_db, "station_status", "last_reported")
    
    db_create_index(
        citibike_trip_db, 
        "station_status", 
        c("station_id", "last_reported")
    )
    
    db_create_index(citibike_trip_db, "station_status", "year")
    db_create_index(citibike_trip_db, "station_status", "month")
    db_create_index(citibike_trip_db, "station_status", "day")
    db_create_index(citibike_trip_db, "station_status", "hour")
    db_create_index(citibike_trip_db, "station_status", "minute")
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # vacuuming
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    dbExecute(citibike_trip_db, "VACUUM")
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # checking indexes
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    dbGetQuery(
        citibike_trip_db, 
        "SELECT * FROM sqlite_master WHERE type = 'index'"
    ) %>% 
        as_tibble()
    
}

#-----------------------------------------------------------------------------------------#
# disconnecting from database
#-----------------------------------------------------------------------------------------#

dbDisconnect(citibike_trip_db)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



