############################################################-
############################################################-
##
## Creating Citi Bike trips database ----
##
############################################################-
############################################################-

#===========================================================#
# Setting up ----
#===========================================================#

#-----------------------------------------------------------#
# Loading libraries ----
#-----------------------------------------------------------#

library(tidyverse)
library(lubridate)
library(DBI)
library(dbplyr)
library(magrittr)
library(RSQLite)


#-----------------------------------------------------------#
# Connecting to database
#-----------------------------------------------------------#

citibike_trip_db <- dbConnect(SQLite(), "./data/citibike_trip_db.sqlite3")


#===========================================================#
# Getting data frame of file names to extract ----
#===========================================================#

trip_files_all <- list.files("./data/raw/", full.names = TRUE)

trip_files_extract <- 
    trip_files_all %>% 
    .[str_detect(., "\\.zip")] %>% 
    .[!str_detect(., "JC-")] %>% 
    data_frame(files = .) %>% 
    filter(str_detect(files, regex(pattern = "201806")))


#===========================================================#
# Extracting trip data and saving to database ----
#===========================================================#

for (i in 1:nrow(trip_files_extract)) {
    
    #-----------------------------------------------------------#
    # Unzipping station files
    #-----------------------------------------------------------#
    
    trip_csv <- 
        unzip(
            zipfile = trip_files_extract$files[i],
            list = TRUE,
            exdir = "./data/raw/Unzipped") %>%
        extract2("Name")
    
    unzip(
        zipfile = trip_files_extract$files[i],
        exdir = "./data/raw/Unzipped"
    )
    
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # cleaning the data
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    trip_unzip <- 
        
        read_csv(
            paste0(
                "./data/raw/Unzipped/", 
                trip_csv)
            # col_types = "iccicddicddicci"
            ) %>%
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
    
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # writing to database
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    dbWriteTable(
        citibike_trip_db,
        "citibike_trips",
        value = trip_unzip,
        append = TRUE,
        temporary = FALSE
    )
    
    
    #-----------------------------------------------------------#
    # Print some pretty details of your progress
    #-----------------------------------------------------------#
    
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
    
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # removing the previous data from memory to avoid duplicates
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    rm(trip_unzip)
    
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # removing the unzipped files
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    file.remove(paste0("./data/raw/Unzipped/", trip_csv))
    
    gc()
    
}


#-----------------------------------------------------------#
# Creating indexes ----
#-----------------------------------------------------------#

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


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# checking indexes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

dbGetQuery(citibike_trip_db, "SELECT * FROM sqlite_master WHERE type = 'index'")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# disconnecting from database
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

dbDisconnect(citibike_trip_db)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #             ---- THIS IS THE END ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
