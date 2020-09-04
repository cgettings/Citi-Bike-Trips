###########################################################################################-
###########################################################################################-
##
## Creating Citi Bike trips database ----
##
###########################################################################################-
###########################################################################################-

#=========================================================================================#
# Setting up ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Loading libraries
#-----------------------------------------------------------------------------------------#

library(tidyverse)
library(lubridate)
library(DBI)
library(dbplyr)
library(RSQLite)
library(fs)
library(here)
library(glue)

#-----------------------------------------------------------------------------------------#
# Connecting to database
#-----------------------------------------------------------------------------------------#

citibike_trip_db <- dbConnect(SQLite(), "data/citibike_trip_db.sqlite3")

#-----------------------------------------------------------------------------------------#
# Getting date of most recent data
#-----------------------------------------------------------------------------------------#

database_has_citibike_trips <- citibike_trip_db %>% db_has_table("citibike_trips")

if (database_has_citibike_trips) {
    
    query <- 
        sql(
            glue(
                "SELECT `year`, `month`, `day`",
                "FROM `citibike_trips`",
                "ORDER BY `year` DESC, `month` DESC, `day` DESC",
                .sep = " "
            )
        )
    
    #-----------------------------------------------------------------------------------------#
    # Setting search parameters
    #-----------------------------------------------------------------------------------------#
    
    most_recent_day <- 
        citibike_trip_db %>% 
        db_collect(query, n = 1) %>% 
        mutate(date = str_c(year, month, day, sep = "-") %>% as_date()) %>% 
        pull(date)
    
} else {
    
    most_recent_day <- as_date("1900-01-01")
    
}


#-----------------------------------------------------------------------------------------#
# Listing raw data files
#-----------------------------------------------------------------------------------------#

local_dir <- str_c(here(), "/data/raw/")

trip_file_list <- 
    dir_info(local_dir,
             recurse = FALSE,
             regexp = "[.]zip") %>%
    filter(str_detect(path, regex("jc", ignore_case = TRUE), negate = TRUE)) %>%
    filter(str_detect(path, "citibike")) %>%
    filter(str_detect(path, "nyc", negate = TRUE)) %>%
    arrange(desc(birth_time)) %>%
    pull(path) %>%
    path_file()

trip_file_names <- 
    trip_file_list %>% 
    path_ext_remove() %>% 
    path_ext_remove()

trip_file_dates <- 
    trip_file_names %>% 
    str_extract("(\\d{4,8})") %>% 
    parse_date_time(orders = c("ym", "y")) %>% 
    as_date()

trip_files <- 
    tibble(trip_file_list,
           trip_file_names,
           trip_file_dates) %>%
    arrange(desc(trip_file_dates))

#-----------------------------------------------------------------------------------------#
# Finding most recent raw data file
#-----------------------------------------------------------------------------------------#

# Dates that are later than the latest in local folder

trip_files_to_add <- 
    trip_files %>% 
    filter(trip_file_dates > most_recent_day) %>% 
    arrange(trip_file_dates)

#=========================================================================================#
# Extracting trip data and saving to database ----
#=========================================================================================#

local_dir_unzipped <- str_c(local_dir, "unzipped")

dir_create(local_dir_unzipped)

for (i in 1:nrow(trip_files_to_add)) {
    
    #-----------------------------------------------------------------------------------------#
    # Print some pretty details of your progress
    #-----------------------------------------------------------------------------------------#
    
    cat("===============================\n")
    cat("Loop", i, "\n")
    cat(trip_files_to_add$trip_file_names[i], "\n")
    
    #-----------------------------------------------------------------------------------------#
    # Unzipping station files
    #-----------------------------------------------------------------------------------------#
    
    trip_csv <- 
        unzip(
            zipfile = str_c(local_dir, trip_files_to_add$trip_file_list[i]),
            list = TRUE,
            exdir = local_dir_unzipped) %>%
        slice(1) %>% 
        pull(Name)
    
    unzip(
        zipfile = str_c(local_dir, trip_csv),
        exdir = local_dir_unzipped,
        unzip = "unzip"
    )
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # cleaning the data
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    trip_unzip <- 
        
        read_csv(str_c(local_dir_unzipped, trip_csv, sep = "/")) %>%
        `attr<-`("problems", NULL) %>% 
        
        rename_all( ~ str_replace_all(.x, " ", "_")) %>% 
        
        rename(
            trip_duration = tripduration,
            bike_id       = bikeid,
            user_type     = usertype,
            start_time    = starttime,
            stop_time     = stoptime
        ) %>% 
            
        mutate(
            start_time = 
                start_time %>% 
                parse_date_time(
                    orders = c("ymd HMS", "mdy HMS"), 
                    tz = "US/Eastern"
                ),
            
            stop_time = 
                stop_time %>% 
                parse_date_time(
                    orders = c("ymd HMS", "mdy HMS"), 
                    tz = "US/Eastern"
                ),
            
            year  = start_time %>% year()  %>% as.integer(),
            month = start_time %>% month() %>% as.integer(),
            day   = start_time %>% day()   %>% as.integer(),
            
            start_station_id = as.integer(start_station_id),
            end_station_id   = as.integer(end_station_id),
            birth_year       = as.integer(birth_year),
            bike_id          = as.integer(bike_id)
            
        )
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # writing to database
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    dbWriteTable(
        citibike_trip_db,
        "citibike_trips",
        value = trip_unzip,
        append = TRUE,
        temporary = FALSE
    )
    
    #-----------------------------------------------------------------------------------------#
    # Print some pretty details of your progress
    #-----------------------------------------------------------------------------------------#
    
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
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # removing the previous data from memory to avoid duplicates
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    rm(trip_unzip)
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # removing the unzipped files
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    file_delete(str_c(local_dir_unzipped, trip_csv, sep = "/"))
    
    gc()
    
}

#-----------------------------------------------------------------------------------------#
# Creating indexes ----
#-----------------------------------------------------------------------------------------#

index_tbl <- 
    dbGetQuery(
        citibike_trip_db, 
        "SELECT * FROM sqlite_master WHERE type = 'index'"
    ) %>% 
    as_tibble()


if (all(index_tbl$tbl_name != "citibike_trips")) {
    
    db_create_index(citibike_trip_db, "citibike_trips", "year")
    db_create_index(citibike_trip_db, "citibike_trips", "month")
    db_create_index(citibike_trip_db, "citibike_trips", "day")
    db_create_index(citibike_trip_db, "citibike_trips", "start_station_id")
    db_create_index(citibike_trip_db, "citibike_trips", "end_station_id")
    db_create_index(citibike_trip_db, "citibike_trips", "bike_id")
    db_create_index(citibike_trip_db, "citibike_trips", "user_type")
    db_create_index(citibike_trip_db, "citibike_trips", "birth_year")
    db_create_index(citibike_trip_db, "citibike_trips", "gender")
    
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
