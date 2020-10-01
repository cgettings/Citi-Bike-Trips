###########################################################################################-
###########################################################################################-
##
## Station status text files to database ----
##
###########################################################################################-
###########################################################################################-

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
library(fs)
library(here)
library(glue)
library(iterators)
library(foreach)
library(doParallel)
library(tictoc)
library(zip)

#-----------------------------------------------------------------------------------------#
# Loading custom functions
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# dbWriteTable, but with "IGNORE" option
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# copied from: https://gist.github.com/jeffwong/5925000

# source(here("code/functions/db_insert_or_ignore.R"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# make `fromJSON()` robust to errors (e.g., empty files)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

fromJSON_possibly <-
    possibly(
        fromJSON,
        otherwise = tibble()
    )

#-----------------------------------------------------------------------------------------#
# Getting current station list
#-----------------------------------------------------------------------------------------#

station_list <- 
    fromJSON("https://gbfs.citibikenyc.com/gbfs/en/station_information.json")$data$stations %>% 
    as_tibble() %>% 
    select(
        station_id, 
        station_latitude = lat, 
        station_longitude = lon, 
        station_name = name
    ) %>% 
    mutate(station_id = as.integer(station_id))

#-----------------------------------------------------------------------------------------#
# Connecting to database
#-----------------------------------------------------------------------------------------#

citibike_trip_db <- dbConnect(SQLite(), here("data/citibike_trip_db.sqlite3"))

#-----------------------------------------------------------------------------------------#
# Getting date of most recent data
#-----------------------------------------------------------------------------------------#

database_has_station_status <- citibike_trip_db %>% db_has_table("station_status")

if (database_has_station_status) {
    
    query <- 
        sql(
            glue(
                "SELECT `year`, `month`, `day`",
                "FROM `station_status`",
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
        mutate(date = make_date(year, month, day)) %>% 
        pull(date)
    
} else {
    
    most_recent_day <- as_date("1900-01-01")
    
}


#-----------------------------------------------------------------------------------------#
# Listing raw data files
#-----------------------------------------------------------------------------------------#

local_dir <- here("data/station_status/raw")

file_list <- 
    dir_info(local_dir,
             recurse = FALSE,
             regexp = "[.]json") %>%
    arrange(path) %>%
    pull(path)

file_names <- 
    file_list %>%
    path_file() %>% 
    str_remove(".bz2") %>% 
    str_remove(".json")

file_dates <- 
    file_names %>% 
    str_extract("(\\[.*\\))") %>% 
    parse_date_time(orders = "ymd HMS")

station_status_files <- 
    tibble(file_list,
           file_names,
           file_dates) %>%
    arrange(desc(file_dates))


#-----------------------------------------------------------------------------------------#
# Finding most recent raw data file
#-----------------------------------------------------------------------------------------#

# Dates that are later than the latest in local folder

station_status_files_to_add <- 
    station_status_files %>% 
    filter(file_dates > most_recent_day) %>% 
    arrange(file_dates)


#=========================================================================================#
# Reading JSON data and saving to database ----
#=========================================================================================#

cl <- makePSOCKcluster(6)
registerDoParallel(cl)

#-----------------------------------------------------------------------------------------#
# Cleaning
#-----------------------------------------------------------------------------------------#

# setting initial values of range

chunk_size <- 24000
start      <- 1
end        <- chunk_size

# iterating through list of files, in 1000 file chunks, until there are no more files

repeat {
    
    files_subset <- 
        station_status_files_to_add$file_list[start:end] %>% 
        na.omit()
    
    # if no more files, break
    
    if (length(files_subset) == 0) break
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # Print some pretty details of your progress
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    cat("=========================================\n")
    cat("Loop:", end/chunk_size, "\n")
    cat("- - - - - - - - - - - - - - - - - - - - -\n")
    cat("start:", "|", as.character(station_status_files_to_add$file_dates[start]), "[", start, "]", "\n")
    cat("end:  ", "|", as.character(station_status_files_to_add$file_dates[end]), "[", end, "]", "\n")
    
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # cleaning the data
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    station_status_parsed <- 
        
        foreach(
            
            i = 1:length(files_subset),
            .combine = function(...) rbind(..., fill = TRUE),
            .multicombine = TRUE,
            .errorhandling = "pass",
            .inorder = FALSE,
            .packages = c("tidyverse", "jsonlite", "fs", "data.table")
            
        ) %dopar% {
            
            if (path_ext(files_subset[i]) == "json") {
                
                fromJSON_possibly(files_subset[i])$data$stations %>%
                    as_tibble() %>%
                    
                    # necessary to get data.table's rbind to work (i think)
                    
                    select(-where(is.list)) %>%
                    select_at(
                        vars(
                            -matches("rental_access_method"),
                            -matches("eightd"),
                            -matches("legacy_id")
                        )
                    ) %>% 
                    as.data.table()
                
            } else if (path_ext(files_subset[i]) == "bz2") {
                
                fromJSON_possibly(bzfile(files_subset[i]))$data$stations %>%
                    as_tibble() %>%
                    select(-where(is.list)) %>%
                    select_at(
                        vars(
                            -matches("rental_access_method"),
                            -matches("eightd"),
                            -matches("legacy_id")
                        )
                    ) %>% 
                    as.data.table()
                
            }
            
        }
    
    
    # if there are rows, mutate them
    
    if (nrow(station_status_parsed) > 0) {
        
        station_status <- 
            
            station_status_parsed %>% 
            
            as_tibble() %>% 
            
            mutate(
                
                station_id   = as.integer(station_id),
                
                last_reported = as_datetime(last_reported, tz = "US/Eastern"),
                date          = as_date(last_reported)     %>% as.integer(),
                year          = last_reported %>% year()   %>% as.integer(),
                month         = last_reported %>% month()  %>% as.integer(),
                day           = last_reported %>% day()    %>% as.integer(),
                hour          = last_reported %>% hour()   %>% as.integer(),
                minute        = last_reported %>% minute() %>% as.integer()
                
            ) %>% 
            
            # removing broken dates by removing years before 2010 (e.g., 1970)
            
            filter(year > 2010) %>% 
            
            distinct(station_id, last_reported, .keep_all = TRUE) %>% 
            
            left_join(., station_list, by = "station_id")
        
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        # Print some pretty details of your progress
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        
        cat("- - - - - - - - - - - - - - - - - - - - -\n")
        cat("Y:", unique(station_status$year), "M:", sort(unique(station_status$month)))
        cat("\n")
        
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        # writing to database
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        
        # tryCatch({
        
        dbWriteTable(
            citibike_trip_db,
            "station_status",
            value = station_status,
            append = TRUE,
            temporary = FALSE
        )
        
        # },
        
        # error = function(e) {
        #     
        #     db_insert_or_ignore(
        #         conn = citibike_trip_db,
        #         name = "station_status",
        #         value = station_status
        #     )
        
        # })
        
    }
    
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # Print some pretty details of your progress
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    cat("- - - - - - - - - - - - - - - - - - - - -\n")
    cat(nrow(station_status), "rows added")
    cat("\n")
    
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # updating range
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    start <- start + chunk_size
    end   <- end   + chunk_size
    
}

stopCluster(cl)

dbDisconnect(citibike_trip_db)

#-----------------------------------------------------------------------------------------#
# Cleaning up raw files ----
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# adding to archive
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

if (file_exists(here("data/station_status/raw/added_raw_files.zip"))) {
    
    zipr_append(
        zipfile = here("data/station_status/raw/added_raw_files.zip"), 
        files = station_status_files_to_add$file_list,
        compression_level = 4
    )
    
} else {
    
    zipr(
        zipfile = here("data/station_status/raw/added_raw_files.zip"), 
        files = station_status_files_to_add$file_list,
        compression_level = 4
    )
    
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# deleting
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

file_delete(station_status_files_to_add$file_list)

#-----------------------------------------------------------------------------------------#
# Creating indexes ----
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Connecting to database
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

citibike_trip_db <- dbConnect(SQLite(), here("data/citibike_trip_db.sqlite3"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# listing indexes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

index_tbl <- 
    citibike_trip_db %>% 
    dbGetQuery(
        "SELECT * FROM sqlite_master WHERE type = 'index'"
    ) %>% 
    as_tibble()


if (all(index_tbl$tbl_name != "station_status")) {
    
    citibike_trip_db %>% db_create_index("station_status", "station_id")
    citibike_trip_db %>% db_create_index("station_status", "last_reported")
    
    citibike_trip_db %>% 
        db_create_index(
            "station_status", 
            c("station_id", "last_reported")
        )
    
    citibike_trip_db %>% db_create_index("station_status", "date")
    citibike_trip_db %>% db_create_index("station_status", "year")
    citibike_trip_db %>% db_create_index("station_status", "month")
    citibike_trip_db %>% db_create_index("station_status", "day")
    citibike_trip_db %>% db_create_index("station_status", "hour")
    citibike_trip_db %>% db_create_index("station_status", "minute")
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # vacuuming
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    citibike_trip_db %>% dbExecute("VACUUM")
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # checking indexes
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    citibike_trip_db %>% 
        dbGetQuery(
            "SELECT * FROM sqlite_master WHERE type = 'index'"
        ) %>% 
        as_tibble()
    
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# disconnecting from database
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

dbDisconnect(citibike_trip_db)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
