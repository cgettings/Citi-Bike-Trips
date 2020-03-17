##########################################################################################-
##########################################################################################-
##
## Downloading trip data ----
##
##########################################################################################-
##########################################################################################-

#=========================================================================================#
# Setting up ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Loading libraries
#-----------------------------------------------------------------------------------------#

library(lubridate)
library(tidyverse)
library(fs)
library(glue)
library(here)

#-----------------------------------------------------------------------------------------#
# Getting names of existing data files (from raw)
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Paths
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

data_paths <- 
    dir_ls(
        glue("{here()}/data/raw"), 
        recurse = FALSE, 
        regexp = ".csv.zip")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Just file names
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

data_names <- 
    data_paths %>% 
    path_file() %>% 
    path_ext_remove() %>% 
    path_ext_remove()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Parsing names to extract dates
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

data_dates <- 
    data_names %>% 
    str_remove("([:punct:]|[:space:])citibike.*") %>% 
    str_remove("JC([:punct:]|[:space:])") %>% 
    parse_date_time(orders = c("ym"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Putting them all together
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

data_files <- 
    tibble(
        data_names,
        data_paths,
        data_dates
    ) %>%
    arrange(desc(data_dates))

#-----------------------------------------------------------------------------------------#
# Getting names of existing data files (from database)
#-----------------------------------------------------------------------------------------#

# Getting latest date (`slice(1)`), then using that as the first date in a sequence ending with `today()`

download_dates <- 
    data_files %>% 
    slice(1) %>% 
    pull(data_dates) %>% 
    # + months(1) %>% 
    # seq(., as_date(now()), "1 month") %>% 
    seq(as_date("2019-09-30 EDT"), as_date(ceiling_date(now(), "month") - months(1)), "1 month") %>% 
    as_date()


database_exists <- citibike_trip_db %>% db_has_table("citibike_trips")

if (database_exists) {
    
    #-----------------------------------------------------------------------------------------#
    # Setting search parameters
    #-----------------------------------------------------------------------------------------#
    
    most_recent_datetime <- 
        citibike_trip_db %>% 
        tbl("citibike_trips") %>% 
        select(updated_at) %>% 
        arrange(desc(updated_at)) %>% 
        head(1) %>%
        pull() %>% 
        as_datetime(tz = "US/Eastern") %>% 
        floor_date(unit = "day")
    
    most_recent_datetime_numeric <- 
        as.numeric(most_recent_datetime)
    
    most_recent_day <- 
        most_recent_datetime %>% 
        as_date()
    
} else {
    
    most_recent_day <- as_date("1900-01-01")
    
}


#=========================================================================================#
# Downloading files ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Constructing download links ----
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# JC
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trip_files_jc <- 
    glue(
        "https://s3.amazonaws.com/tripdata/",
        "JC-{year(download_dates)}{download_dates %>% format('%m')}",
        "-citibike-tripdata.csv.zip"
    )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# NYC
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trip_files_nyc <- 
    glue(
        "https://s3.amazonaws.com/tripdata/",
        "{year(download_dates)}{download_dates %>% format('%m')}",
        "-citibike-tripdata.csv.zip"
    )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# NYC
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trip_files <- c(trip_files_jc, trip_files_nyc)

#-----------------------------------------------------------------------------------------#
# Downloading ----
#-----------------------------------------------------------------------------------------#

# If method = "libcurl", you can provide a vector for url and destfile and they will
#   download simultaneously

download.file(
    url = trip_files,
    destfile = glue("{here()}/data/raw/{path_file(trip_files)}"),
    mode = "wb",
    method = "libcurl"
)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
