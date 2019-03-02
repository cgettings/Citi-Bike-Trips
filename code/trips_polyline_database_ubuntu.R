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
library(jsonlite)
library(lubridate)
library(DBI)
library(dbplyr)
library(magrittr)
library(RSQLite)
library(gepaf)
library(dbplyr)
library(rstudioapi)
library(iterators)
library(glue)
library(rvest)
library(tictoc)
library(doSNOW)

#-----------------------------------------------------------#
# Connecting to databases
#-----------------------------------------------------------#

citibike_trip_db          <- dbConnect(SQLite(), "./data/citibike_trip_db.sqlite3")
citibike_trip_polyline_db <- dbConnect(SQLite(), "./data/citibike_trip_polyline_db.sqlite3")

#===========================================================#
# Initializing OSRM engine ----
#===========================================================#

osm_dir <- "D:/Files/BASP/RANALY~1/Maps/OSM/"
port    <- 5000

terminalExecute(
    glue(
        "{osm_dir}osrm-routed.exe",
        " --port {port}", 
        " {osm_dir}data/osrm/north-america/us/new-york-latest.osrm"), 
    show = FALSE)

#-----------------------------------------------------------#
# Function for sending requests to routing engine
#-----------------------------------------------------------#

get_local_route <- 
    function(df) {
        read_json(
            glue_data(
                df,
                "http://localhost:5000/route/v1/cycling/",
                "{start_station_longitude},{start_station_latitude};",
                "{end_station_longitude},{end_station_latitude}?",
                "annotations=true&continue_straight=true"
            ),
            simplifyVector = FALSE,
            simplifyDataFrame = TRUE,
            simplifyMatrix = FALSE,
            flatten = TRUE
        ) %>%
            extract2("routes") %>%
            select(distance, duration, weight_name, polyline = geometry)
    }

get_local_route_possibly <- possibly(.f = get_local_route, otherwise = tibble())

#===========================================================#
# Computing routes ----
#===========================================================#

#-----------------------------------------------------------#
# Initiating cluster
#-----------------------------------------------------------#

cl <- makeSOCKcluster(3)
registerDoSNOW(cl)

#-----------------------------------------------------------#
# Sending query
#-----------------------------------------------------------#

i_inf <- icount()

res <- 
    dbSendQuery(
        citibike_trip_db, 
        "SELECT * FROM citibike_trips WHERE month IN (2,3,4,5,6,7,8,9) AND year = 2018")

#-----------------------------------------------------------#
# Looping through the results
#-----------------------------------------------------------#

while (!dbHasCompleted(res)) {
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # pretty printing
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    nxt <- nextElem(i_inf)
    cat("Chunk", nxt, "\n")
    
    tic()
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # fetching chunks
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    chunk <- dbFetch(res, n = 1e6)
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # actually sending the coordinates to the engine
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    # polyline_df <-
    #     chunk %>%
    #     plyr::adply(
    #         .data = .,
    #         .margins = 1,
    #         .fun = get_local_route_possibly
    #         # .fun = get_local_route
    #     ) %>%
    #     as_tibble()
    
    polyline_df <-
        chunk %>%
        plyr::adply(
            .data = .,
            .margins = 1,
            .parallel = TRUE,
            .paropts = list(
                .packages = c("dplyr",
                              "glue",
                              "jsonlite",
                              "magrittr",
                              "purrr"),
                .export = "port",
                .errorhandling = "pass",
                .verbose = FALSE
            ), 
            .fun = get_local_route_possibly
        ) %>%
        as_tibble()
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # writing the results to the database
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    dbWriteTable(
        citibike_trip_polyline_db,
        "citibike_trips",
        value = polyline_df,
        append = TRUE,
        temporary = FALSE
    )
    
    toc()
    
}

stopCluster(cl)

dbClearResult(res)

write_lines(terminalBuffer(terminalList()), "./output/osrm buffer.txt")

system("Taskkill /IM osrm-routed.exe /F", show.output.on.console = TRUE)

terminalKill(terminalList())


#===========================================================#
# Creating indexes ----
#===========================================================#

# db_create_index(citibike_trip_polyline_db, "citibike_trips", "year")
# db_create_index(citibike_trip_polyline_db, "citibike_trips", "month")
# db_create_index(citibike_trip_polyline_db, "citibike_trips", "day")
# db_create_index(citibike_trip_polyline_db, "citibike_trips", "start_station_id")
# db_create_index(citibike_trip_polyline_db, "citibike_trips", "end_station_id")
# db_create_index(citibike_trip_polyline_db, "citibike_trips", "bike_id")
# db_create_index(citibike_trip_polyline_db, "citibike_trips", "user_type")
# db_create_index(citibike_trip_polyline_db, "citibike_trips", "birth_year")
# db_create_index(citibike_trip_polyline_db, "citibike_trips", "gender")
# 
# db_list_tables(citibike_trip_polyline_db)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Checking indexes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# dbGetQuery(citibike_trip_polyline_db, "SELECT * FROM sqlite_master WHERE type = 'index'")

#===========================================================#
# Disconnecting ----
#===========================================================#

dbDisconnect(citibike_trip_db)
dbDisconnect(citibike_trip_polyline_db)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #             ---- THIS IS THE END ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
