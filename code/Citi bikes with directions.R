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
library(rstudioapi)
library(iterators)
library(doSNOW)

setwd("D:/Files/BASP/R analyses/Bike Share/NYC - Citi Bike/Trip Data/")

#######################################

# "https://maps.googleapis.com/maps/api/directions/json?origin=Toronto&destination=Montreal&avoid=highways&mode=bicycling&key=YOUR_API_KEY"


json_response <-
    read_json(
        glue_data(
            trips_6[1,],
            "https://maps.googleapis.com/maps/api/directions/",
            "json?",
            "origin={start_station_latitude},{start_station_longitude}&",
            "destination={end_station_latitude},{end_station_longitude}&",
            "mode=bicycling&",
            "key=AIzaSyAOIqOIOJnrGVcjhPkSjCr6EwVSvRJAAvo"
        ),
        simplifyVector = TRUE
    ) %>%
    extract2("routes") %>%
    pull(overview_polyline) %>%
    select(polyline = points)



#######################################

cl <- makeSOCKcluster(3)
registerDoSNOW(cl)

stopCluster(cl)

#######################################

citibike_trip_db <- dbConnect(RSQLite::SQLite(), "data/citibike_trip_db.sqlite3")
citibike_trip_db_chunk <- dbConnect(RSQLite::SQLite(), "data/citibike_trip_db_chunk.sqlite3")

#######################################

osm_dir <- "D:/Files/BASP/RANALY~1/Maps/OSM/"
port <- 5000

terminalExecute(
    glue(
        "{osm_dir}osrm-routed.exe",
        " --port {port}", 
        " {osm_dir}data/osrm/north-america/us-northeast-latest.osrm"), 
    show = FALSE)

# terminalKill(terminalList())

#######################################

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

get_local_route_safely <- safely(.f = get_local_route)
get_local_route_possibly <- possibly(.f = get_local_route, otherwise = tibble())

#######################################

i_inf <- icount()
# indexes <- data.frame()

res <- dbSendQuery(citibike_trip_db, "SELECT * FROM citibike_trips")

####

while (!dbHasCompleted(res)) {
    
    nxt <- nextElem(i_inf)
    cat("Chunk", nxt, "\n")
    
    # start_time <- now()
    
    chunk <- dbFetch(res, n = 10000)
    
    ###-####-####-####-####-####-####-####-###
    
    polyline_df <-
        chunk %>%
        # slice(8001:10000) %>%
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
                # .export = "port",
                .errorhandling = "pass",
                .verbose = FALSE
            ),
            .fun = get_local_route_possibly
        ) %>%
        as_tibble()
    
    
    ###-####-####-####-####-####-####-####-###
    
    dbWriteTable(
        citibike_trip_db_chunk,
        "citibike_trips",
        value = polyline_df,
        append = TRUE,
        temporary = FALSE
    )
    
    # end_time <- now()
    # start_time - end_time
    
}

#######################################

terminalKill(terminalList())

dbDisconnect(citibike_trip_db)
dbDisconnect(citibike_trip_db_chunk)

#######################################


bike_trips_5 <- 
    citibike_trip_db %>% 
    tbl("citibike_trips") %>% 
    filter(year == 2017, month == 5, day == 5) %>% 
    count(bike_id) %>% 
    arrange(-n) %>% 
    head(5) %>% 
    collect()


bike_trips_6 <- 
    citibike_trip_db %>% 
    tbl("citibike_trips") %>% 
    filter(year == 2017, month == 5, day == 6) %>% 
    count(bike_id) %>% 
    arrange(-n) %>% 
    head(5) %>% 
    collect()


bike_trips_5_2 <- 
    citibike_trip_db %>% 
    tbl("citibike_trips") %>% 
    filter(year == 2017, month == 5, day == 5, bike_id %in% bike_trips_5$bike_id) %>% 
    collect()

bike_trips_6_2 <- 
    citibike_trip_db %>% 
    tbl("citibike_trips") %>% 
    filter(year == 2017, month == 5, day == 6, bike_id %in% bike_trips_6$bike_id) %>% 
    collect()



#######################################
#######################################


polyline_5_df <-
    bike_trips_5_2 %>%
    do(., plyr::adply(
        .data = .,
        .margins = 1,
        .fun = function(df) {
            read_json(
                glue_data(
                    df,
                    "https://maps.googleapis.com/maps/api/directions/",
                    "json?",
                    "origin={start_station_latitude},{start_station_longitude}&",
                    "destination={end_station_latitude},{end_station_longitude}&",
                    "mode=bicycling&",
                    "key=AIzaSyAOIqOIOJnrGVcjhPkSjCr6EwVSvRJAAvo"
                ),
                simplifyVector = TRUE
            ) %>%
                extract2("routes") %>%
                pull(overview_polyline) %>%
                select(polyline = points)
        }
    )) %>% 
    as_tibble()


write_csv(polyline_5_df, "./data/polyline_5_df.csv")
write_rds(polyline_5_df, "./data/polyline_5_df.rds")


#######################################
#######################################


polyline_5_df <-
    bike_trips_5_2 %>%
    do(., plyr::adply(
        .data = .,
        .margins = 1,
        .fun = function(df) {
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
    )) %>% 
    as_tibble()


write_csv(polyline_5_df, "./data/polyline_5_osrm_df.csv")
write_rds(polyline_5_df, "./data/polyline_5_osrm_df.rds")


#######################################
#######################################


polyline_6_df <-
    bike_trips_6_2 %>%
    do(., plyr::adply(
        .data = .,
        .margins = 1,
        .fun = function(df) {
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
    )) %>% 
    as_tibble()


write_csv(polyline_6_df, "./data/polyline_6_osrm_df.csv")
write_rds(polyline_6_df, "./data/polyline_6_osrm_df.rds")


#######################################
#######################################

polyline_df <- 
    bind_rows(polyline_5_df, polyline_6_df) %>% 
    add_column(trip_num = 1:nrow(.), .before = 1)


write_csv(polyline_df, "./data/polyline_osrm_df.csv")
write_rds(polyline_df, "./data/polyline_osrm_df.rds")

#######################################
#######################################

## Running loop ##

unique_points <- data_frame()

for (i in 1:nrow(polyline_df)) {
    
    unique_points <-
        
        polyline_df %>%
        select(trip_num, polyline) %>%
        slice(i) %>%
        group_by(trip_num) %>%
        do(decodePolyline(.$polyline) %>% as_tibble()) %>%
        mutate(point_order = 1:nrow(.)) %>%
        nest(.key = "trip_points") %>% 
        left_join(polyline_df %>% slice(i), ., by = "trip_num") %>% 
        bind_rows(unique_points, .)
    
}

write_rds(unique_points, "./data/unique_points_osrm.rds")

#######################################
#######################################



polyline_5_df <-
    bike_trips_5_2 %>%
    do(., plyr::adply(
        .data = .,
        .margins = 1,
        .fun = function(df) {
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
                select(distance, duration, weight_name, geometry)
        }
    )) %>% 
    as_tibble()


write_csv(polyline_5_df, "./data/polyline_5_df.csv")
write_rds(polyline_5_df, "./data/polyline_5_df.rds")



#######################################
#######################################


polyline_5_df <-
    bike_trips_5_2 %>%
    do(., plyr::adply(
        .data = .,
        .margins = 1,
        .fun = function(df) {
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
                select(distance, duration, weight_name, geometry)
        }
    )) %>% 
    as_tibble()


#---------------------------------#
# ---- Creating indexes ----
#---------------------------------#

db_create_index(citibike_trip_db_chunk, "citibike_trips", "year")
db_create_index(citibike_trip_db_chunk, "citibike_trips", "month")
db_create_index(citibike_trip_db_chunk, "citibike_trips", "day")
db_create_index(citibike_trip_db_chunk, "citibike_trips", "start_station_id")
db_create_index(citibike_trip_db_chunk, "citibike_trips", "end_station_id")
db_create_index(citibike_trip_db_chunk, "citibike_trips", "bike_id")
db_create_index(citibike_trip_db_chunk, "citibike_trips", "user_type")
db_create_index(citibike_trip_db_chunk, "citibike_trips", "birth_year")
db_create_index(citibike_trip_db_chunk, "citibike_trips", "gender")

db_list_tables(citibike_trip_db_chunk)

### Checking indexes ### 

dbGetQuery(citibike_trip_db_chunk, "SELECT * FROM sqlite_master WHERE type = 'index'")

#######################

dbDisconnect(citibike_trip_db)

############################################################
############################################################


