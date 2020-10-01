
# computing distance from citi bike stations to subway entrances

library(lubridate)
library(tidyverse)
library(geosphere)
library(DBI)
library(dbplyr)
library(RSQLite)


subway_entrances <- 
    read_csv("data/StationEntrances.csv") %>% 
    rename_all("str_to_lower") %>% 
    select(division, station_name, latitude, longitude, route_1:route_11) %>% 
    mutate_at(vars(route_1:route_11), "as.character") %>% 
    add_column(entrance_number = 1:nrow(.), .before = 1)


citibike_trip_db <- dbConnect(SQLite(), "data/citibike_trip_db.sqlite3")


stations <-
    citibike_trip_db %>%
    tbl("citibike_trips") %>%
    filter(year == 2018 & month %in% 3:5) %>%
    select(
        start_station_id,
        start_station_name,
        start_station_longitude,
        start_station_latitude
    ) %>%
    collect() %>% 
    distinct(start_station_id, start_station_name, .keep_all = TRUE)


# combine

stations_subway_entrances <- 
    expand_grid(stations, subway_entrances) %>% 
    mutate(
        distance = 
            distCosine(
                p1 = select(., start_station_longitude, start_station_latitude), 
                p2 = select(., longitude, latitude)
            )
    ) %>% 
    filter(distance < 100) %>% 
    select(-start_station_longitude, -start_station_latitude) %>% 
    left_join(
        stations, 
        ., 
        by = c("start_station_id", "start_station_name")
    )


stations_route_count <- 
    stations_subway_entrances %>% 
    pivot_longer(
        route_1:route_11, 
        names_to = "route_number", 
        values_to = "route", 
        values_drop_na = TRUE
    ) %>% 
    group_by(start_station_id) %>% 
    summarise(route_count = n_distinct(route, na.rm = TRUE))


stations_entrance_count <- 
    stations_subway_entrances %>% 
    group_by(start_station_id) %>% 
    summarise(entrance_count = n_distinct(entrance_number, na.rm = TRUE))


stations_entrances_route_count <- 
    stations_subway_entrances %>% 
    left_join(
        .,
        stations_route_count,
        by = "start_station_id"
    ) %>% 
    left_join(
        .,
        stations_entrance_count,
        by = "start_station_id"
    ) %>% 
    select(-(route_1:route_11)) %>% 
    mutate(
        entrance_count = replace_na(entrance_count, 0),
        route_count = replace_na(route_count, 0)
    ) %>% 
    arrange(start_station_name, distance) %>% 
    distinct(start_station_id, .keep_all = TRUE) %>% 
    rename(closest_distance = distance) %>% 
    select(-(entrance_number:longitude))


write_rds(stations_entrances_route_count, "data/stations_entrances_route_count.rds")



