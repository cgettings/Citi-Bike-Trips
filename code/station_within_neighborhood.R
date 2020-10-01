
# categorizing citi bike stations into neighborhoods

#################################################################################

library(raster)
library(tidyverse)
library(ggspatial)
library(sf)
library(lwgeom)
library(DBI)
library(RSQLite)
library(dbplyr)
library(lubridate)

#################################################################################

my_crs <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#################################################################################

stations <- read_rds("data/stations.rds")

#################################################################################

unique_stations <- 
    stations %>% 
    rename_all( ~ str_remove(.x, "start_")) %>% 
    mutate(
        station_loc = 
            map2(
                station_longitude, 
                station_latitude,
                ~ st_point(
                    cbind(.x, .y)
                )
            ) %>% st_sfc(crs = my_crs),
        station_row = 1:nrow(.)
    )

#################################################################################

# Downloaded from: https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=Original

nyc_boro_bounds <- 
    raster::shapefile("data/gis/2020_03_13_nybb_20a/nybb_20a/nybb.shp") %>% 
    st_as_sf() %>% 
    st_transform(crs = my_crs) %>% 
    as_tibble()

#################################################################################

neighborhoods <- 
    raster::shapefile("data/gis/neighborhoods/nynta/nynta.shp") %>% 
    st_as_sf() %>% 
    st_transform(crs = my_crs) %>%
    as_tibble() %>% 
    mutate(neighborhood_row = 1:nrow(.))

#################################################################################

nyc_covers <- 
    st_geod_covers(
        nyc_boro_bounds$geometry,
        unique_stations$station_loc
    )

nyc_covers_tbl <- 
    nyc_covers %>% 
    as_tibble()

#################################################################################

stations_in_nyc <- 
    left_join(
        nyc_covers_tbl,
        unique_stations,
        by = c("col.id" = "station_row")
    )

#################################################################################

neighborhood_covers <- 
    st_geod_covers(
        neighborhoods$geometry,
        unique_stations$station_loc
    )

neighborhood_covers_tbl <- 
    neighborhood_covers %>% 
    as_tibble()

#################################################################################

covered_neighborhoods <- 
    left_join(
        neighborhood_covers_tbl,
        neighborhoods,
        by = c("row.id" = "neighborhood_row")
    )

#################################################################################

stations_neighborhoods <- 
    left_join(
        covered_neighborhoods,
        unique_stations,
        by = c("col.id" = "station_row")
    ) %>% 
    select(-row.id, -col.id, -geometry, -station_loc)

#################################################################################

write_rds(stations_neighborhoods, "data/stations_neighborhoods.rds")

#################################################################################
