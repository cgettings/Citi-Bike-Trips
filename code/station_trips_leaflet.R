###########################################################################################-
###########################################################################################-
##
## Leaflet map for Citi bike trips in April 2019 ---
##
###########################################################################################-
###########################################################################################-

#=========================================================================================#
# Setting up ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Loading libraries ----
#-----------------------------------------------------------------------------------------#

library(jsonlite)
library(tidyverse)
library(lubridate)
library(DBI)
library(RSQLite)
library(dbplyr)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)
library(here)
library(glue)
library(magick)
library(viridis)
library(fs)
library(scales)

#-----------------------------------------------------------------------------------------#
# Connecting to database
#-----------------------------------------------------------------------------------------#

citibike_trip_db <- dbConnect(SQLite(), "data/citibike_trip_db.sqlite3")

#-----------------------------------------------------------------------------------------#
# Loading data
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Trip data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Specifying year and month filters

set.seed(726)

trips <-
    citibike_trip_db %>%
    tbl("citibike_trips") %>%
    filter(year == 2019, month == 4) %>%
    select(
        bike_id,
        trip_duration,
        start_time,
        end_time = stop_time,
        start_station_id,
        start_station_name,
        start_station_longitude,
        start_station_latitude,
        end_station_id,
        end_station_name,
        end_station_longitude,
        end_station_latitude
    ) %>%
    collect() %>%
    # filter(start_station_id %in% sample(unique(start_station_id), 100)) %>% 
    mutate(
        start_time = as_datetime(start_time, tz = "US/Eastern"),
        end_time   = as_datetime(end_time,  tz = "US/Eastern")) %>%
    mutate(
        start_time_hourly = floor_date(start_time, "hours"),
        end_time_hourly   = floor_date(end_time,  "hours"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Station info
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

station_info <- 
    fromJSON("https://gbfs.citibikenyc.com/gbfs/en/station_information.json")$data$stations %>% 
    as_tibble() %>% 
    filter(short_name %>% str_detect("JC", negate = TRUE)) %>% 
    select(station_id, longitude = lon, latitude = lat, name, capacity) %>% 
    type_convert() %>% 
    mutate(capacity = na_if(capacity, 0)) %>% 
    arrange(capacity) %>% 
    filter(station_id %in% trips$start_station_id)


#-----------------------------------------------------------------------------------------#
# Summarizing trips
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# By start and end station
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Station list

start_end_stations <- 
    trips %>% 
    distinct(
        start_station_id,
        start_station_name,
        start_station_longitude,
        start_station_latitude,
        end_station_id,
        end_station_name,
        end_station_longitude,
        end_station_latitude
    )

# Counting

trips_count_start_end <- 
    trips %>% 
    count(
        start_station_id,
        end_station_id,
        name = "trips"
    ) %>% 
    left_join(
        .,
        start_end_stations,
        by = c("start_station_id", "end_station_id")
    ) %>% 
    arrange(trips)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# By start station
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Station list

start_stations <- 
    trips %>% 
    distinct(
        start_station_id,
        start_station_name,
        start_station_longitude,
        start_station_latitude
    )

# Counting

trips_count_start <-
    trips %>%
    count(
        start_station_id,
        name = "trips"
    ) %>% 
    left_join(
        .,
        start_stations,
        by = "start_station_id"
    ) %>% 
    arrange(trips)


#=========================================================================================#
# Building the map ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Adding extras
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# plugins & dependencies
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

registerPlugin <- 
    function(map, plugin) {
        map$dependencies <- c(map$dependencies, list(plugin))
        map
    }

# my own FA library

fa_dir <- path(path_home(), "node_modules/@fortawesome/fontawesome-free")

fa_plugin <-
    htmlDependency(
        name = "fontawesome", 
        version = fromJSON(path(fa_dir, "package.json"))$version,
        src = c(file = fa_dir),
        stylesheet = "css/all.css",
        all_files = TRUE
    )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# modifications of {leaflet} functions
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# `addResetMapButton` from {leaflet.extras}, but allowing specification of position

source(here("code/functions/addResetMapButtonPosition.R"))

# `addEasyButton` from {leaflet}, but removing fontawesome dependency (so I can use the current 
#   version from node repo)

source(here("code/functions/addEasyButtonNoFaDeps.R"))


#-----------------------------------------------------------------------------------------#
# Constructing map
#-----------------------------------------------------------------------------------------#

show_station_group_on_click_js <- read_file(here("code/js/show_station_group_on_click.js"))


start_trips_pal <-
    colorNumeric(
        viridis(128, option = "E"),
        trips_count_start %>% pull(trips)
    )


stations_map <- 
    
    leaflet(
        options = list(
            "duration" = 0.375,
            "zoomSnap" = 0.5,
            "preferCanvas" = FALSE, 
            "updateWhenZooming" = TRUE,
            "updateWhenIdle" = TRUE
        )
    ) %>%
    
    fitBounds(
        lng1 = min(c(trips$end_station_longitude, trips$start_station_longitude)),
        lat1 = min(c(trips$end_station_latitude,  trips$start_station_latitude)),
        lng2 = max(c(trips$end_station_longitude, trips$start_station_longitude)),
        lat2 = max(c(trips$end_station_latitude,  trips$start_station_latitude)),
        options = list("padding" = c(5, 5))
    ) %>%
    
    # setView(lng = -73.925380, lat = 40.736118, zoom = 12) %>%
    
    enableTileCaching() %>%
    
    addProviderTiles(
        provider = "CartoDB.DarkMatter",
        options = tileOptions(useCache = TRUE, crossOrigin = TRUE)
    ) %>% 
    
    # Default base group is "all stations", with station capacity
    
    addCircleMarkers(
        data = trips_count_start,
        group = "all_stations",
        layerId = ~ start_station_name,
        label =
            ~ paste(
                start_station_name,
                "|",
                trips, "total trips"
            ),
        lng = ~ start_station_longitude,
        lat = ~ start_station_latitude,
        radius = 4,
        stroke = FALSE,
        weight = 0,
        fillOpacity = 1,
        fillColor = ~ start_trips_pal(trips),
        labelOptions = labelOptions(textsize = "1em", opacity = .9, offset = c(0, -10), direction = "top")
    )


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Adding points for trip destination stations, for each start station
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Looping over start stations

for (i in 1:nrow(trips_count_start)) {
# for (i in 1:100) {
    
    # Defining palette for trip counts, filtered by start station
    
    end_trips_pal <- 
        colorNumeric(
            viridis(128, option = "D"), 
            trips_count_start_end %>% 
                filter(start_station_id == !!trips_count_start$start_station_id[i]) %>% 
                pull(trips)
        )
    
    
    stations_map <- 
        
        stations_map %>% 
        
        # Color-scaled destination stations, filtered by start station
        
        addCircleMarkers(
            data = 
                trips_count_start_end %>% 
                filter(start_station_id == !!trips_count_start$start_station_id[i]),
            
            group = trips_count_start$start_station_name[i],
            lng = ~ end_station_longitude,
            lat = ~ end_station_latitude,
            radius = 4,
            stroke = FALSE,
            weight = 0,
            fillOpacity = 1,
            fillColor = ~ end_trips_pal(trips),
            label =
                ~ paste(
                    end_station_name,
                    "|",
                    trips, "trips"
                ),
            labelOptions = labelOptions(textsize = "15px", opacity = .9, offset = c(0, -10), direction = "top")
        ) %>% 
        
        # Start station markers
        
        addCircleMarkers(
            data = 
                trips_count_start %>% 
                filter(start_station_id == !!trips_count_start$start_station_id[i]),
            
            group = trips_count_start$start_station_name[i],
            lng = ~ start_station_longitude,
            lat = ~ start_station_latitude,
            radius = 8,
            stroke = TRUE,
            color = "white",
            weight = 2,
            opacity = 1,
            fill = FALSE
            
        ) %>% 
        
        # hiding each group by default
        
        hideGroup(trips_count_start$start_station_name[i])
    
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Adding javascript for displaying groups on click
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

stations_map <- 
    stations_map %>% 
    
    addResetMapButtonPosition(position = "bottomleft") %>%
    registerPlugin(fa_plugin) %>%
    
    onRender(
        str_c(
            "function(el, x, data) {\n",
            show_station_group_on_click_js,
            "}"
        ), 
        data = trips_count_start
    )

stations_map

#-----------------------------------------------------------------------------------------#
# Saving map ----
#-----------------------------------------------------------------------------------------#

saveWidget(
    widget = stations_map,
    file = here("docs/stations_map.html"),
    selfcontained = TRUE
)

#=========================================================================================#
# Building the map ----
#=========================================================================================#
