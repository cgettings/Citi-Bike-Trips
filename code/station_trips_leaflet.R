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

trips <-
    citibike_trip_db %>%
    tbl("citibike_trips") %>%
    filter(year == 2019, month == 4) %>%
    select(
        year,
        month,
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
        year,
        month,
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
        year,
        month,
        start_station_id,
        end_station_id,
        name = "trips"
    ) %>% 
    left_join(
        .,
        start_end_stations,
        by = c("year", "month", "start_station_id", "end_station_id")
    ) %>% 
    arrange(year, month, start_station_id, trips)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# By start station
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Station list

start_stations <- 
    trips %>% 
    distinct(
        year,
        month,
        start_station_id,
        start_station_name,
        start_station_longitude,
        start_station_latitude
    )

# Counting

trips_count_start <-
    trips %>%
    count(
        year,
        month,
        start_station_id,
        name = "trips"
    ) %>% 
    left_join(
        .,
        start_stations,
        by = c("year", "month", "start_station_id")
    ) %>% 
    arrange(year, month, start_station_id, trips)


#-----------------------------------------------------------------------------------------#
# Computing trip count colors here instead of in JS on add
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Generic palette function
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

end_trips_pal <- colour_ramp(colors = viridis(128), na.color = "gray30", alpha = FALSE)

trips_colors <- list()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# For each station, compute trip count colors
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

for (i in 1:nrow(trips_count_start)) {
    
    trips_colors[[i]] <-
        trips_count_start_end %>% 
        filter(
            year == !!trips_count_start$year[i],
            month == !!trips_count_start$month[i],
            start_station_id == !!trips_count_start$start_station_id[i]
        ) %>% 
        pull(trips) %>% 
        rescale() %>% 
        end_trips_pal()
    
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Add colors to dataset
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trips_count_start_end_color <- 
    trips_count_start_end %>% 
    mutate(colors = flatten_chr(trips_colors)) %>% 
    group_by(start_station_name) %>% 
    ungroup()


#-----------------------------------------------------------------------------------------#
# Converting data to nested, named list
#-----------------------------------------------------------------------------------------#

# this will make subsetting the data easier in JS, as well as reduce size of data given to JS
# 
# `split()` created a list with elements named after the split variable
# 
# end product is `trips_nested_list[year][month][start_station_name]`

trips_nested_list <- 
    
    trips_count_start_end_color %>% 
    
    # initially splitting by year
    
    split(
        x = select(., -year), 
        f = .$year
    ) %>% 
    
    # then within each year group, splitting by month
    
    map( 
        ~ split(
            x = select(.x, -month), 
            f = .x$month
        ) %>% 
            
            # then within each month group, splitting by station name
            
            map( 
                ~ split(
                    x = select(.x, -start_station_name), 
                    f = .x$start_station_name
                )
            )
    )


#=========================================================================================#
# Building the map ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Adding extras
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Reading in javascript for displaying groups on click
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

add_station_group_on_click_js <- read_file(here("code/js/add_station_group_on_click.js"))

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

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# defining color palette
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

start_trips_pal <-
    colorNumeric(
        viridis(128, option = "D"),
        trips_count_start %>% pull(trips)
    )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Base map and "all stations" markers
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

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
    
    # setting initial counds based on extent of extracted data
    
    fitBounds(
        lng1 = min(c(trips$end_station_longitude, trips$start_station_longitude)),
        lat1 = min(c(trips$end_station_latitude,  trips$start_station_latitude)),
        lng2 = max(c(trips$end_station_longitude, trips$start_station_longitude)),
        lat2 = max(c(trips$end_station_latitude,  trips$start_station_latitude)),
        options = list("padding" = c(5, 5))
    ) %>%
    
    enableTileCaching() %>%
    
    addProviderTiles(
        provider = "CartoDB.DarkMatter",
        options = tileOptions(useCache = TRUE, crossOrigin = TRUE)
    ) %>% 
    
    # Default base group is "all stations", with total trips
    
    addCircleMarkers(
        data = trips_count_start,
        group = "all_stations",
        layerId = ~ start_station_name,
        label =
            ~ paste(
                start_station_name,
                " - ",
                trips, "total trips"
            ),
        lng = ~ start_station_longitude,
        lat = ~ start_station_latitude,
        radius = 4,
        stroke = FALSE,
        weight = 0,
        fillOpacity = 1,
        fillColor = ~ start_trips_pal(trips),
        labelOptions = labelOptions(textsize = "1.1em", opacity = .9, offset = c(0, -10), direction = "top")
    ) %>% 
    
    addResetMapButtonPosition(position = "bottomleft") %>%
    registerPlugin(fa_plugin) %>%
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # Adding javascript for displaying groups on click
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    onRender(
        str_c(
            "function(el, x, data) {\n",
            add_station_group_on_click_js,
            "}"
        ), 
        data = trips_nested_list
    )

# stations_map

#-----------------------------------------------------------------------------------------#
# Saving map ----
#-----------------------------------------------------------------------------------------#

saveWidget(
    widget = stations_map,
    file = here("docs/stations_map.html"),
    selfcontained = TRUE,
    title = "Trips between Citi Bike stations, April 2019"
)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
