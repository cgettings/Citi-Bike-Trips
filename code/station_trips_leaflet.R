###########################################################################################-
###########################################################################################-
##
## Leaflet map for Citi bike trips in April 2019 ---
##
###########################################################################################-
###########################################################################################-

# This script constructs a map of citibike trips in April 2019. The map displays the top-ten 
#   origin stations for the month (i.e., most bikes taken out), and each of the destination
#   stations for those trips.
#   
# The default layer displays each station in the system, along with its capacity.

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

#-----------------------------------------------------------------------------------------#
# Connecting to database
#-----------------------------------------------------------------------------------------#

citibike_trip_db <- dbConnect(SQLite(), "data/citibike_trip_db.sqlite3")

#=========================================================================================#
# Loading data ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Trip data
#-----------------------------------------------------------------------------------------#

# Specifying year and month filters

trips <-
    citibike_trip_db %>%
    tbl("citibike_trips") %>%
    filter(year == 2019 & month == 4) %>%
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
    mutate(
        start_time = as_datetime(start_time, tz = "US/Eastern"),
        end_time   = as_datetime(end_time,  tz = "US/Eastern")) %>%
    mutate(
        start_time_hourly = floor_date(start_time, "hours"),
        end_time_hourly   = floor_date(end_time,  "hours"))

#-----------------------------------------------------------------------------------------#
# Station info
#-----------------------------------------------------------------------------------------#

station_info <- 
    fromJSON("https://gbfs.citibikenyc.com/gbfs/en/station_information.json")$data$stations %>% 
    as_tibble() %>% 
    filter(short_name %>% str_detect("JC", negate = TRUE)) %>% 
    select(station_id, longitude = lon, latitude = lat, name, capacity) %>% 
    type_convert() %>% 
    mutate(capacity = na_if(capacity, 0)) %>% 
    arrange(-latitude)

#-----------------------------------------------------------------------------------------#
# Summarizing trips
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# By start and end station
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trips_count_start_end <- 
    trips %>% 
    count(
        start_station_id,
        start_station_name,
        start_station_longitude,
        start_station_latitude,
        end_station_id,
        end_station_name,
        end_station_longitude,
        end_station_latitude, 
        name = "trips"
    ) %>% 
    arrange(start_station_longitude)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# By start station
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trips_count_start <-
    trips %>%
    count(
        start_station_id,
        start_station_name,
        start_station_longitude,
        start_station_latitude,
        name = "trips"
    ) %>% 
    arrange(-trips)


#-----------------------------------------------------------------------------------------#
# Pulling out top-10 origin stations
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Extracting top-10
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

top_10_stations <- 
    trips_count_start %>% 
    slice(1:10)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Subsetting top-10
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trips_count_start_end_top_10 <- 
    trips_count_start_end %>% 
    filter(start_station_name %in% !!top_10_stations$start_station_name)


#=========================================================================================#
# Building the map ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Custom map marker
#-----------------------------------------------------------------------------------------#

# Reading in SVG image

map_marker <- 
    image_read_svg(
        "https://upload.wikimedia.org/wikipedia/commons/9/92/Ic_location_on_48px.svg",
        width = 300
    )

# citibike light blue color

map_marker_red <-
    image_fill(
        image = map_marker,
        color = "red",
        point = "+150+50", 
        fuzz = 0
    )

# Saving modified marker

image_write(map_marker_red, "map_marker_red.svg", format = "svg")

# Turning the marker image into a leaflet marker icon

marker_icon <- 
    makeIcon(
        iconUrl = "map_marker_red.svg",
        iconWidth = 40,
        iconAnchorX = 20,
        iconAnchorY = 40
    )


#-----------------------------------------------------------------------------------------#
# Constructing map ----
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Base map
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

stations_map <- 
    
    leaflet() %>%
    
    enableTileCaching() %>%
    
    addProviderTiles(
        provider = "CartoDB.DarkMatter",
        options = tileOptions(useCache = TRUE, crossOrigin = TRUE)
    ) %>% 
    
    # Default base group is "all stations", with station capacity
    
    addCircleMarkers(
        data = station_info,
        group = "All stations",
        lng = ~ longitude,
        lat = ~ latitude,
        label = ~ name,
        radius = 4,
        stroke = FALSE,
        weight = 0,
        fillOpacity = 1,
        color = "#0A6ECE",
        popup = 
            ~ paste(
                "<div style='font-size:15px'>", 
                "<b>", name, "</b>", 
                "<br/>", 
                "Capacity:", capacity,
                "</div>"
            ),
        labelOptions = labelOptions(textsize = "15px", opacity = .9),
        popupOptions = popupOptions(closeOnClick = TRUE, closeOnEscapeKey = TRUE)
    )


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Adding points for trip destination stations, for each start station
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Looping over start stations

for (i in 1:nrow(top_10_stations)) {
    
    # Defining palette for trip counts
    
    my_viridis_pal <- 
        colorNumeric(
            cividis(512), 
            trips_count_start_end_top_10 %>% 
                filter(start_station_name == !!top_10_stations$start_station_name[i]) %>% 
                pull(trips)
        )
    
    
    stations_map <- 
        
        stations_map %>% 
        
        # Color-scaled destination stations
        
        addCircleMarkers(
            data = 
                trips_count_start_end_top_10 %>% 
                filter(start_station_name == !!top_10_stations$start_station_name[i]),
            
            group = top_10_stations$start_station_name[i],
            lng = ~ end_station_longitude,
            lat = ~ end_station_latitude,
            label = ~ end_station_name,
            radius = 4,
            stroke = FALSE,
            weight = 0,
            fillOpacity = 1,
            fillColor = ~ my_viridis_pal(trips),
            popup = 
                ~ paste(
                    "<div style='font-size:15px'>", 
                    "<b>", end_station_name, "</b>", 
                    "<br/>", 
                    "<i>", trips, "trips", "</i>",
                    "</div>"
                ),
            labelOptions = labelOptions(textsize = "15px", opacity = .9),
            popupOptions = popupOptions(closeOnClick = TRUE, closeOnEscapeKey = TRUE)
        ) %>% 
        
        # Start station markers
        
        addMarkers(
            data = top_10_stations %>% filter(start_station_name == !!top_10_stations$start_station_name[i]),
            group = top_10_stations$start_station_name[i],
            lng = ~ start_station_longitude,
            lat = ~ start_station_latitude,
            label = ~ start_station_name,
            icon = marker_icon,
            popup = 
                ~ paste(
                    "<div style='font-size:15px'>", 
                    "<b>", start_station_name, "</b>", 
                    "<br/>", 
                    "<i>", sum(trips), "total trips", "</i>",
                    "</div>"
                ),
            options = markerOptions(opacity = .85),
            labelOptions = labelOptions(textsize = "15px", opacity = .9),
            popupOptions = popupOptions(closeOnClick = TRUE, closeOnEscapeKey = TRUE)
        )
    
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Adding legend and controls
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

stations_map <- 
    
    stations_map %>%
    
    addLayersControl(
        baseGroups = 
            c(
                "All stations", 
                top_10_stations$start_station_name
            ), 
        options = layersControlOptions(collapsed = FALSE)
    )

stations_map

#-----------------------------------------------------------------------------------------#
# Saving map ----
#-----------------------------------------------------------------------------------------#

saveWidget(
    widget = stations_map,
    file = here("docs/stations_map.html"),
    selfcontained = TRUE,
    title = "Top-10 start stations map"
)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
