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

library(tidyverse)
library(lubridate)
library(DBI)
library(RSQLite)
library(dbplyr)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(here)
library(leaflet.extras)

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
    jsonlite::fromJSON("https://gbfs.citibikenyc.com/gbfs/en/station_information.json")$data$stations %>% 
    as_tibble() %>% 
    filter(short_name %>% str_detect("JC", negate = TRUE)) %>% 
    select(station_id, longitude = lon, latitude = lat, name, capacity) %>% 
    type_convert() %>% 
    mutate(capacity = na_if(capacity, 0)) %>% 
    arrange(-latitude)

#=========================================================================================#
# Summarizing trips by station ----
#=========================================================================================#

trips_count <- 
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


#########################################################################################

viridis_pal <- colorNumeric("inferno", station_info$capacity)

stations_plot <- 
    leaflet() %>%
    enableTileCaching() %>%
    addTiles(
        urlTemplate = "https://tiles.stadiamaps.com/tiles/alidade_smooth_dark/{z}/{x}/{y}{r}.png",
        options = tileOptions(useCache = TRUE, crossOrigin = TRUE)
    ) %>% 
    addCircleMarkers(
        data = station_info,
        group = "stations",
        lng = ~ longitude,
        lat = ~ latitude,
        label = ~ name,
        radius = 8,
        stroke = TRUE,
        weight = 1,
        opacity = 1,
        fillOpacity = .9,
        color = ~ viridis_pal(capacity),
        popup = 
            ~ paste(
                "<div style='font-size:15px'>", 
                "<b>", name, "</b>", 
                "<br/>", 
                "<i>", "Capacity:", capacity, "</i>",
                "</div>"
            ),
        options = markerOptions(),
        labelOptions = labelOptions(textsize = "15px", opacity = .75)
    ) %>% 
    addLegend(
        data = station_info,
        pal = viridis_pal,
        position = "bottomright",
        values = ~ capacity,
        group = "stations",
        opacity = 1,
        title = "Capacity"
    ) %>% 
    addLayersControl(
        overlayGroups = c("stations"), 
        options = layersControlOptions(collapsed = FALSE)
    )
   
stations_plot

#########################################################################################

save_html(stations_plot, file = here("plots/stations_plot.html"))

#########################################################################################
#########################################################################################
#########################################################################################

viridis_pal <- colorNumeric("inferno", station_info$capacity)

stations_plot_2 <- 
    leaflet() %>%
    enableTileCaching() %>%
    addTiles(
        urlTemplate = "https://tiles.stadiamaps.com/tiles/alidade_smooth_dark/{z}/{x}/{y}{r}.png",
        options = tileOptions(useCache = TRUE, crossOrigin = TRUE)
    )

#########################################################################################

sample_stations <- station_info %>% head(50)

for (i in 1:nrow(sample_stations)) {
    
    stations_plot_2 <- 
        stations_plot_2 %>% 
        addCircleMarkers(
            data = sample_stations[i,],
            group = sample_stations$name[i],
            lng = ~ longitude,
            lat = ~ latitude,
            label = ~ name,
            radius = 8,
            stroke = TRUE,
            weight = 1,
            opacity = 1,
            fillOpacity = .9,
            color = ~ viridis_pal(capacity),
            popup = 
                ~ paste(
                    "<div style='font-size:15px'>", 
                    "<b>", name, "</b>", 
                    "<br/>", 
                    "Capacity:", capacity,
                    "</div>"
                ),
            options = markerOptions(),
            labelOptions = labelOptions(textsize = "15px", opacity = .75)
        )
    
}

#########################################################################################

stations_plot_2 <- 
    stations_plot_2 %>% 
    # addLegend(
    #     data = station_info,
    #     pal = viridis_pal,
    #     position = "bottomright",
    #     values = ~ capacity,
    #     group = "stations",
    #     opacity = 1,
    #     title = "Capacity"
    # ) %>% 
    addLayersControl(
        # overlayGroups = sample_stations$name, 
        baseGroups = sample_stations$name, 
        options = layersControlOptions(collapsed = FALSE)
    )

stations_plot_2

#########################################################################################
#########################################################################################
#########################################################################################

# add the layers with different origin stations as base groups, so that you can look at one at a time,
#   *** BUT *** add "all stations" as the first and default base group

stations_plot_3 <- 
    leaflet() %>%
    enableTileCaching() %>%
    addProviderTiles(
        provider = "CartoDB.DarkMatter",
        options = tileOptions(useCache = TRUE, crossOrigin = TRUE)
    ) %>% 
    # addTiles(
    #     urlTemplate = "https://tiles.stadiamaps.com/tiles/alidade_smooth_dark/{z}/{x}/{y}{r}.png",
    #     options = tileOptions(useCache = TRUE, crossOrigin = TRUE)
    # ) %>% 
    addCircleMarkers(
        data = station_info,
        group = "All stations",
        lng = ~ longitude,
        lat = ~ latitude,
        label = ~ name,
        radius = 3,
        stroke = TRUE,
        weight = 1.5,
        opacity = 1,
        fillOpacity = .25,
        color = "white",
        popup = 
            ~ paste(
                "<div style='font-size:15px'>", 
                "<b>", name, "</b>", 
                "<br/>", 
                "Capacity:", capacity,
                "</div>"
            ),
        options = markerOptions(),
        labelOptions = labelOptions(textsize = "15px", opacity = .75)
    )

# stations_plot_3

#########################################################################################-

viridis_pal <- colorNumeric("inferno", sample_trips$trips)

set.seed(726)

sample_trips <- 
    trips_count %>% 
    filter(start_station_name %in% sample(start_station_name, 10))

sample_stations <- unique(sample_trips$start_station_name)

for (i in 1:length(sample_stations)) {
    
    stations_plot_3 <- 
        stations_plot_3 %>% 
        addCircleMarkers(
            data = sample_trips %>% filter(start_station_name == !!sample_stations[i]),
            group = sample_stations[i],
            lng = ~ end_station_longitude,
            lat = ~ end_station_latitude,
            label = ~ end_station_name,
            radius = 6,
            stroke = TRUE,
            weight = 1,
            opacity = 1,
            fillOpacity = .9,
            color = ~ viridis_pal(trips),
            popup = 
                ~ paste(
                    "<div style='font-size:15px'>", 
                    "<b>", end_station_name, "</b>", 
                    "<br/>", 
                    "<i>", trips, "trips", "</i>",
                    "</div>"
                ),
            labelOptions = labelOptions(textsize = "15px", opacity = .75)
        ) %>% 
        addMarkers(
            data = sample_trips %>% filter(start_station_name == !!sample_stations[i]),
            group = sample_stations[i],
            lng = ~ start_station_longitude,
            lat = ~ start_station_latitude,
            label = ~ start_station_name,
            popup = 
                ~ paste(
                    "<div style='font-size:15px'>", 
                    "<b>", start_station_name, "</b>", 
                    "<br/>", 
                    "<i>", sum(trips), "total trips", "</i>",
                    "</div>"
                ),
            labelOptions = labelOptions(textsize = "15px", opacity = .75)
            # icon = list(iconUrl = "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fb/Map_pin_icon_green.svg/30px-Map_pin_icon_green.svg.png")
        )
    
}

#########################################################################################-

stations_plot_3 <- 
    stations_plot_3 %>% 
    addLegend(
        data = sample_trips,
        pal = viridis_pal,
        position = "bottomright",
        values = ~ trips,
        opacity = 1,
        title = "# of trips"
    ) %>%
    addLayersControl(
        baseGroups = c("All stations", sample_stations), 
        options = layersControlOptions(collapsed = FALSE)
    )

stations_plot_3

#########################################################################################

saveWidget(
    widget = stations_plot_3,
    file = here("docs/stations_plot.html"),
    selfcontained = TRUE,
    title = "Stations plot"
)

#########################################################################################
#########################################################################################
#########################################################################################


