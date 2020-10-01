###########################################################################################-
###########################################################################################-
##
## visualizing model predictions with Leaflet ---
##
###########################################################################################-
###########################################################################################-

#=========================================================================================#
# Setting up ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Loading libraries ----
#-----------------------------------------------------------------------------------------#

library(raster)
library(tidyverse)
library(glue)
library(lubridate)
library(fs)
library(here)
library(glue)
library(hms)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(htmltools)
library(htmlwidgets)
library(viridis)
library(ggdark)
library(ggspatial)
library(rosm)
library(tmaptools)
library(sf)
library(sp)

#-----------------------------------------------------------------------------------------#
# Specifying directories
#-----------------------------------------------------------------------------------------#

data_dir   <- "data/2018"
plots_dir  <- "plots/time_series/2018"
output_dir <- "output/2018"

fit_name <- "glmmTMB_fit_m345_20s_genpois_141213"

#-----------------------------------------------------------------------------------------#
# Loading data
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# stations
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

stations <- read_rds(here("data/stations.rds"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# model predictions
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

predictions <- 
    read_rds(
        path(
            output_dir, 
            str_c(fit_name, "_predictions_on_grid")
        ) %>% 
            path_ext_set("rds")
    ) %>% 
    left_join(
        .,
        stations,
        by = c("start_station_name" = "station_name")
    )


#-----------------------------------------------------------------------------------------#
# Map features
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Creating bbox
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

lon_diff <- max(predictions$station_longitude) - min(predictions$station_longitude)
lat_diff <- max(predictions$station_latitude)  - min(predictions$station_latitude)

stations_bbox <- 
    c(
        min(predictions$station_longitude) - (lon_diff * 0.1),
        min(predictions$station_latitude)  - (lat_diff * 0.1),
        max(predictions$station_longitude) + (lon_diff * 0.1),
        max(predictions$station_latitude)  + (lat_diff * 0.1)
    ) %>% 
    bb()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# CRS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

my_crs <- "epsg:4326"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Map layers
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

nyc_streets <- 
    st_read("data/gis/Streets/Centerline/Centerline.shp") %>%
    filter(BOROCODE %in% c(1, 2, 3, 4) & RW_TYPE %in% c(1, 2, 3)) %>%
    st_transform(crs = my_crs) %>% 
    st_crop(stations_bbox)

nyc_boro_bounds <- 
    st_read("data/gis/2020_03_13_nybb_20a/nybb_20a/nybb.shp") %>% 
    st_transform(crs = my_crs)

nyc_parks <- 
    st_read("data/gis/Open Space (Parks)/geo_export_ac63f253-9910-4819-8d6c-344836349c03.shp") %>%
    st_transform(crs = my_crs)

#=========================================================================================#
# Mapping ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Static
#-----------------------------------------------------------------------------------------#

station_mean_predictions <- 
    predictions %>% 
    drop_na() %>% 
    group_by(start_station_name) %>% 
    summarise(fitted_station_mean = mean(fitted, na.rm = TRUE)) 


predictions_summ <- 
    predictions %>% 
    drop_na() %>% 
    
    group_by(precip_yn, temperature_level, start_station_name) %>% 
    summarise(fitted_station_precip_yn_temperature_level_mean = mean(fitted, na.rm = TRUE)) %>% 
    ungroup() %>% 
    
    left_join(., station_mean_predictions) %>% 
    
    mutate(
        fitted_station_precip_yn_temperature_level_deviation = 
            fitted_station_precip_yn_temperature_level_mean - fitted_station_mean,
        
        fitted_station_precip_yn_temperature_level_deviation_perc = 
            fitted_station_precip_yn_temperature_level_deviation / fitted_station_mean
    ) %>% 
    
    left_join(
        ., 
        stations,
        by = c("start_station_name" = "station_name")
    )


fitted_station_precip_yn_temperature_level_deviation_perc_plot <- 
    
    ggplot() + 
    
    geom_sf(
        data = nyc_boro_bounds, 
        aes(geometry = geometry),
        color = NA,
        fill = "gray25"
    ) +
    geom_sf(
        data = nyc_parks,
        aes(geometry = geometry),
        fill = "gray7",
        color = NA
    ) +
    geom_sf(
        data = nyc_streets,
        aes(geometry = geometry),
        color = "gray12",
        size = .125
    ) +
    geom_point(
        data = predictions_summ,
        aes(
            station_longitude, 
            station_latitude,
            color = fitted_station_precip_yn_temperature_level_deviation_perc, 
            size = fitted_station_mean
        ),
        shape = 16
    ) +
    
    scale_color_viridis_c(name = "% Difference from\nStation Mean", option = "C") +
    scale_size(name = "Mean Trips / Hour") +
    
    facet_grid(
        rows = vars(precip_yn),
        cols = vars("Temperature" = temperature_level),
        labeller = 
            labeller(
                precip_yn = as_labeller(c(`0` = "Not Raining", `1` = "Raining")),
                Temperature = label_both
            )
    ) +
    
    coord_sf(
        xlim = c(stations_bbox[["xmin"]], stations_bbox[["xmax"]]),
        ylim = c(stations_bbox[["ymin"]], stations_bbox[["ymax"]]),
        expand = FALSE
    ) +
    dark_theme_minimal() +
    theme(
        axis.text = element_blank(), 
        axis.title = element_blank(),
        plot.background = element_rect(color = NA),
        panel.border = element_rect(color = "gray", fill = NA)
    )


#-----------------------------------------------------------------------------------------#
# Saving map ----
#-----------------------------------------------------------------------------------------#

ggsave(
    path(
        plots_dir,
        fit_name,
        str_c(fit_name, "_fitted_precip_yn_temperature_level_deviation_perc") %>% 
            path_ext_set("png")
    ),
    fitted_station_precip_yn_temperature_level_deviation_perc_plot, 
    scale = 1, 
    width = 9, 
    height = 8,
    bg = "black"
)


#-----------------------------------------------------------------------------------------#
# Interactive
#-----------------------------------------------------------------------------------------#


# stations_map <- 
#     
#     leaflet() %>%
#     
#     setView(lng = -73.925380, lat = 40.736118, zoom = 12) %>%
#     
#     enableTileCaching() %>%
#     
#     addProviderTiles(
#         provider = "CartoDB.DarkMatter",
#         options = tileOptions(useCache = TRUE, crossOrigin = TRUE)
#     ) %>% 
#     
#     addCircleMarkers(
#         data = 
#             trips_count_start_end_top_10 %>% 
#             filter(start_station_name == !!top_10_stations$start_station_name[i]),
#         
#         group = top_10_stations$start_station_name[i],
#         lng = ~ end_station_longitude,
#         lat = ~ end_station_latitude,
#         label = ~ end_station_name,
#         radius = 4,
#         stroke = FALSE,
#         weight = 0,
#         fillOpacity = 1,
#         fillColor = ~ my_viridis_pal(trips),
#         popup = 
#             ~ paste(
#                 "<div style='font-size:15px'>", 
#                 "<b>", end_station_name, "</b>", 
#                 "<br/>", 
#                 "<i>", trips, "trips", "</i>",
#                 "</div>"
#             ),
#         labelOptions = labelOptions(textsize = "15px", opacity = .9),
#         popupOptions = popupOptions(closeOnClick = TRUE, closeOnEscapeKey = TRUE)
#     )


#-----------------------------------------------------------------------------------------#
# Saving map ----
#-----------------------------------------------------------------------------------------#

# saveWidget(
#     widget = stations_map,
#     file = here("docs/stations_map.html"),
#     selfcontained = TRUE,
#     title = "Top-10 start stations map"
# )

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
