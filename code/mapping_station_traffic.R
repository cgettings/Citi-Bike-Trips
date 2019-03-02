#############################################################-
#############################################################-
##
## Mapping station-level bike traffic ----
##
#############################################################-
#############################################################-

#===========================================================#
# Setting up ----
#===========================================================#

#-----------------------------------------------------------#
# Loading libraries ----
#-----------------------------------------------------------#

library(tidyverse)
library(lubridate)
library(broom)
library(ggmap)
library(sf)
library(maptools)
library(viridis)
library(ggthemes)
library(ggforce)

#-----------------------------------------------------------#
# Loading data from RDS ----
#-----------------------------------------------------------#

rush_hour_trips_summary_station_coordinates <- 
    read_rds("data/march_april_may/rush_hour_trips_summary_station_coordinates.rds")

#-----------------------------------------------------------#
# Mapping data ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# City boundaries ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

city_sptl_df <- 
    read_rds("data/city_sptl_df.RDS") %>% 
    as_tibble()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Park lines ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

park_sptl_df <- 
    read_rds("data/park_sptl_df.RDS") %>% 
    as_tibble()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Streets ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

streets_sptl_df <-
    read_rds("data/streets_sptl_df_2.RDS") %>%
    as_tibble()

#-----------------------------------------------------------#
# Connecting to database ----
#-----------------------------------------------------------#

citibike_trip_db <- dbConnect(SQLite(), "./data/citibike_trip_db.sqlite3")

#-----------------------------------------------------------#
# Loading distinct station coordinates ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# reading from rds ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

station_coordinates_distinct <- 
    read_rds("data/march_april_may/station_coordinates_distinct.rds")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# extracting from database ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# station_coordinates <- 
#     citibike_trip_db %>%
#     tbl("citibike_trips") %>%
#     filter(year == 2018 & month %in% 3:5) %>%
#     select(
#         start_station_id,
#         start_station_latitude,
#         start_station_longitude,
#         end_station_id,
#         end_station_latitude,
#         end_station_longitude) %>%
#     collect()


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# getting distinct ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# station_coordinates_distinct <- 
#     station_coordinates %>% 
#     add_column(trip_id = 1:nrow(.)) %>% 
#     mutate_all("as.character") %>% 
#     gather(var, value, start_station_id:end_station_longitude) %>%
#     mutate(var_long = str_remove(var, "start_|end_")) %>% 
#     mutate(start_end = str_extract(var, "start|end")) %>% 
#     select(-var) %>% 
#     spread(var_long, value) %>% 
#     mutate(start_end = fct_relevel(start_end, "start", "end")) %>% 
#     type_convert() %>% 
#     distinct(station_id, .keep_all = TRUE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Writing to RDS ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# write_rds(
#     station_coordinates_distinct,
#     "data/march_april_may/station_coordinates_distinct.rds"
# )

#-----------------------------------------------------------#
# Loading trips data from RDS ----
#-----------------------------------------------------------#

trips <- 
    read_rds("./data/march_april_may/trips.rds") %>% 
    
    add_column(trip_id = 1:nrow(.), .before = 1) %>% 
    
    rename(end_time = stop_time) %>% 
    rename(end_time_hourly = stop_time_hourly) %>% 
    
    mutate(
        start_date   = as_date(start_time),
        start_year   = year(start_time)   %>% as.integer(),
        start_month  = month(start_time)  %>% as.integer(),
        start_day    = day(start_time)    %>% as.integer(),
        start_hour   = hour(start_time)   %>% as.integer(),
        start_minute = minute(start_time) %>% as.integer(),
        start_wday   = wday(start_time)   %>% as.integer(),
            
        end_date     = as_date(end_time),
        end_year     = year(end_time)   %>% as.integer(),
        end_month    = month(end_time)  %>% as.integer(),
        end_day      = day(end_time)    %>% as.integer(),
        end_hour     = hour(end_time)   %>% as.integer(),
        end_minute   = minute(end_time) %>% as.integer(),
        end_wday     = wday(start_time) %>% as.integer()
    )

#===========================================================#
# Transforming into long format ----
#===========================================================#

trips_long <- 
    trips %>% 
    mutate_all("as.character") %>% 
    gather(var, value, start_time:end_wday) %>%
    mutate(var_long = str_remove(var, "start_|end_")) %>% 
    mutate(start_end = str_extract(var, "start|end")) %>% 
    select(-var) %>% 
    spread(var_long, value) %>% 
    mutate(start_end = fct_relevel(start_end, "start", "end")) %>% 
    type_convert() %>% 
    arrange(start_end, trip_id)

#-----------------------------------------------------------#
# Writing to RDS ----
#-----------------------------------------------------------#

write_rds(
    trips_long, 
    "data/march_april_may/trips_long.rds",
    compress = "gz"
)

#===========================================================#
# Extracting trips during rush hour ----
#===========================================================#

rush_hour_trips <- 
    trips_long %>% 
    filter(wday %in% 1:5 & hour %in% c(6:9, 16:19)) %>% 
    mutate(
        rush_hour = case_when(
            hour %in% 6:9 ~ "AM",
            hour %in% 16:19 ~ "PM",
            TRUE ~ NA_character_
            )
        )

#-----------------------------------------------------------#
# Writing to RDS ----
#-----------------------------------------------------------#

write_rds(
    rush_hour_trips, 
    "data/march_april_may/rush_hour_trips.rds", 
    compress = "gz"
)

#===========================================================#
# Summarizing ----
#===========================================================#

#-----------------------------------------------------------#
# Summarizing counts
#-----------------------------------------------------------#

rush_hour_trips_count <- 
    rush_hour_trips %>% 
    count(rush_hour, start_end, station_id) %>% 
    rename(trips = n)

#-----------------------------------------------------------#
# Summarizing duration
#-----------------------------------------------------------#

rush_hour_trips_duration <- 
    rush_hour_trips %>% 
    group_by(rush_hour, start_end, station_id) %>% 
    summarize(trip_duration_mean = mean(trip_duration)) %>% 
    ungroup()

#-----------------------------------------------------------#
# Combining summaries
#-----------------------------------------------------------#

rush_hour_trips_summary <- 
    left_join(
        rush_hour_trips_count, 
        rush_hour_trips_duration
    )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Writing to RDS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

write_rds(
    rush_hour_trips_summary, 
    "data/march_april_may/rush_hour_trips_summary.rds"
)

#-----------------------------------------------------------#
# Combining with station coordinates
#-----------------------------------------------------------#

rush_hour_trips_summary_station_coordinates <- 
    left_join(
        rush_hour_trips_summary, 
        station_coordinates_distinct %>% select(station_id, station_latitude, station_longitude)
    )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Writing to RDS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

write_rds(
    rush_hour_trips_summary_station_coordinates, 
    "data/march_april_may/rush_hour_trips_summary_station_coordinates.rds"
)


#===========================================================#
# Drawing maps ----
#===========================================================#

#-----------------------------------------------------------#
# Map 1 ----
#-----------------------------------------------------------#

bike_map_1 <-
    ggplot() +
    
    geom_polygon(
        data = city_sptl_df,
        aes(x = long, y = lat, group = group),
        color = "gray50",
        fill = "gray50",
        size = .1) + 
    
    # geom_polygon(
    #     data = park_sptl_df,
    #     aes(x = long, y = lat, group = group),
    #     color = "gray85",
    #     fill = "gray30",
    #     size = 0.1) +
    # 
    # geom_path(
    #     data = streets_sptl_df,
    #     aes(x = long, y = lat, group = group),
    #     color = "gray80",
    #     size = 0.1) +

    geom_point(
        data = rush_hour_trips_summary_station_coordinates %>% 
            mutate(start_end = fct_relevel(start_end, "end", "start")),
        aes(station_longitude,
            station_latitude,
            size = trips,
            fill = trips
        ),
        stroke = .1,
        color = "white",
        shape = 21) +
    
    scale_fill_viridis(option = "viridis") +
    
    facet_grid(start_end ~ rush_hour, switch = "y") +

    coord_map(
        
        projection = "orthographic",
        orientation = c(40.738639, -73.962710, 264),
        
        xlim = c(-74.015, -73.915),
        ylim = c(40.6525, 40.815)
        
    ) +
    
    theme_map() +
    theme(
        legend.position = "none",
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(0, 0, 0, 0),
        panel.background = element_rect(fill = "black"),
        panel.border = element_rect(fill = NA, color = "white"),
        strip.background = element_rect(fill = "gray20", color = "white"),
        strip.text = element_text(size = 12, color = "white")
    )

bike_map_1

#####

ggsave(filename = "plots/bike_map_1.png", plot = bike_map_1, width = 9, height = 15, dpi = 300)

#-----------------------------------------------------------#
# Map 2 ----
#-----------------------------------------------------------#

bike_map_2 <-
    ggplot() +
    
    geom_polygon(
        data = city_sptl_df,
        aes(x = long, y = lat, group = group),
        color = "gray68",
        fill = "gray68",
        size = .1
    ) +
    
    # geom_polygon(
    #     data = park_sptl_df,
    #     aes(x = long, y = lat, group = group),
    #     color = "gray80",
    #     fill = "gray60",
    #     size = 0.1
    # ) +
    
    # geom_path(
    #     data = streets_sptl_df,
    #     aes(x = long, y = lat, group = group),
    #     color = "gray90",
    #     size = 0.1
    # ) +

    geom_point(
        data = rush_hour_trips_summary_station_coordinates,
        aes(station_longitude,
            station_latitude,
            size = trips,
            fill = trips
        ),
        color = "white",
        shape = 21) +
    
    scale_fill_viridis(option = "plasma", begin = .1) +
    
    facet_grid(rush_hour ~ start_end) +
    
    coord_quickmap(
        xlim = c(-74.03, -73.91),
        ylim = c(40.6515, 40.806),
        expand = FALSE
    ) +
    theme_map() +
    theme(
        legend.position = "none",
        plot.background = element_rect(fill = "black", color = "black"),
        plot.margin = margin(0, 0, 0, 0)
    )

bike_map_2

#########################

