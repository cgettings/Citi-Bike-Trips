##################################################-
##################################################-
##
## Plotting bike stops ----
##
##################################################-
##################################################-


#========================================#
# Setting up ----
#========================================#

#---------------------------------#
# Loading libraries ----
#---------------------------------#

library(readr)
library(tibble)
library(lubridate)
library(stringr)
library(ggplot2)
library(ggthemes)
library(viridis)
library(broom)
# library(ggmap)
library(maptools)
library(rgeos)
# library(rgdal)
library(magrittr)
library(dplyr)
library(scales)
library(ggforce)
library(dplyr)
library(ggrepel)
library(animation)
library(gganimate)
library(gepaf)
library(magick)
library(sf)
library(extrafont)
library(tidyr)
library(DBI)
library(dbplyr)
library(foreach)
library(glue)

# extrafont::loadfonts("win")


#========================================#
# Loading data ----
#========================================#

#---------------------------------#
# Mapping data ----
#---------------------------------#


## City boundaries ##

city_sptl_df <- 
    read_rds("./data/city_sptl_df.RDS") %>% 
    as_tibble()


## Park lines ##

park_sptl_df <- 
    read_rds("./data/park_sptl_df.RDS") %>% 
    as_tibble()


## Streets ##

streets_sptl_df <-
    read_rds("./data/streets_sptl_df_2.RDS") %>%
    as_tibble()


#---------------------------------#
# Citi bike Trips ----
#---------------------------------#


citibike_trip_db <- dbConnect(RSQLite::SQLite(), "citibike_trip_db.sqlite3")


citibike_trips_subset <- 
    tbl(citibike_trip_db, "citibike_trips") %>%
    filter(year == 2017 & month == 7 & day == 13) %>%
    select(
        month,
        day,
        year,
        bike_id,
        start_time,
        start_station_id,
        start_station_name,
        start_station_longitude,
        start_station_latitude,
        stop_time,
        end_station_id,
        end_station_name,
        end_station_longitude,
        end_station_latitude, 
        trip_duration
    ) %>%
    collect() %>% 
    mutate(
        start_time = as_datetime(start_time, tz = "US/Eastern"),
        stop_time  = as_datetime(stop_time,  tz = "US/Eastern"))


### Counting each bike's trips in time period ###


bike_id_count <-
    citibike_trips_subset %>% 
    group_by(day) %>% 
    count(bike_id) %>% 
    # ungroup() %>% 
    arrange(-n)


### Selecting the bike with the most trips ###

most_trips <- 
    bike_id_count %>% 
    arrange(-n) %>% 
    slice(1) %>%
    pull(bike_id) %>% 
    as.character() %>% 
    as.integer()


one_bike <- 
    citibike_trips_subset %>% 
    filter(bike_id %in% most_trips) %>% 
    arrange(start_time) %>% 
    group_by(day, bike_id) %>% 
    mutate(stop_num = min_rank(start_time)) %>% 
    mutate(
        start_same_date = start_time,
        stop_same_date  = stop_time) %>% 
    mutate(
        # duration_col = col_numeric(palette = plasma(n=256), domain = NULL)(trip_duration),
        duration_01  = rescale(trip_duration, to = c(0.5, 2.0)))


one_bike %>% 
    group_by(day) %>% 
    count(bike_id) %>% 
    arrange(-n) %>% 
    top_n(1)


one_bike$start_time
one_bike$stop_time


### Kludge for making every trip the same day ###

date(one_bike$start_same_date) <- today()
date(one_bike$stop_same_date)  <- today()


#========================================#
# Creating breaks ----
#========================================#


hour_breaks <- 
    seq(
        as.POSIXct("00:00", format = "%H:%M"),
        as.POSIXct("24:00", format = "%H:%M"),
        "1 hour"
    )

minor_hour_breaks <- 
    seq(
        as.POSIXct("00:30", format = "%H:%M"),
        as.POSIXct("23:30", format = "%H:%M"),
        "1 hour"
    )

minute_breaks_1 <- 
    seq(
        as.POSIXct("00:00", format = "%H:%M"),
        as.POSIXct("24:00", format = "%H:%M"),
        "min"
    )


### Kludge for truncating the empty time with no trips - depends on the bike ###

# minute_breaks <- minute_breaks_1[421:600]
minute_breaks <- minute_breaks_1[381:475]

first(minute_breaks)
last(minute_breaks)

last(minute_breaks) - first(minute_breaks)


#========================================#
# Creating the map background ----
#========================================#

#---------------------------------#
# Building the plot ----
#---------------------------------#

stops_background <-
    ggplot() +
    
    geom_polygon(
        data = city_sptl_df,
        aes(x = long, y = lat, group = group),
        color = "gray68",
        fill = "gray68",
        size = .1
    ) +
    
    
    ## Park shapes ##
    
    geom_polygon(
        data = park_sptl_df,
        aes(x = long, y = lat, group = group),
        color = "gray80",
        fill = "gray60",
        size = 0.1
    ) +
    
    ## Streets ##
    
    geom_path(
        data = streets_sptl_df,
        aes(x = long, y = lat, group = group),
        color = "gray90",
        size = 0.1
    ) +
    
    coord_quickmap(
        xlim = c(-74.0285, -73.9375),
        ylim = c(40.7075, 40.7900),
        expand = FALSE
    ) +
    
    theme_map(base_size = 1) +
    
    theme(
        text = element_text(family = "DejaVu Sans Mono"),
        plot.title = element_text(
            color = "white",
            size = 14,
            hjust = .5
        ),
        # plot.background = element_rect(fill = "red", color = "yellow", size = 2),
        # plot.background = element_rect(fill = "gray5", color = "white", size = 2),*
        plot.background = element_rect(fill = "gray10", color = "white", size = 2.125),
        # plot.background = element_rect(fill = "gray5", color = NA, size = 2),
        # plot.background = element_rect(fill = "gray5", color = "white", size = 2),
        # panel.background = element_rect(fill = "gray10", color = NA, size = 0),*
        panel.background = element_rect(fill = "gray10", color = NA, size = 0),
        # panel.background = element_rect(fill = "green", color = NA, size = 0),
        # panel.border = element_rect(fill = NA, color = "white", size = 2),
        # plot.margin = margin(2, 2, 2, 2, unit = "mm"),
        # plot.margin = margin(0, 0, 0, 0, unit = "mm"),
        plot.margin = margin(1.125, 1.125, 1.125, 1.125, unit = "mm"),
        panel.grid = element_line(color = "gray20", size = .1),
        panel.grid.major = element_line(color = "gray20"),
        panel.grid.minor = element_line(color = "gray20")
    )


#---------------------------------------#
# Manipulating and saving the image ----
#---------------------------------------#


# background <- image_graph(1200, 1418, res = 320, clip = FALSE)
# background <- image_graph(900, 1063, res = 350, clip = FALSE)
background <- image_graph(800, 945, res = 350, clip = FALSE)

print(stops_background)

image_write(background, "stops_plot_background_20.gif")

map_background <- background


#---------------------------------------#
# Reading previously saved image ----
#---------------------------------------#

# map_background <- 
#     image_read(
#         "~/BASP/R analyses/Citi Bike Data/Trip Data/stops_plot_background_20.gif"
#     )


#========================================#
# Creating the map foreground ----
#========================================#


#---------------------------------#
# Building the plot ----
#---------------------------------#


# stops_fg <- image_graph(900, 1063, res = 320, clip = FALSE, antialias = TRUE)
stops_fg <- image_graph(800, 945, res = 350, clip = FALSE, antialias = TRUE)
# stops_fg <- image_graph(1000, 1181.67, res = 350, clip = FALSE, antialias = TRUE)
# stops_fg <- image_graph(600, 709, res = 240, clip = FALSE, antialias = FALSE)
# stops_plot_fg_list <- list()


for(i in 1:nrow(one_bike)) {
# for(i in 1:10) {
    
    # cat(format(hour_breaks[i], "%R"), "-", format(hour_breaks[i+1], "%R"), "\n")
    
    cat("Stop: ", i, "/", nrow(one_bike), "\n", sep = "")
    
    stops_plot_fg <-
        
        ggplot() +
        
        ## Segments (Whole path) ##
        
        geom_segment(
            data = one_bike,
            aes(
                x    = start_station_longitude,
                y    = start_station_latitude,
                xend = end_station_longitude,
                yend = end_station_latitude
            ),
            size = 0.125,
            color = "gray40",
            arrow = arrow(
                angle = 30,
                length = unit(.015, "npc"),
                ends = "last",
                type = "closed"
            )
        ) +
        
        
        # First station label, persistent #
        
        geom_label_repel(
            data = one_bike %>% slice(1),
            aes(
                start_station_longitude,
                start_station_latitude,
                label = start_station_name
            ),
            label.r = unit(0, "lines"),
            force = 0,
            min.segment.length = unit(0.2, "lines"),
            point.padding = unit(0, "lines"),
            label.padding = unit(0.125, "lines"),
            label.size = .15,
            segment.size = .175,
            segment.color = "gray15",
            color = "white",
            fill = "gray15",
            direction = "y",
            nudge_y = 0.00,
            nudge_x = 0.025,
            size = 1,
            fontface = "bold",
            family = "DejaVu Sans"
        ) +  
        
        
        # Cumulative station points, persistent #
        
        geom_point(
            data = one_bike %>% slice(1:i),
            aes(
                start_station_longitude,
                start_station_latitude
            ),
            size = 1,
            color = "black",
            fill = "white",
            shape = 21,
            stroke = .125
        ) +
        
        geom_point(
            data = one_bike %>% slice(i-1),
            
            aes(
                end_station_longitude,
                end_station_latitude
            ),
            size = 1.5,
            color = "black",
            fill = "white",
            shape = 21,
            stroke = .175
        ) +

        
        
        ### Segments (progressive) ###
        
        # Previous segments (t-2) #
        
        geom_segment(
            data = one_bike %>% slice(1:(i-1)),
            aes(
                x    = start_station_longitude,
                y    = start_station_latitude,
                xend = end_station_longitude,
                yend = end_station_latitude
            ),
            size = 0.19,
            color = "gray16",
            lineend = "butt",
            arrow = arrow(
                angle = 30,
                length = unit(.014, "npc"),
                ends = "last",
                type = "closed"
            )
        ) +
        
        # Previous segments (t-1) #
        
        geom_segment(
            data = one_bike %>% slice(i-1),
            aes(
                x    = start_station_longitude,
                y    = start_station_latitude,
                xend = end_station_longitude,
                yend = end_station_latitude
            ),
            size = 0.225,
            color = "black",
            lineend = "butt",
            arrow = arrow(
                angle = 30,
                length = unit(.019, "npc"),
                ends = "last",
                type = "closed"
            )
        ) +
        
        
        ### Current station points ###
        
        # Start stations #
        
        geom_point(
            data = one_bike %>% slice(i), 
            aes(
                start_station_longitude,
                start_station_latitude
            ),
            size = 1.875,
            color = "black",
            fill = "white",
            shape = 21,
            stroke = .125
        ) +
        
        
        # End stations #
        
        geom_point(
            data = one_bike %>% slice(i),
            aes(
                end_station_longitude,
                end_station_latitude,
                size = duration_01*2.5
            ),
            # size = 2,
            color = "gray15",
            fill = "#FF9100",
            shape = 21,
            stroke = 0.1875
        ) +   
        
        
        # Current segment #
        
        geom_segment(
            data = one_bike %>% slice(i),
            aes(
                x    = start_station_longitude,
                y    = start_station_latitude,
                xend = end_station_longitude,
                yend = end_station_latitude,
                size = duration_01
            ),
            # size = 0.375,
            color = "gray15",
            lineend = "butt",
            linejoin = "mitre",
            arrow = arrow(
                angle = 30,
                length = unit(one_bike$duration_01[i]/75, "npc"),
                ends = "last",
                type = "closed"
            )
        ) +
        
        # geom_point(
        #     data = one_bike %>% slice(i),
        #     aes(
        #         start_station_longitude,
        #         start_station_latitude
        #     ),
        #     size = 0.1,
        #     color = "black",
        #     shape = 16
        # ) +
        
        geom_segment(
            data = one_bike %>% slice(i), 
            aes(
                x    = start_station_longitude,
                y    = start_station_latitude,
                xend = end_station_longitude,
                yend = end_station_latitude,
                size = duration_01*(7/10)
            ),
            # size = .125,
            color = "#FF9100",
            lineend = "butt",
            linejoin = "mitre",
            arrow = arrow(
                angle = 30,
                # length = unit(.020, "npc"),
                length = unit(one_bike$duration_01[i]/75, "npc"),
                ends = "last",
                type = "closed"
            )
        ) +
        
        
        ## Station order labels ##
        
        
        # geom_label_repel(
        #     data = one_bike %>% slice(i), 
        #     aes(
        #         start_station_longitude,
        #         start_station_latitude,
        #         label = start_station_name
        #     ),
        #     label.r = unit(0, "lines"),
        #     force = .5,
        #     box.padding = unit(0.1, "lines"),
        #     min.segment.length = unit(0.25, "lines"),
        #     point.padding = unit(0.125, "lines"),
        #     label.padding = unit(0.125, "lines"),
        #     label.size = .15,
        #     segment.size = .15,
        #     segment.color = "black",
        #     direction = "both",
        #     # nudge_y = .015,
        #     size = 1,
        #     color = "black",
        #     fill = "white",
        #     family = "DejaVu Sans"
        # ) +
        
        
        # geom_label_repel(
        #     data = one_bike %>% slice(i), 
        #     aes(
        #         end_station_longitude,
        #         end_station_latitude,
        #         label = end_station_name
        #     ),
        #     label.r = unit(0, "lines"),
        #     force = .5,
        #     box.padding = unit(0.1, "lines"),
        #     min.segment.length = unit(0.25, "lines"),
        #     point.padding = unit(0.35, "lines"),
        #     label.padding = unit(0.125, "lines"),
        #     label.size = .15,
        #     segment.size = .15,
        #     segment.color = "#FF9100",
        #     direction = "both",
        #     # nudge_y = .0075,
        #     size = 1,
        #     color = "#FF9100",
        #     fill = "gray15",
        #     fontface = "bold",
        #     family = "DejaVu Sans"
        # ) +
        
        
        
        geom_label_repel(
            # data = one_bike %>% slice(i), 
            
            data = one_bike %>%
                ungroup() %>% 
                slice(i) %>%
                select(
                    start_station_longitude, 
                    end_station_longitude,
                    start_station_latitude,
                    end_station_latitude,
                    start_station_name,
                    end_station_name
                    ) %>%
                gather(key = start_stop,
                       value = lon,
                       start_station_longitude,
                       end_station_longitude) %>% 
                transmute(
                    lon = lon,
                    lat  = c(start_station_latitude[1], end_station_latitude[1]),
                    name = c(start_station_name[1],     end_station_name[1])
                    ),
            aes(
                lon,
                lat,
                label = name
            ),
            label.r = unit(0, "lines"),
            force = .5,
            box.padding = unit(0.1, "lines"),
            min.segment.length = unit(0.25, "lines"),
            point.padding = unit(0.3, "lines"),
            label.padding = unit(0.125, "lines"),
            label.size = .15,
            segment.size = .15,
            direction = "both",
            nudge_y = .0075,
            size = 1,
            segment.color = c("black", "#FF9100"),
            color = c("black", "#FF9100"),
            fill = c("white", "gray15"),
            fontface = c("plain", "bold"),
            family = "DejaVu Sans"
        ) +
        
        coord_quickmap(
            xlim = c(-74.0285, -73.9375),
            ylim = c(40.7075, 40.7900),
            expand = FALSE
        ) +
        
        # scale_size_continuous(range = c(0.5, 3.0), guide = FALSE) +
        scale_size_identity() +
        
        theme_map(base_size = 1) +
        
        theme(
            text = element_text(family = "DejaVu Sans Mono"),
            plot.title = element_text(color = "white", size = 14, hjust = .5),
            plot.margin = margin(1.125, 1.125, 1.125, 1.125, unit = "mm"),
            plot.background = element_rect(fill = "gray99", color = NA), 
            panel.background = element_rect(fill = "gray99", color = NA)
        )
    
    plot(stops_plot_fg)
    
    # stops_plot_fg_list[[i]] <- stops_plot_fg
    
    # ggsave(
    #     filename = paste0("images/stops_plot_fg.", i, ".png"), 
    #     plot = stops_plot_fg, 
    #     width = 3.75, 
    #     height = 4.43125, 
    #     dpi = 320)
    
}

dev.off()


#-----------------------------------------#
# Combining foreground and background ----
#-----------------------------------------#


### Making the background transparent ###


system.time(
    
    stops_fg_trans <- 
        image_transparent(
            image = stops_fg, 
            color = "gray99")
    
)


### Resizing ###


# stops_fg_bg <- 
#     image_apply(
#         stops_fg_trans,
#         function(x) {
#             image_composite(image_scale(map_background, geometry = "600x"), x)
#         })


### Overlaying forground on background ###


system.time(
    
    stops_fg_bg <- 
        image_apply(
            stops_fg_trans,
            function(x) {
                image_composite(map_background, x)
            })
    
)

# stops_fg_bg_animate <- image_animate(stops_fg_bg, fps = 4, dispose = "none")

# image_write(
#     image_animate(
#         stops_fg_bg, 
#         fps = 4, 
#         dispose = "none"), 
#     "stops_plot_animate_15.gif")

# system.time(
# 
# image_write(
#     image_animate(
#         image_scale(stops_fg_bg, geometry = "800x"), 
#         fps = 4, 
#         dispose = "none"), 
#     "stops_plot_animate_17.gif")
# 
# )


### Animating the image ###


system.time(
    
    stops_fg_bg_animate <- 
        image_animate(
            stops_fg_bg,
            fps = 0.5,
            dispose = "none")
    
)


### Saving the image ###

system.time(
    
    image_write(stops_fg_bg_animate, "stops_plot_animate_25.gif")
    
)


#=========================================#
# Creating a visual timeline ----
#=========================================#


#-----------------------------------------#
# Building the plot ----
#-----------------------------------------#


### Saving every frame to a list so that I can manipulate them later ###


timeline_plot_gt_list <- list()

for(i in 1:nrow(one_bike)) {
# for(i in 1:5) {
    
    cat("Stop: ", i, "/", nrow(one_bike), "\n", sep = "")
    
    timeline_plot <-
        ggplot(one_bike, aes(x = hour_breaks)) +
        scale_x_time(
            "",
            breaks = hour_breaks,
            labels = hour_breaks %>% format(format = "%l:%M"),
            limits = c(hour_breaks[1] - minutes(30), hour_breaks[25] + minutes(30))
        ) +
        
        geom_rect(
            # data = one_bike %>% slice(i),
            # data = one_bike,
            aes(
                xmin = start_same_date[i],
                xmax = stop_same_date[i],
                ymin = 0,
                ymax = 1
            ),
            # color = "gray80",
            fill  = "#FF9100",
            # size = 0.15,
            inherit.aes = FALSE
            ) +
        
        geom_vline(xintercept = hour_breaks,
                   color = "gray0",
                   size = 0.100) +
        geom_vline(xintercept = minor_hour_breaks,
                   color = "gray20",
                   size = 0.075) +
        
        geom_label(
            data = one_bike %>%
                ungroup() %>% 
                slice(i) %>%
                select(start_same_date, stop_same_date) %>%
                gather(key = start_stop, value = time) %>%
                add_column(y = 1.25),
            aes(x = time,
                y = y,
                label = format(time, format = "%l:%M")
                # color = duration_col
                ),
            color = c("gray20", "#FFA52F"),
            # fill = c("white", "white"),
            fill = c("white", "gray15"),
            size = 1.125,
            nudge_y = .1,
            nudge_x = c(-2350, 2350),
            label.r = unit(0, "lines"),
            label.padding = unit(0.05, "lines"),
            label.size = .1,
            family = "DejaVu Sans",
            fontface = c("plain", "plain"),
            hjust = "middle"
        ) +
        
        ggtitle(
            one_bike$start_time[i] %>% date() %>% format("%x"),
            subtitle = paste0(i, "/", nrow(one_bike))
            ) +
        
        theme_minimal() +
        
        coord_cartesian(
            xlim = c(hour_breaks[1], hour_breaks[25]),
            ylim = c(0, 1),
            expand = FALSE
        ) +
        
        
        # scale_color_identity() +
        # scale_fill_identity() +
        
        theme(
            axis.text.x = element_text(
                # family = "Times",
                family = "DejaVu Sans",
                color = "white",
                size = 2.5,
                angle = 0,
                vjust = .5,
                hjust = .5,
                margin = margin(.875, 0, 0, 0)
            ),
            plot.title = element_text(
                family = "DejaVu Sans",
                # family = "Ubuntu Condensed",
                color = "white",
                size = 5,
                hjust = .5,
                margin = margin(2, 0, 0, 0)
            ), 
            plot.subtitle = element_text(
                family = "DejaVu Sans",
                # family = "Ubuntu Condensed",
                color = "#FFA52F",
                size = 3.25,
                hjust = .5,
                margin = margin(0.25, 0, 6, 0)
            ),
            # plot.subtitle = element_text(
            #     family = "DejaVu Sans",
            #     color = "#FF9100",
            #     face = "bold",
            #     size = 4,
            #     hjust = .5,
            #     margin = margin(2, 0, 2, 0)
            # ), 
            
            plot.margin = margin(3, 8, -10, 8),
            panel.grid = element_blank(),
            axis.ticks.x = element_line(color = "gray5", size = .125),
            axis.ticks.length = unit(0, "points"),
            panel.border = element_rect(fill = NA, color = "gray0", size = .2),
            plot.background = element_rect(fill = "gray10", color = "white", size = 2.25),
            panel.background = element_rect(fill = "gray30", color = NA, size = .225),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.y = element_blank()
        )
    
    
    ### Manipulating the plot data to plot out of bounds ###
    
    
    timeline_plot_gt <- ggplot_gtable(ggplot_build(timeline_plot))
    timeline_plot_gt$layout$clip[timeline_plot_gt$layout$name == "panel"] <- "off"
    
    
    ### Saving each frame to a list ###
    
    timeline_plot_gt_list[[i]] <- timeline_plot_gt
}


#-----------------------------------------#
# Animating and saving the timeline ----
#-----------------------------------------#


# timeline <- image_graph(1200, 190, res = 350, clip = FALSE)
timeline <- image_graph(800, 160, res = 320, clip = FALSE)


### Sending each frame to the plot device ###


system.time(
    
    lapply(timeline_plot_gt_list, plot)
    
)

dev.off()


### Animating ###


system.time(
    
    timeline_animate <- 
        image_animate(
            timeline,
            fps = 0.25,
            dispose = "none"
        )
    
)


### Saving the timeline gif ###


system.time(
    
    image_write(timeline_animate, "timeline_animate_25.gif")
    
)


#=========================================#
# Combining the main plot and timeline ----
#=========================================#

#-----------------------------------------#
# Loading previously saved ----
#-----------------------------------------#

# stops_fg_bg_animate <- image_read("stops_plot_animate_25.gif")
# timeline_animate   <- image_read("timeline_animate_24.gif")

#-----------------------------------------#
# Building from the environment ----
#-----------------------------------------#


## Joining the i-th main and timeline frame, appending the joined frame to a list, 
##  then joining the list, producing a single gif


stops_timeline_animate <- 
    image_join(
        
        foreach(i = 1:length(timeline_animate)) %do% 
            
            image_append(
                image_join(
                    timeline_animate[i],
                    stops_fg_bg_animate[i]),
                stack = TRUE)
    )

# stops_timeline_animate


### Saving the combined plot ###


system.time(
    
    image_write(
        stops_timeline_animate,
        "stops_timeline_animate_25.gif")
    
)

# system.time(
#     
#     image_write(
#         image_scale(
#             stops_timeline_animate, 
#             "400x"
#         ),
#         "stops_timeline_animate_18_400.gif")
#     
# )


################################################
################################################

stops_timeline_animate <- image_read("plots/stops_timeline_animate_25.gif")

foreach(i = 1:length(stops_timeline_animate)) %do%
    
    image_write(
        stops_timeline_animate[i],
        path = glue("plots/one_bike_frames/stops_timeline_animate_{i}.png"), 
        format = "png"
    )

################################################
################################################
