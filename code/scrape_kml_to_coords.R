##########################################################-
##########################################################-
##
## Citibike trips plotting ----
##
##########################################################-
##########################################################-

#========================================================#
# Loading packages ----
#========================================================#

library(tidyverse)
library(rvest)
library(maptools)
library(RgoogleMaps)
library(ggmap)
library(geosphere)
library(ggforce)
library(glue)
library(iterators)
library(fs)


#========================================================#
# Create JavaScript function ----
#========================================================#

#--------------------------------------------------------#
# write the javascript code to a new file, scrape.js ----
#--------------------------------------------------------#

writeLines(
    "var url = 'http://ridewithgps.com';
    var page = new WebPage();
    var fs = require('fs');

    page.open(url, function (status) {
            just_wait();
    });

    function just_wait() {
        setTimeout(function() {
                   fs.write('./data/ride_pages/results.html', page.content, 'w');
                phantom.exit();
        }, 2500);
    }
    ",
    con = "./code/functions/scrape.js")


#--------------------------------------------------------#
# Creating scraping function ----
#--------------------------------------------------------#

## Adapted from http://blog.brooke.science/posts/scraping-javascript-websites-in-r/

## Set path to PahntomJS ##

phantompath <- 
    shortPathName("C:/Program Files (x86)/phantomjs/phantomjs-2.1.1-windows/bin/phantomjs.exe")


## Scraper function ##

js_scrape <- 
    
    function(
        url,
        js_path = "./code/functions/scrape.js",
        html_seq,
        phantompath = phantompath) {
        
        ## This section will replace the url in scrape.js to whatever you want ##
        
        lines <- readLines(js_path)
        lines[1] <- paste0("var url ='", url , "';")
        writeLines(lines, js_path)
        
        ## Creating system command ##
        
        command <- paste(phantompath, js_path, sep = " ")
        system(command, show.output.on.console = FALSE)
        
    }


## Making function robust to errors ##

js_scrape_possibly <- possibly(.f = js_scrape, otherwise = NULL)


#========================================================#
# Calibrating search grid spacing ----
#========================================================#

#--------------------------------------------------------#
# Creating map circle function ----
#--------------------------------------------------------#

## Adpated from https://stackoverflow.com/a/29133886

circle_coords <- 
    
    function(lon, lat, r) {
        
    # lat = latitude in decimal degrees of the center of the circle
    # lon = longitude in decimal degrees
    # r = radius of the circle in kilometers
        
    ER <- 6371                   # Mean Earth radius in kilometers.
    AngDeg  <- seq(0:360)        # angles in degrees 
    Lat1Rad <- lat * (pi/180)    # Latitude of the center of the circle in radians
    Lon1Rad <- lon * (pi/180)    # Longitude of the center of the circle in radians
    AngRad  <- AngDeg * (pi/180) # angles in radians
    
    # Latitude of each point of the circle rearding to angle in radians:
    
    Lat2Rad <- asin(sin(Lat1Rad) * cos(r / ER) + cos(Lat1Rad) * sin(r / ER) * cos(AngRad)) 
    
    # Longitude of each point of the circle rearding to angle in radians:
    
    Lon2Rad <- 
        Lon1Rad +
        atan2(sin(AngRad) * sin(r / ER) * cos(Lat1Rad),
              cos(r / ER) - sin(Lat1Rad) * sin(Lat2Rad))
    
    # Latitude of each point of the circle rearding to angle in degrees:
    
    lat_deg <- Lat2Rad * (180/pi)
    
    # Longitude of each point of the circle rearding to angle in degrees:
    
    lon_deg <- Lon2Rad * (180/pi)
    
    # Return data frame of coordinates
    
    return(tibble(lon = lon_deg, lat = lat_deg))
    
}


#--------------------------------------------------------#
# Downloading map ----
#--------------------------------------------------------#

m1 <-
    get_map(
        location = c(lon = -73.971806, lat = 40.767907), # central park zoo
        maptype = "terrain-lines",
        zoom = 11,
        source = "stamen"
    )


#--------------------------------------------------------#
# Plotting circles ----
#--------------------------------------------------------#

## Spacing of circle center points ##

crosstown_distances <- seq(0, 2250*3,  2250)
uptown_distances    <- seq(0, 2250*10, 2250)

origin <- c(-74.035633, 40.685032) # southwest-most point, the start of the grid


## Placing grid over Manhattan ##

### Points across town ###

crosstown_points <- 
    destPoint(
        p = origin,
        b = 115, # Approximate angle of Manhattan cross-streets wrt true north
        d = crosstown_distances)


### Points uptown ###

uptown_points <-
    plyr::adply(
        .data = crosstown_points,
        .margins = 1,
        .fun = function(x) {
            destPoint(p = x,
                      b = 25, # Approximate angle of Manhattan avenues wrt true north
                      d = uptown_distances)
        }
    ) %>% 
    as_tibble() %>% 
    select(-X1) %>%
    rename(lon_orig = lon, lat_orig = lat) %>% 
    group_by(point = row_number())


### Saving points ###

write_rds(uptown_points, "./data/uptown_points.rds")


## Creating points to draw circles ##

circle_coords_df <-
    uptown_points %>%
    do(
        circle_coords(
            lon = .$lon_orig,
            lat = .$lat_orig,
            r = 1.6)) %>%
    left_join(uptown_points, ., by = "point") %>% 
    as_tibble()

### Saving coordinates ###

write_rds(circle_coords_df, "./data/circle_coords_df.rds")


## Reading in saved files ##

# circle_coords_df <- read_rds("circle_coords_df.rds")


## Plotting map with circles ##

ggmap(m1, extent = "device") +
    geom_point(aes(x = origin[1], y = origin[2]), color = "black", shape = 8, size = 3) +
    geom_point(data = uptown_points, aes(x = lon, y = lat), color = "green", size = 3) +
    geom_path(data = circle_coords_df, aes(x = lon, y = lat, group = point), color = "red", size = .1) +
    coord_quickmap(expand = FALSE)


#========================================================#
# Scraping rides ----
#========================================================#

#--------------------------------------------------------#
# Setting parameters ----
#--------------------------------------------------------#

## Results-per-page offset ##

offset_seq <- seq(0, 975, 25) # Max offset is 999


## Start distance radius ##

start_distance <- 1

## Because of the limited offset values available, the above code will not 
##  download complete results from every map point query. To get around this
##  limitation, I'm downloading up to 1000 rides <= 50 miles long, in descending
##  order of distance; then, downloading all the rides <= 20 miles long, in
##  ascending order of distance. This should capture nearly all of the rides
##  available at each queried point.

# max_distance <- 50
max_distance <- 20
# sorting <- "desc"
sorting <- "asc"


#--------------------------------------------------------#
# Downloading rides as html files ----
#--------------------------------------------------------#

## Pretty printing ##

cat("###===###===###===###===###===###===###===###===###===###===###===###\n")
cat("#\n")
cat("# Start distance:",  start_distance,
    "-- Max distance:", max_distance,
    "-- Sorted by:", "length &", sorting,
    "\n")
cat("#\n")
cat("###===###===###===###===###===###===###===###===###===###===###===###\n")


## Looping through points on the map

for (i in 1:nrow(uptown_points)) {
    
    ## Pretty printing ##
    
    cat("==================================================\n")
    cat("Point:",  uptown_points$point[i],
        "-- lon:", uptown_points$lon_orig[i],
        "-- lat:", uptown_points$lat_orig[i],
        "\n")
    cat("==================================================\n")
    
    
    ## Looping through offset values ##
    
    for (j in 1:length(offset_seq)) {
        
        ## Pretty printing ##
        
        cat(
            "Loop:", j, "|", 
            "Offset:", offset_seq[j],
            "\n"
        )
        
        
        ## Constructing URL ##
        
        url <- 
            glue(
                "https://ridewithgps.com/find#search/0/search[offset]=", 
            
                # offset:
                "{offset_seq[j]}", 
                #-----------------#
                
                "&search[start_distance]={start_distance}&search[start_location]=",
                
                # coords:
                "{uptown_points$lat_orig[i]},+{uptown_points$lon_orig[i]}", 
                #-----------------#
                
                "&search[keywords]=&search[length_min]=0&search[length_max]=",
                
                # Max ride distance:
                "{max_distance}", 
                #-----------------#
                
                "&search[elevation_min]=0&search[elevation_max]=10000&",
                "search[recreation_types][]=3&search[recreation_types][]=4&",
                "search[recreation_types][]=10&search[sort_by]=length+{sorting}"
        )
        
        
        ## Applying scaper function to constructed URL ##
        
        js_scrape(
            url = url,
            html_seq = offset_seq[j]
        )
        
        
        ## Renaming each downloaded file ##
        
        file_move(
            "./data/ride_pages/results.html",
            glue(
                "./data/ride_pages/point_{uptown_points$point[i]};",
                "max_dist_{max_distance};",
                "offset_{offset_seq[j]}.html"
            )
        )
        
        
        ## Setting a stopping rule based on the downloaded file size ##
        
        file_size <-
            file_info(
                glue(
                    "./data/ride_pages/point_{uptown_points$point[i]};",
                    "max_dist_{max_distance};",
                    "offset_{offset_seq[j]}.html"
                )
            )$size
        
        cat("File size: ")
        print(file_size)
        
        cat("\n--------------------------------------------------\n")
        
        if (file_size < as_fs_bytes("100K"))
            break
        
    }
}
    

#--------------------------------------------------------#
# Reading downloaded html files and extracting rides ----
#--------------------------------------------------------#

## Getting file names ##

# ride_pages <- dir_ls("./data/ride_pages") %>% as.character() %>% str_subset("max_dist_50")
ride_pages <- dir_ls("./data/ride_pages") %>% as.character() %>% str_subset("max_dist_20")


## Extracting ride results ##

# trip_ids_50 <-
trip_ids_20 <-
    plyr::ldply(
        .data = ride_pages,
        .fun = function(x) {
            read_html(x) %>%
                html_nodes("div > .result_details") %>%
                html_nodes("a[href!='javascript:;']") %>%
                html_attrs() %>%
                flatten_chr() %>%
                .[!str_detect(., "/users")] %>%
                .[!str_detect(., "/events")] %>% 
                tibble(trip_url = .)
        }
    ) %>% 
    as_tibble() %>% 
    distinct()


## Saving trip ids ##

# write_rds(trip_ids_50, "./data/trip_ids_50.rds")
write_rds(trip_ids_20, "./data/trip_ids_20.rds")


#--------------------------------------------------------#
# Combining trip id files ----
#--------------------------------------------------------#


## Reading in saved files ##

# trip_ids_50 <- read_rds("./data/trip_ids_50.rds")
# trip_ids_20 <- read_rds("./data/trip_ids_20.rds")


## Getting unique ids missed on the "max_dististance = 50" first pass ##

# trip_ids_20_distinct <- anti_join(trip_ids_20, trip_ids_50)


## Combining ids into complete unique set ##

trip_ids_combined <- full_join(trip_ids_20, trip_ids_50)


#--------------------------------------------------------#
# Downloading kml files & extracting coordinates ----
#--------------------------------------------------------#

## Constructing function to download kml coordinates, which are exportable ##

trip_ids_to_latlon <-
    function(trip_id_df) {
        
        kml_latlon <-
            read_html(glue("https://ridewithgps.com{trip_id_df$trip_url}.kml")) %>% 
            html_nodes("coordinates") %>% 
            html_text() %>% 
            read_csv(col_names = c("lon", "lat", "elev")) %>% 
            mutate(trip_url = str_remove(trip_id_df$trip_url, "/")) %>% 
            group_by(trip_url) %>% 
            nest(.key = "coordinates")
        
        return(kml_latlon)
    }


## Making function robust to errors ##

trip_ids_to_latlon_possibly <- possibly(.f = trip_ids_to_latlon, otherwise = data_frame())


## Extracting coordinates ##

coord_df_comb <-
    # trip_ids_50 %>% 
    # trip_ids_20_distinct %>% 
    trip_ids_combined %>% 
    rowwise() %>% 
    do(trip_ids_to_latlon_possibly(.)) %>% 
    ungroup()


## Saving coordinate files ##

# write_rds(coord_df, "./data/coord_df_50.rds")
# write_rds(coord_df, "./data/coord_df_20.rds")


## Reading in saved files ##

# coord_df_50 <- read_rds("./data/coord_df_50_backup.rds")
# coord_df_20 <- read_rds("./data/coord_df_20.rds")

# coord_df_comb <- bind_rows(coord_df_50, coord_df_20)

write_rds(coord_df_comb, "./data/coord_df_comb.rds")


#--------------------------------------------------------#
# Adding Google encoded polyline ----
#--------------------------------------------------------#

coord_df_comb_poly <- 
    coord_df_comb %>% 
    rowwise() %>% 
    mutate(polyline = unnest(coordinates) %>% encode(.)) 


## Saving file with polyline ##

write_rds(coord_df_comb_poly, "./data/coord_df_comb_poly.rds")


#################################################################################################
#################################################################################################
