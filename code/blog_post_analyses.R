#############################################################-
#############################################################-
##
## Blog post analyses ----
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
library(tictoc)
library(lme4)
library(optimx)
library(rlang)
library(compiler)
library(nloptr)
library(broom)
library(ggmap)

setCompilerOptions(optimize = 3)
enableJIT(level = 3)


#-----------------------------------------------------------#
# Loading functions ----
#-----------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# plotting function ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

source("./code/functions/ts_fitted_plot_stations.R")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# optimizer ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

nlopt <- function(fn, par, lower, upper, control) {
    .nloptr <<- res <-
        nloptr(
            x0     = par,
            eval_f = fn,
            lb     = lower,
            ub     = upper,
            opts = list(
                algorithm = "NLOPT_LN_BOBYQA",
                print_level = 0,
                maxeval = 10000,
                xtol_rel = 1e-7,
                ftol_rel = 1e-7
            )
        )
    return(
        list(
            par   = res$solution,
            fval  = res$objective,
            feval = res$iterations,
            conv  = if (res$status > 0) {
                0
            } else {
                res$status
            }, 
            message = res$message
        )
    )
}

nlopt_cmp <- cmpfun(nlopt)

#-----------------------------------------------------------#
# Loading data from RDS ----
#-----------------------------------------------------------#

trwx_data_stations <-
    read_rds("./data/march_april_may/trips_station_weather_data.rds")

trwx_fit <- read_rds("./output/trwx_fit.rds")

#===========================================================#
# Full model ----
#===========================================================#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_fit_data <-
    trwx_data_stations %>%
    as_tibble() %>%
    select(
        date_time_hourly,
        month,
        wday,
        hour,
        trips,
        trend24,
        temperature_c,
        precip_yn,
        start_station_id
    ) %>%
    drop_na()


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# full model  ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

now(tzone = "US/Eastern")
tic("trwx_fit")

trwx_fit <-
    lmer(
        
        trips ~ 
             trend24 * temperature_c * precip_yn +
            (trend24 * temperature_c * precip_yn | wday/hour) +
            (trend24 * temperature_c * precip_yn | month) +
            (trend24 * temperature_c * precip_yn | start_station_id),
        
        data = trwx_fit_data,
        control = lmerControl(calc.derivs = FALSE, optimizer = "nlopt_cmp")
    )

now(tzone = "US/Eastern")
toc()

tidy(trwx_fit) %>% mutate_if(is.numeric, round, digits = 2)


### visualizing model fit ----

trwx_fit_aug <- 
    ts_fitted_plot_stations(
        fit = trwx_fit,
        data = trwx_fit_data,
        path = "./plots/time_series/trwx_fit_plot.png",
        save = TRUE, 
        return.aug = TRUE
    )

write_rds(trwx_fit, "./output/trwx_fit.rds", compress = "gz")
write_rds(trwx_fit_aug, "./output/trwx_fit_aug.rds", compress = "gz")


#===========================================================#
# Full model ----
#===========================================================#

# All starting stations, without regard to end station, faceted by hour


#### Bikes ####

station_comb.summary_2 %>% 
    ggplot(aes(lon, lat)) +
    geom_point(aes(size = bikes_median, color = bikes_sd), shape = 16) +
    coord_equal() +
    scale_color_viridis(option = "inferno") +
    theme_map()


#### City ####

m1 <-
    get_map(
        c(lon = -73.989908, lat = 40.740883),
        maptype = "terrain",
        zoom = 12,
        scale = 2,
        color = "bw"
    )
ggmap(m1, extent = "device")


#### Combined ####

map4 <-
    ggmap(m4, extent = "device") + 
    geom_point(
        data = station_comb.summary_2,
        aes(lon, lat, size = bikes_median, color = bikes_sd),
        shape = 16
    ) +
    coord_quickmap(expand = FALSE) +
    scale_fill_viridis(option = "inferno") +
    theme_map() +
    theme(legend.position = "none")


map1.1 <-
    ggplot() +
    
    
    ## City outline ##
    
    geom_polygon(
        data = city_sptl_df, 
        aes(x = long, y = lat, group = group),
        color = "gray68",
        fill = "gray68",
        size = .1) +
    
    
    ## Most prevalent origin ##
    
    geom_point(
        data = citibike_trips %>%
            filter(start_station_id == 519) %>%
            distinct(start_station_id, .keep_all = TRUE),
        
        aes(start_station_longitude,
            start_station_latitude),
        alpha = .75,
        size = 4,
        color = "cyan",
        fill = "cyan",
        shape = 21
    ) +
    
    geom_point(
        data = citibike_trips %>%
            filter(start_station_id == 519) %>%
            distinct(start_station_id, .keep_all = TRUE),
        
        aes(start_station_longitude,
            start_station_latitude
        ),
        size = 4.25,
        color = "cyan",
        shape = 3,
        stroke = 0.75) +
    
    
    ## All Trips ##
    
    geom_point(
        data = citibike_trips %>%
            filter(start_station_id == 519) %>%
            count(end_station_id) %>%
            left_join(.,
                      citibike_trips %>%
                          distinct(
                              end_station_id,
                              .keep_all = TRUE
                          )),
        aes(end_station_longitude,
            end_station_latitude,
            color = n,
            size = n
        ),
        shape = 16) +
    
    
    ## Streets ##
    
    # geom_path(
    #     data = streets_sptl_df,
    #     aes(x = long, y = lat, group = group),
    #     color = "gray90",
    #     size = 0.1) +
    
    
## Subway lines ##

# geom_path(
#     data = routes_sptl_df,
#     aes(x = long, y = lat, group = group),
#     color = "gray90",
#     size = 0.1) +

scale_color_viridis(
    "# of trips",
    option = "viridis",
    guide = guide_colorbar(direction = "horizontal")) +
    
    
    # scale_color_gradientn(
    #     "# of trips", 
    #     colors = heat.colors(256),
    #     guide = guide_colorbar(direction = "horizontal")) +
    
    # scale_fill_gradientn(colors = topo.colors(256)) +
    
    # scale_fill_gradientn(colors = brewer.pal(8, name = "YlOrRd")) +
    # scale_fill_gradientn(colors = brewer.pal(6, name = "Spectral")) +

scale_size(guide = "none", range = c(0.5, 5)) +
    
    coord_quickmap(xlim = c(-74.05, -73.91), ylim = c(40.6515, 40.802), expand = FALSE) +
    theme_map() +
    
    theme(
        legend.position = c(0, 0), 
        legend.background = element_rect(fill = "gray50", color = "black"),
        legend.direction = "horizontal",
        # legend.key.height = unit(1, "in"), 
        legend.key.width = unit(.190, "npc"), 
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        plot.background = element_rect(fill = "gray10", color = "gray10"),
        plot.margin = margin(0, 0, 0, 0),
        panel.grid = element_line(color = "gray15"),
        panel.grid.major = element_line(color = "gray15"),
        panel.grid.minor = element_line(color = "gray15")
    )

map1.1

####################################################

ggsave("citi bike trips - 4 days 7.png", map1.1, width = 8.5, height = 12, dpi = 300)
# ggsave("citi bike trips - 4 days 4.pdf", map1.1, width = 8.5, height = 12, dpi = 300)

