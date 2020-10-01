###########################################################################################-
###########################################################################################-
##
## glmmTMB on April data - hours x weekend ---
##
###########################################################################################-
###########################################################################################-

#=========================================================================================#
# Setting up ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Loading libraries ----
#-----------------------------------------------------------------------------------------#

library(car)
library(lme4)
library(glmmTMB)
library(bbmle)
library(tidyverse)
library(tictoc)
library(glue)
library(lubridate)
library(fs)
library(nloptr)
library(here)
library(hms)
library(performance)
library(installr)
library(scales)
library(broom.mixed)
library(pryr)
library(ggdark)

#-----------------------------------------------------------------------------------------#
# Specifying directories and names
#-----------------------------------------------------------------------------------------#

data_dir   <- "data/2018"
plots_dir  <- "plots/time_series/2018"
output_dir <- "output/2018"

dir_create(plots_dir)
dir_create(output_dir)

csv_name <- "2018_fits_2020-09-07"

#-----------------------------------------------------------------------------------------#
# custom functions
#-----------------------------------------------------------------------------------------#

source("code/functions/contr.Sdif.R")
source("code/functions/diagnostic_plots.R")
source("code/functions/glmmTMB_size_testing.R")
source("code/functions/se_funs.R")
source("code/functions/resid_patterns.R")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# custom optimizer controls
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

optCtrl <-
    list(
        iter.max = 10000,
        eval.max = 10000,
        rel.tol = 1e-9,
        x.tol = 0,
        trace = 0
    )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# residual covariance matrix
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

rescov <- 
    
    function(model, struct) {
        
        var.d <- crossprod(struct$condList$reTrms$Lambdat)
        Zt <- struct$condList$reTrms$Zt
        vr <- sigma(model)^2
        var.b <- vr*(t(Zt) %*% var.d %*% Zt)
        sI <- vr * Diagonal(nrow(model.frame(model)))
        var.y <- var.b + sI
        invisible(var.y)
        
    }

#-----------------------------------------------------------------------------------------#
# Loading data
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Loading previously sampled data from RDS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trips_station_weather_data <- 
    read_rds(
        path(
            data_dir, 
            "trips_station_weather_data_extra_2018.rds" 
        )
    )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Loading fit stats spreadsheet
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

fit_stats <- read_csv(path(output_dir, csv_name) %>% path_ext_set("csv"))


#=========================================================================================#
# model fitting ----
#=========================================================================================#

if (!any(ls() %in% "fit_info")) fit_info <- list()

fit_info[[length(fit_info) + 1]] <- 
    
    glmmTMB_size_testing(
        
        trips ~ 
            
            hour * workday + 
            
            precip_yn +
            
            temperature_45_c15 +
            I(temperature_45_c15 ^ 2) + 
            
            daily_uv_index_10_c5 +
            I(daily_uv_index_10_c5 ^ 2) +
            
            humidity_100_c60 +
            I(humidity_100_c60 ^ 2) +
            
            dew_point_45_c7 +
            I(dew_point_45_c7 ^ 2) +
            
            total_trend_yearly +
            
            sin_year_r_2 +
            cos_year_r_2 +
            
            capacity_med_30_c30 +
            in_service_fct +
            
            us(1 | start_station_name) +
            
            ar1(0 + ts_yday | start_station_name) + 
            ar1(0 + hour | start_station_name) + 
            
            ar1(0 + hour:workday | start_station_name) + 
            ar1(0 + hour:ts_yday | start_station_name) + 
            
            ar1(0 + hour:wday:workday | start_station_name) + 
            ar1(0 + hour:precip_yn:workday | start_station_name) +
            
            ar1(0 + hour:precip_yn:temperature_45_c15 | start_station_name) +
            ar1(0 + hour:precip_yn:humidity_100_c60 | start_station_name) +
            
            ar1(0 + hour:workday:temperature_45_c15 | start_station_name) +
            ar1(0 + hour:workday:humidity_100_c60 | start_station_name),
            
        data = trips_station_weather_data,
        months = 3:5,
        n_stations = 50,
        family = "genpois",
        seed = 726, 
        output_dir = output_dir,
        verbose = FALSE,
        profile = TRUE,
        optCtrl = optCtrl,
        csv_name = csv_name
    )

invisible(gc())

fit_list_elem <- length(fit_info)

names(fit_info)[fit_list_elem] <- fit_info[[fit_list_elem]]$fit_name

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Loading updated fit stats spreadsheet
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

fit_stats <- read_csv(path(output_dir, csv_name) %>% path_ext_set("csv"))


#=========================================================================================#
# augment & plot ----#
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# augment
#-----------------------------------------------------------------------------------------#

fit_list_elem <- length(fit_info)

if (!hasName(fit_info[[fit_list_elem]], "fit_aug")) {
    
    fit_info[[fit_list_elem]]$fit_aug <-
        trips_station_weather_data %>% 
        filter(month(date_time_hourly) %in% fit_info[[fit_list_elem]]$fit_months) %>%
        filter(
            start_station_name %in% 
                unique(model.frame(fit_info[[fit_list_elem]]$fit_obj)$start_station_name)
        ) %>%
        arrange(start_station_name, date, hour) %>% 
        mutate(
            .fitted_resp = predict(fit_info[[fit_list_elem]]$fit_obj, type = "response"),
            .resid_resp = .fitted_resp - trips,
            resid_resp_direction = if_else(.resid_resp < 0, "un", "ov"),
            wday_hour = str_c(wday, hour, sep = " ") %>% as_factor() %>% as.ordered(),
            wday_hour = 
                fct_relevel(
                    wday_hour, 
                    ~ map(
                        levels(wday), 
                        ~ str_c(.x, levels(hour), sep = " ")
                    ) %>% flatten_chr()
                ),
            route_count_fct = as.factor(route_count),
            entrance_count_fct = as.factor(entrance_count),
            closest_distance_fct = as.factor(closest_distance),
            capacity_fct = as.factor(capacity)
        ) %>% 
        
        # which type of fitted values?
        
        mutate(
            fitted = .fitted_resp,
            resid = .resid_resp,
            resid_direction = resid_resp_direction
        )
    
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# saving whole data info object --
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

write_rds(
    fit_info[[fit_list_elem]], 
    path(
        output_dir, 
        str_c(fit_info[[fit_list_elem]]$fit_name, "_fit_info")
    ) %>% path_ext_set("rds"),
    compress = "gz"
)

#-----------------------------------------------------------------------------------------#
# diagnostic plots ----#
#-----------------------------------------------------------------------------------------#

suppressMessages(
    diagnostic_plots(
        fit_info = fit_info[[fit_list_elem]],
        col_var = "day_period",
        row_var = "wday_wend",
        station_ribbons = TRUE,
        plots.path = plots_dir, 
        output.path = output_dir
    )
)

#-----------------------------------------------------------------------------------------#
# Residual pattern plots
#-----------------------------------------------------------------------------------------#

# general

resid_plot(total_trend_yearly, fit_info[[fit_list_elem]])

# time

resid_plot(wday, fit_info[[fit_list_elem]])
resid_plot(hour, fit_info[[fit_list_elem]])

# weather in model

resid_plot(temperature_45_c15, fit_info[[fit_list_elem]])

# new weather

resid_plot(dew_point_45_c7, fit_info[[fit_list_elem]])
resid_plot(humidity_100_c60, fit_info[[fit_list_elem]])
resid_plot(sky_cover, fit_info[[fit_list_elem]])
resid_plot(heat_index_45_c15, fit_info[[fit_list_elem]])
resid_plot(daily_uv_index_10_c5, fit_info[[fit_list_elem]])
resid_plot(daily_ozone_300_c300, fit_info[[fit_list_elem]])


#-----------------------------------------------------------------------------------------#
# Covariance pattern plots  --
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# getting preprocessed data and parameters
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

dofit <- 
    update(
        fit_info[[fit_list_elem]]$fit_obj, 
        data = model.frame(fit_info[[fit_list_elem]]$fit_obj), 
        family = family(fit_info[[fit_list_elem]]$fit_obj)$family, 
        doFit = FALSE, 
        verbose = FALSE, 
        ziformula = ~0, 
        dispformula = ~1, 
        control = glmmTMBControl(parallel = 8, optCtrl = list(iter.max = 10000, eval.max = 10000))
    )

invisible(gc())

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# constructing matrix
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

fit_rescov <- 
    rescov(fit_info[[fit_list_elem]]$fit_obj, dofit) %>% 
    cov2cor()

rm(dofit)

invisible(gc())

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# saving images
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

fit_folder <- path(here(), plots_dir, fit_info[[fit_list_elem]]$fit_name)

station_obs <- 
    fit_info[[fit_list_elem]]$fit_obj %>% 
    model.frame() %>% 
    count(start_station_name) %>% 
    pull(n) %>% 
    max()

sizes <- c(10, 50, 100, station_obs/2, station_obs) %>% round(0)


png(path(fit_folder, glue("varcov_1-{sizes[1]}.png")), width = 1000, height = 1000, res = 100)
image(
    fit_rescov[1:sizes[1], 1:sizes[1]],
    useRaster = TRUE,
    colorkey = TRUE,
    col.regions = 
        c(
            viridis_pal(direction = 1, option = "E")(6),
            viridis_pal(direction = 1, option = "A")(7),
            viridis_pal(direction = -1, option = "D")(7)
        ),
    at = seq(0, 1, 0.05)
)
dev.off()



png(path(fit_folder, glue("varcov_1-{sizes[2]}.png")), width = 1000, height = 1000, res = 100)
image(
    fit_rescov[1:sizes[2], 1:sizes[2]],
    useRaster = TRUE,
    colorkey = TRUE,
    col.regions = 
        c(
            viridis_pal(direction = 1, option = "E")(6),
            viridis_pal(direction = 1, option = "A")(7),
            viridis_pal(direction = -1, option = "D")(7)
        ),
    at = seq(0, 1, 0.05)
)
dev.off()



png(path(fit_folder, glue("varcov_1-{sizes[3]}.png")), width = 1000, height = 1000, res = 100)
image(
    fit_rescov[1:sizes[3], 1:sizes[3]],
    useRaster = TRUE,
    colorkey = TRUE,
    col.regions = 
        c(
            viridis_pal(direction = 1, option = "E")(6),
            viridis_pal(direction = 1, option = "A")(7),
            viridis_pal(direction = -1, option = "D")(7)
        ),
    at = seq(0, 1, 0.05)
)
dev.off()



png(path(fit_folder, glue("varcov_1-{sizes[4]}.png")), width = 2000, height = 2000, res = 100)
image(
    fit_rescov[1:sizes[4], 1:sizes[4]],
    useRaster = TRUE,
    colorkey = TRUE,
    col.regions = 
        c(
            viridis_pal(direction = 1, option = "E")(6),
            viridis_pal(direction = 1, option = "A")(7),
            viridis_pal(direction = -1, option = "D")(7)
        ),
    at = seq(0, 1, 0.05)
)
dev.off()



png(path(fit_folder, glue("varcov_1-{sizes[5]}.png")), width = 2000, height = 2000, res = 100)
image(
    fit_rescov[1:sizes[5], 1:sizes[5]],
    useRaster = TRUE,
    colorkey = TRUE,
    col.regions = 
        c(
            viridis_pal(direction = 1, option = "E")(6),
            viridis_pal(direction = 1, option = "A")(7),
            viridis_pal(direction = -1, option = "D")(7)
        ),
    at = seq(0, 1, 0.05)
)
dev.off()



png(path(fit_folder, glue("varcov_{(sizes[5]+1)}-{sizes[5]+sizes[4]}.png")), width = 2000, height = 2000, res = 100)
image(
    fit_rescov[(sizes[5]+1):(sizes[5]+sizes[4]), (sizes[5]+1):(sizes[5]+sizes[4])],
    useRaster = TRUE,
    colorkey = TRUE,
    col.regions = 
        c(
            viridis_pal(direction = 1, option = "E")(6),
            viridis_pal(direction = 1, option = "A")(7),
            viridis_pal(direction = -1, option = "D")(7)
        ),
    at = seq(0, 1, 0.05)
)
dev.off()


png(path(fit_folder, glue("varcov_{(sizes[5]+1)}-{sizes[5]+sizes[5]}.png")), width = 2000, height = 2000, res = 100)
image(
    fit_rescov[(sizes[5]+1):(2*sizes[5]), (sizes[5]+1):(2*sizes[5])],
    useRaster = TRUE,
    colorkey = TRUE,
    col.regions = 
        c(
            viridis_pal(direction = 1, option = "E")(6),
            viridis_pal(direction = 1, option = "A")(7),
            viridis_pal(direction = -1, option = "D")(7)
        ),
    at = seq(0, 1, 0.05)
)
dev.off()


png(path(fit_folder, glue("varcov_{(sizes[5]*9)+1}-{(sizes[5]*9)+sizes[4]}.png")), width = 2000, height = 2000, res = 100)
image(
    fit_rescov[((sizes[5]*9)+1):((sizes[5]*9)+sizes[4]), ((sizes[5]*9)+1):((sizes[5]*9)+sizes[4])],
    useRaster = TRUE,
    colorkey = TRUE,
    col.regions = 
        c(
            viridis_pal(direction = 1, option = "E")(6),
            viridis_pal(direction = 1, option = "A")(7),
            viridis_pal(direction = -1, option = "D")(7)
        ),
    at = seq(0, 1, 0.05)
)
dev.off()


png(path(fit_folder, glue("varcov_{(sizes[5]*9)+1}-{(sizes[5]*9)+sizes[5]}.png")), width = 2000, height = 2000, res = 100)
image(
    fit_rescov[((sizes[5]*9)+1):((sizes[5]*9)+sizes[5]), ((sizes[5]*9)+1):((sizes[5]*9)+sizes[5])],
    useRaster = TRUE,
    colorkey = TRUE,
    col.regions = 
        c(
            viridis_pal(direction = 1, option = "E")(6),
            viridis_pal(direction = 1, option = "A")(7),
            viridis_pal(direction = -1, option = "D")(7)
        ),
    at = seq(0, 1, 0.05)
)
dev.off()



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
