###########################################################################################-
###########################################################################################-
##
## predictions on grid ---
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
library(prophet)

#-----------------------------------------------------------------------------------------#
# Specifying directories
#-----------------------------------------------------------------------------------------#

data_dir   <- "data/2018"
plots_dir  <- "plots/time_series/2018"
output_dir <- "output/2018"

fit_name <- "glmmTMB_fit_m345_20s_genpois_141213"

# THIS COULD BE A FUNCTION WITH THE FIT NAME AS THE ARGUMENT

#-----------------------------------------------------------------------------------------#
# Custom functions
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Factor levels to numeric, for `mutate()`
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

fct_2_num <- function(x) as.numeric(as.character(x))

#-----------------------------------------------------------------------------------------#
# Loading data
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Need to load model object, for model.frame
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

if (!any(ls() %in% "fit_info")) fit_info <- list()

fit_list_elem <- 1

fit_info[[fit_list_elem]] <- 
    
    read_rds(
        path(output_dir, str_c(fit_name, "_fit_info")) %>% 
            path_ext_set("rds")
    )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Loading holiday dates
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

us_holidays <- 
    generated_holidays %>% 
    as_tibble() %>% 
    filter(country == "US") %>% 
    mutate(date = as_date(ds))

#-----------------------------------------------------------------------------------------#
# Constructing predictor grid
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Loading holiday dates
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# +/- 5 DEGREES

model_frame <- 
    model.frame(fit_info[[fit_list_elem]]$fit_obj) %>% 
    as_tibble() %>% 
    mutate(
        date = as.Date(fct_2_num(ts_yday)-1, origin = "2018-01-01"),
        date_time_hourly = 
            make_datetime(
                year  = year(date), 
                month = month(date), 
                day   = day(date), 
                hour  = fct_2_num(hour), 
                tz = "US/Eastern"
            )
    )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Loading holiday dates
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

capacity <- 
    model_frame %>% 
    select(start_station_name, capacity_med_30_c30) %>% 
    distinct(start_station_name, .keep_all = TRUE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Loading holiday dates
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

weather <- 
    model_frame %>% 
    filter(start_station_name == unique(first(start_station_name))) %>% 
    select(
        date,
        # date_time_hourly,
        temperature_45_c15,
        humidity_100_c60,
        dew_point_45_c7,
        daily_uv_index_10_c5
    ) %>% 
    transmute(
        date,
        temperature    = (temperature_45_c15*45)+15,
        humidity       = (humidity_100_c60*100)+60,
        dew_point      = (dew_point_45_c7*45)+7,
        daily_uv_index = (daily_uv_index_10_c5*10)+5
    )

# range of temp, but average of the other weather vars

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Loading holiday dates
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

weather_intervals <- 
    weather %>% 
    group_by(date) %>% 
    summarise(
        temperature_mean = mean(temperature, na.rm = TRUE),
        humidity_mean = mean(humidity, na.rm = TRUE),
        dew_point_mean = mean(dew_point, na.rm = TRUE),
        daily_uv_index_mean = mean(daily_uv_index, na.rm = TRUE)
    ) %>% 
    mutate(
        temperature_minus5 = temperature_mean - 5,
        # temperature_minus4 = temperature_mean - 4,
        # temperature_minus3 = temperature_mean - 3,
        # temperature_minus2 = temperature_mean - 2,
        # temperature_minus1 = temperature_mean - 1,
        temperature_mean  = temperature_mean,
        # temperature_plus1 = temperature_mean + 1,
        # temperature_plus2 = temperature_mean + 2,
        # temperature_plus3 = temperature_mean + 3,
        # temperature_plus4 = temperature_mean + 4,
        temperature_plus5 = temperature_mean + 5
    ) %>% 
    pivot_longer(
        cols = contains("temperature"),
        names_to = "temperature_level",
        names_prefix = "temperature_",
        values_to = "temperature"
    ) %>% 
    rename_with(~ str_remove(.x, "_mean")) %>% 
    mutate(
        temperature_level = 
            temperature_level %>% 
            str_replace("minus", "-") %>% 
            str_replace("plus", "+") %>% 
            str_c(., "°") %>% 
            as_factor() %>% 
            fct_relevel(
                "-5°",
                # "-4°",
                # "-3°",
                # "-2°",
                # "-1°",
                "mean°",
                # "+1°",
                # "+2°",
                # "+3°",
                # "+4°",
                "+5°"
            )
    )


# this is a grid of values on the original scale
#   i can use this for user inputs

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Loading holiday dates ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

predictor_grid <- 
    
    expand_grid(
        weather_intervals,
        start_station_name = unique(model_frame$start_station_name),
        hour = 0:23,
        in_service_fct = as_factor(1),
        precip_yn = as_factor(c(0, 1))
        
    ) %>% 
    
    left_join(
        .,
        capacity,
        by ="start_station_name"
    ) %>% 
    
    left_join(
        ., 
        us_holidays %>% mutate(isholiday = TRUE) %>% select(date, isholiday),
        by = "date"
    ) %>% 
    
    # i then turn these into the scales values
    
    mutate(
        
        date_time_hourly = make_datetime(year(date), month(date), day(date), hour, tz = "US/Eastern"),
        
        hour = as_factor(hour),
        
        isholiday = replace_na(isholiday, FALSE),
        
        wday = as_factor(wday(date_time_hourly, label = TRUE)),
        
        wday_wend = 
            case_when(
                wday %in% c("Sun", "Sat") ~ "weekend", 
                TRUE ~ "weekday"
            ) %>% 
            as.factor(),
        
        workday = 
            case_when(
                wday_wend == "weekday" & isholiday == FALSE ~ TRUE, 
                TRUE ~ FALSE
            ) %>% 
            as.factor() %>% 
            fct_rev(),
        
        total_trend_yearly   = (yday(date_time_hourly)-1)/(365.25),
        
        sin_year_r_2         = (sin(2 * pi * total_trend_yearly))/2,
        cos_year_r_2         = (cos(2 * pi * total_trend_yearly))/2,
        
        temperature_45_c15   = (temperature - 15)/45,
        humidity_100_c60     = (humidity - 60)/100,
        dew_point_45_c7      = (dew_point - 7)/45,
        daily_uv_index_10_c5 = (daily_uv_index - 5)/10,
        
        ts_yday = as_factor(yday(date_time_hourly))
        
    ) %>% 
    
    select(-c(wday_wend, isholiday))

contrasts(predictor_grid$hour)           <- "contr.Sum"
contrasts(predictor_grid$workday)        <- "contr.Sum"
contrasts(predictor_grid$precip_yn)      <- "contr.Sum"

#=========================================================================================#
# Computing predictions ----
#=========================================================================================#

model_fitted <- 
    predict(
        object = fit_info[[fit_list_elem]]$fit_obj, 
        newdata = predictor_grid,
        type = "response", 
        se.fit = FALSE
    )

predictions <- predictor_grid %>% mutate(fitted = model_fitted)

#-----------------------------------------------------------------------------------------#
# Saving
#-----------------------------------------------------------------------------------------#

write_rds(
    predictions, 
    path(
        output_dir, 
        str_c(fit_name, "_predictions_on_grid")
    ) %>% 
        path_ext_set("rds"),
    compress = "gz"
)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
