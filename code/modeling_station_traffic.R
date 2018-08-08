#############################################################-
#############################################################-
##
## A model for predicting station-level bike traffic ----
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

#-----------------------------------------------------------#
# Byte code compiler optimization levels ----
#-----------------------------------------------------------#

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
# custom optimizer ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

### defining options ----

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

### compiling function ----

nlopt_cmp <- cmpfun(nlopt)

#-----------------------------------------------------------#
# Loading data from RDS ----
#-----------------------------------------------------------#

## Data represent only a trip's start station (for now)

trwx_data_stations <-
    read_rds("./data/march_april_may/trips_station_weather_data.rds")

#-----------------------------------------------------------#
# Loading previous model fit (if applicable) ----
#-----------------------------------------------------------#

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


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #             ---- THIS IS THE END ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
