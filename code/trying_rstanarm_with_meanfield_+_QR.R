#############################################################-
#############################################################-
##
## Trying rstanarm with meanfield + QR ----
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
library(rstanarm)
library(broom)

setCompilerOptions(optimize = 3)
enableJIT(level = 3)

#-----------------------------------------------------------#
# Loading functions ----
#-----------------------------------------------------------#

nlopt <- function(fn, par, lower, upper, control) {
    .nloptr <<- res <- nloptr(
        x0 = par,
        eval_f = fn,
        lb = lower,
        ub = upper,
        opts = list(
            # algorithm = "NLOPT_LN_NELDERMEAD",
            # algorithm = "NLOPT_LN_BOBYQA",
            algorithm = "NLOPT_LN_COBYLA",
            print_level = 0,
            maxeval = 10000,
            xtol_rel = 1e-9,
            ftol_rel = 1e-9
        )
    )
    return(
        list(
            par = res$solution,
            fval = res$objective,
            feval = res$iterations,
            conv = if (res$status > 0)
                0
            else
                res$status,
            message = res$message
        )
    )
}

nlopt_cmp <- cmpfun(nlopt)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# data ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

trwx_data_stations <- read_rds("./data/march_april_may/trips_station_weather_data.rds")

fit_04_vars <- exprs(wday, month, hour, trips, trend24_r, start_station_id, temperature_r) %>% as.character()

trwx_fit_04_data <-
    trwx_data_stations %>%
    # filter(month == 3) %>% 
    as_tibble() %>%
    select(fit_04_vars) %>%
    mutate(wday = as.integer(wday)) %>% 
    drop_na(fit_04_vars)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# nlopt bobyqa ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

now(tzone = "US/Eastern")
# tic("nlopt_cmp bobyqa")
# tic("nlopt_cmp nelder-mead")
tic("nlopt_cmp cobyla")

trwx_fit_0421 <- 
    lmer(
        
        trips ~ 
            trend24_r * temperature_r + 
            (trend24_r * temperature_r | start_station_id) +
            (trend24_r * temperature_r | month) +
            (trend24_r * temperature_r | wday/hour),
        
        REML = FALSE,
        data = trwx_fit_04_data,
        control = lmerControl(optimizer = "nlopt_cmp", calc.derivs = FALSE)
    )

now(tzone = "US/Eastern")
toc()

write_rds(trwx_fit_0421, "./output/trwx_fit_0421.rds", compress = "gz")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# rstanarm meanfield  ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

now(tzone = "US/Eastern")
tic("rstanarm testing")

trwx_stan_fit_0421 <- 
    stan_lmer(
        
        trips ~
            trend24_r * temperature_r +
            (trend24_r * temperature_r | start_station_id) +
            (trend24_r * temperature_r | wday / hour), 
        
        data = trwx_fit_04_data,
        # chains = 2, 
        # iter = 500, 
        # cores = 4, 
        # verbose = TRUE, 
        algorithm = "meanfield", 
        QR = TRUE
    )

now(tzone = "US/Eastern")
toc()

write_rds(trwx_stan_fit_0421, "./output/trwx_stan_fit_0421.rds", compress = "gz")


################################################################


tidy(trwx_fit_0421) %>% mutate_if(is.numeric, round, digits = 2) %>% View(title = "nelder-mead")
tidy(trwx_fit_0422) %>% mutate_if(is.numeric, round, digits = 2) %>% View(title = "bobyqa")
tidy(trwx_stan_fit_0421, parameters = c("non", "hier")) %>% mutate_if(is.numeric, round, digits = 2) %>% View(title = "stan")


# term                                                            estimate std.error group           
# <chr>                                                              <dbl>     <dbl> <chr>           
# 1 (Intercept)                                                  0.959          1.70 fixed           
# 2 trend24_r                                                    0.586          1.71 fixed           
# 3 temperature_r                                                4.77           1.72 fixed           
# 4 trend24_r:temperature_r                                     -2.10           1.72 fixed           
# 5 sd_(Intercept).start_station_id                              1.72          NA    start_station_id
# 6 sd_trend24_r.start_station_id                                4.21          NA    start_station_id
# 7 sd_temperature_r.start_station_id                            4.33          NA    start_station_id
# 8 sd_trend24_r:temperature_r.start_station_id                  4.46          NA    start_station_id
# 9 cor_(Intercept).trend24_r.start_station_id                  -0.0572        NA    start_station_id
# 10 cor_(Intercept).temperature_r.start_station_id              0.0704        NA    start_station_id
# 11 cor_(Intercept).trend24_r:temperature_r.start_station_id    0.0407        NA    start_station_id
# 12 cor_trend24_r.temperature_r.start_station_id               -0.0996        NA    start_station_id
# 13 cor_trend24_r.trend24_r:temperature_r.start_station_id     -0.0809        NA    start_station_id
# 14 cor_temperature_r.trend24_r:temperature_r.start_station_id  0.00923       NA    start_station_id
# 15 sd_(Intercept).hour:wday                                    4.22          NA    hour:wday       
# 16 sd_trend24_r.hour:wday                                      4.27          NA    hour:wday       
# 17 sd_temperature_r.hour:wday                                  4.85          NA    hour:wday       
# 18 sd_trend24_r:temperature_r.hour:wday                        4.69          NA    hour:wday       
# 19 cor_(Intercept).trend24_r.hour:wday                        -0.00218       NA    hour:wday       
# 20 cor_(Intercept).temperature_r.hour:wday                     0.00175       NA    hour:wday       
# 21 cor_(Intercept).trend24_r:temperature_r.hour:wday           0.00898       NA    hour:wday       
# 22 cor_trend24_r.temperature_r.hour:wday                       0.000378      NA    hour:wday       
# 23 cor_trend24_r.trend24_r:temperature_r.hour:wday            -0.0134        NA    hour:wday       
# 24 cor_temperature_r.trend24_r:temperature_r.hour:wday        -0.0772        NA    hour:wday       
# 25 sd_(Intercept).wday                                         4.41          NA    wday            
# 26 sd_trend24_r.wday                                           4.41          NA    wday            
# 27 sd_temperature_r.wday                                       4.42          NA    wday            
# 28 sd_trend24_r:temperature_r.wday                             4.42          NA    wday            
# 29 cor_(Intercept).trend24_r.wday                             -0.0000595     NA    wday            
# 30 cor_(Intercept).temperature_r.wday                         -0.000304      NA    wday            
# 31 cor_(Intercept).trend24_r:temperature_r.wday                0.000392      NA    wday            
# 32 cor_trend24_r.temperature_r.wday                           -0.0000968     NA    wday            
# 33 cor_trend24_r.trend24_r:temperature_r.wday                 -0.000205      NA    wday            
# 34 cor_temperature_r.trend24_r:temperature_r.wday             -0.00171       NA    wday            
# 35 sd_Observation.Residual                                     4.42          NA    Residual        


# term                                                          estimate std.error group           
# <chr>                                                            <dbl>     <dbl> <chr>           
# 1 (Intercept)                                                  1.76        0.166 NA              
# 2 trend24_r                                                   -1.96        0.409 NA              
# 3 temperature_r                                                3.99        0.586 NA              
# 4 trend24_r:temperature_r                                      4.78        0.906 NA              
# 5 sd_(Intercept).start_station_id                              0.778      NA     start_station_id
# 6 sd_trend24_r.start_station_id                                0.107      NA     start_station_id
# 7 sd_temperature_r.start_station_id                            4.13       NA     start_station_id
# 8 sd_trend24_r:temperature_r.start_station_id                  2.25       NA     start_station_id
# 9 cor_(Intercept).trend24_r.start_station_id                   0.952      NA     start_station_id
# 10 cor_(Intercept).temperature_r.start_station_id             -0.00696    NA     start_station_id
# 11 cor_(Intercept).trend24_r:temperature_r.start_station_id   -0.252      NA     start_station_id
# 12 cor_trend24_r.temperature_r.start_station_id               -0.00640    NA     start_station_id
# 13 cor_trend24_r.trend24_r:temperature_r.start_station_id     -0.247      NA     start_station_id
# 14 cor_temperature_r.trend24_r:temperature_r.start_station_id -0.815      NA     start_station_id
# 15 sd_(Intercept).hour:wday                                    1.05       NA     hour:wday       
# 16 sd_trend24_r.hour:wday                                      0.632      NA     hour:wday       
# 17 sd_temperature_r.hour:wday                                  0.625      NA     hour:wday       
# 18 sd_trend24_r:temperature_r.hour:wday                        1.16       NA     hour:wday       
# 19 cor_(Intercept).trend24_r.hour:wday                        -0.567      NA     hour:wday       
# 20 cor_(Intercept).temperature_r.hour:wday                     0.699      NA     hour:wday       
# 21 cor_(Intercept).trend24_r:temperature_r.hour:wday           0.00995    NA     hour:wday       
# 22 cor_trend24_r.temperature_r.hour:wday                      -0.972      NA     hour:wday       
# 23 cor_trend24_r.trend24_r:temperature_r.hour:wday            -0.122      NA     hour:wday       
# 24 cor_temperature_r.trend24_r:temperature_r.hour:wday         0.102      NA     hour:wday       
# 25 sd_(Intercept).wday                                         0.0385     NA     wday            
# 26 sd_trend24_r.wday                                           0.350      NA     wday            
# 27 sd_temperature_r.wday                                       0.309      NA     wday            
# 28 sd_trend24_r:temperature_r.wday                             0.0910     NA     wday            
# 29 cor_(Intercept).trend24_r.wday                             -0.334      NA     wday            
# 30 cor_(Intercept).temperature_r.wday                          0.135      NA     wday            
# 31 cor_(Intercept).trend24_r:temperature_r.wday               -0.396      NA     wday            
# 32 cor_trend24_r.temperature_r.wday                           -0.959      NA     wday            
# 33 cor_trend24_r.trend24_r:temperature_r.wday                 -0.485      NA     wday            
# 34 cor_temperature_r.trend24_r:temperature_r.wday              0.570      NA     wday            
# 35 sd_Observation.Residual                                     4.56       NA     Residual



