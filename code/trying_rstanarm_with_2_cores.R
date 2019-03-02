##################################################-
##################################################-
##
## Trying rstanarm with 2 cores ----
##
##################################################-
##################################################-

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
            algorithm = "NLOPT_LN_BOBYQA",
            # algorithm = "NLOPT_LN_COBYLA",
            print_level = 0,
            maxeval = 10000,
            xtol_rel = 1e-5,
            ftol_rel = 1e-5
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

fit_04_vars <- exprs(wday, hour, trips, trend24_r, start_station_id, temperature_r) %>% as.character()

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
tic("nlopt_cmp bobyqa")

trwx_fit_0421 <- 
    lmer(
        
        trips ~ 
            trend24_r * temperature_r + 
            (trend24_r * temperature_r | start_station_id) +
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
        chains = 2,
        iter = 300,
        cores = 2,
        verbose = TRUE
        # algorithm = "meanfield", 
        # QR = TRUE
    )

now(tzone = "US/Eastern")
toc()

write_rds(trwx_stan_fit_0421, "./output/trwx_stan_fit_0421.rds", compress = "gz")


################################################################


tidy(trwx_fit_0421) %>% print(n = Inf)
tidy(trwx_stan_fit_042, parameters = c("non-varying", "hier")) %>% print(n = Inf)


