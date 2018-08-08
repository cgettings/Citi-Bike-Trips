################################################################-
################################################################-
##
## Building a database of NYC weather ----
##
################################################################-
################################################################-

#==============================================================#
# Setting up ----
#==============================================================#

#--------------------------------------------------------------#
# Loading libraries ----
#--------------------------------------------------------------#

library(plyr)
library(jsonlite)
library(tidyverse)
library(RSQLite)
library(lubridate)
library(DBI)
library(dbplyr)
library(magrittr)
library(glue)
library(gepaf)
library(V8)
library(httr)
library(countyweather)
library(rnoaa)
library(weathermetrics)
library(geosphere)

#--------------------------------------------------------------#
# Connecting to database ----
#--------------------------------------------------------------#

nyc_weather_db <- dbConnect(SQLite(), "./data/weather/nyc_weather_db.sqlite3")

#--------------------------------------------------------------#
# General parameters ----
#--------------------------------------------------------------#

base_url   <- "https://www.ncdc.noaa.gov/access-data-service/api/v1/data"
dataset    <- "global-hourly"

## Need a NOAA API token ##

noaa_token <- read_lines("./noaa_token.txt")

#==============================================================#
# Downloading ----
#==============================================================#

#--------------------------------------------------------------#
# Getting stations ----
#--------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Load tbl of NOAA ISD/ISH stations ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

stations_tbl <- isd_stations(refresh = FALSE)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Selecting New York City area stations ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Selecting Central Park station

knyc_station <- 
    stations_tbl %>% 
    filter(icao == "KNYC" & str_sub(end, 1, 4) == 2018 & str_sub(begin, 1, 6) <= 201301) %>% 
    select(lon, lat) %>% 
    as.matrix()


# Selecting recent weather stations

stations_tbl_2 <- 
    stations_tbl %>% 
    filter(str_sub(end, 1, 4) == 2018 & str_sub(begin, 1, 6) <= 201307) %>% 
    mutate(station_code = str_c(usaf, wban))


# Selecting NYC-area stations

nyc_stations <- 
    stations_tbl_2 %>% 
    mutate(distance = distGeo(
        p1 = knyc_station, 
        p2 = as.matrix(data.frame(lon, lat)))) %>% 
    arrange(distance) %>% 
    filter(distance <= 25000)


# Pulling station codes

station_codes <- 
    nyc_stations %>% 
    pull(station_code) %>% 
    str_c(collapse = ",")


#--------------------------------------------------------------#
# Getting data for selected dates ----
#--------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Specify the time and date range ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

###--- Start ---###

start_date <- 
    as_datetime(
        "2013/01/01 00:00:00", 
        tz = "US/Eastern") %>% 
    with_tz(tzone = "UTC")

start_year <- year(as_date(start_date))


###--- End ---###

end_year <- year(today(tz = "US/Eastern"))

end_date <- 
    as_datetime(
        glue("{end_year}/12/31 24:00:00"), 
        tz = "US/Eastern") %>% 
    with_tz(tzone = "UTC")


weather_years <- start_year:end_year


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Looping through requested years ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

for (i in 1:length(weather_years)) {
    
    
    ###--- Pretty printing status of loop ---###
    
    cat("Year:\n", weather_years[i], "\n", sep = "")
    
    
    start_date_i <- 
        as_datetime(glue("{weather_years[i]}/01/01 00:00:00"), tz = "US/Eastern") %>% 
        with_tz(tzone = "UTC") %>% 
        format("%FT%T")
    
    end_date_i <- 
        as_datetime(glue("{weather_years[i]}/12/31 24:00:00"), tz = "US/Eastern") %>%
        with_tz(tzone = "UTC") %>% 
        format("%FT%T")
    
    
    ###--- Query the API ---###
    
    nyc_weather <-
        GET(
            glue(
                "{base_url}",
                "?dataset={dataset}&",
                "startDate={start_date_i}&",
                "endDate={end_date_i}&",
                "stations={station_codes}&",
                "includeAttributes=true&includeStationName=true&includeStationLocation=true"
            ),
            timeout(100 * 60),
            add_headers(token = noaa_token)
        ) %>%
        extract2("content") %>%
        read_csv()
    
    
    ###--- Extract measurements and put into new variables ---###
    
    nyc_weather_clean <-
        nyc_weather %>%
        
        filter(str_detect(REPORT_TYPE, "FM-15")) %>%
        
        select(
            DATE,
            REPORT_TYPE,
            NAME,
            STATION,
            CALL_SIGN,
            LATITUDE,
            LONGITUDE,
            ELEVATION,
            TMP,
            DEW,
            SLP,
            WND,
            CIG,
            VIS,
            GD1,
            AA1,
            GM1
        ) %>% 
        
        mutate(
            temperature = (str_sub(TMP, start = 1L, end = 5L)  %>% as.numeric()) / 10,
            # Celsius
            
            dew_point   = (str_sub(DEW, start = 1L, end = 5L)  %>% as.numeric()) / 10,
            # Celsius
            
            pressure    = (str_sub(SLP, start = 1L, end = 5L)  %>% as.numeric()) / 10,
            # hectopascals
            
            wind_speed  = (str_sub(WND, start = 9L, end = 12L) %>% as.numeric()) / 10,
            # meters per second
            
            ceiling_height = str_sub(CIG, start = 1L, end = 5L) %>% as.numeric(),
            # meters
            
            visibility     = str_sub(VIS, start = 1L, end = 6L) %>% as.numeric(),
            # meters
            
            sky_cover      = str_sub(GD1, start = 1L, end = 1L) %>% as.numeric(),
            # categorical
            
            precip         = (str_sub(AA1, start = 4L, end = 7L) %>% as.numeric()),
            # millimeters x 10
            
            precip_yn      = case_when(precip == 0 ~ 0L, precip > 0 ~ 1L, TRUE ~ NA_integer_)
            
        ) %>%
        
        mutate(
            temperature    = na_if(temperature, 999.9),
            dew_point      = na_if(dew_point, 999.9),
            pressure       = na_if(pressure, 9999.9),
            wind_speed     = na_if(wind_speed, 999.9),
            ceiling_height = na_if(ceiling_height, 99999),
            visibility     = na_if(visibility, 999999),
            sky_cover      = na_if(sky_cover, 9),
            precip         = na_if(precip, 999.9),
            humidity       = dewpoint.to.humidity(dew_point, temperature, "celsius"),
            heat_index     = heat.index(
                t = temperature,
                dp = dew_point,
                temperature.metric = "celsius",
                output.metric = "celsius"
            )
        ) %>%
        
        select(
            date_time = DATE,
            report_type = REPORT_TYPE,
            name      = NAME,
            usaf_wban = STATION,
            callsign  = CALL_SIGN,
            lat       = LATITUDE,
            lon       = LONGITUDE,
            elevation = ELEVATION,
            temperature,
            dew_point,
            humidity,
            heat_index,
            pressure,
            wind_speed,
            ceiling_height,
            visibility,
            sky_cover,
            precip,
            precip_yn
            
        ) %>%
        
        mutate(
            date_time        = with_tz(date_time, tz = "US/Eastern"),
            date             = as_date(date_time),
            date_time_hourly = floor_date(date_time, "hours"),
            year             = year(date_time) %>% as.integer(),
            month            = month(date_time) %>% as.integer(),
            day              = day(date_time) %>% as.integer(),
            hour             = hour(date_time) %>% as.integer(),
            minute           = minute(date_time) %>% as.integer()
        )
    
    gc()
    
    
    ###--- writing to database ---###
    
    dbWriteTable(
        nyc_weather_db,
        "nyc_weather",
        value = nyc_weather_clean,
        append = TRUE,
        temporary = FALSE
    )
    
}

dbDisconnect(nyc_weather_db)


#--------------------------------------------------------------#
# Getting UVI data ----
#--------------------------------------------------------------#

nyc_weather_db <- dbConnect(SQLite(), "./data/weather/nyc_weather_db.sqlite3")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Every 3 minutes - Geneva, NY ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

## Loading column names for data cleaning

three_min_uv_names <- read_rds("./data/weather/three_min_uv_names.rds")


## Downloaded from https://uvb.nrel.colostate.edu/UVB/da_UvIndex.jsf

three_min_uv_data <- 
    read_csv(
        "./data/weather/Geneva_NY_UVI_2013-01-01.csv", 
        col_names = three_min_uv_names) %>% 
    select(-year_u:-second_u) %>% 
    rename_at(vars(year_l:second_l), .funs = function(x) str_remove(x, "_l")) %>% 
    mutate_at(vars(month:second), funs(as.integer)) %>% 
    mutate(
        date_time = force_tz(date_time, tzone = "US/Eastern"),
        date = as_date(date_time),
        date_time_hourly = floor_date(date_time, "hours")
    )


####---- writing to database ----####

dbWriteTable(
    nyc_weather_db,
    "three_min_uv_data",
    value = three_min_uv_data,
    append = TRUE,
    temporary = FALSE
)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Every day - Clark, NJ ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

daily_uv_data <- 
    read_lines(
        "http://www.temis.nl/uvradiation/archives/v2.0/overpass/uv_Clark_New_Jersey_USA.dat", 
        skip = 28) %>% 
    str_remove(string = ., pattern = "#") %>% 
    str_trim() %>% 
    str_replace_all(string = ., pattern = regex("\\h+"), replacement = ",") %>% 
    str_split(string = ., pattern = ",", simplify = TRUE) %>% 
    as_data_frame() %>% 
    set_colnames(value = .[1,]) %>% 
    filter(row_number() != 1) %>% 
    type_convert() %>% 
    filter(YYYYMMDD >= 20130101) %>% 
    mutate(YYYYMMDD = ymd(YYYYMMDD)) %>% 
    rename(
        date = YYYYMMDD,
        
        uv_index                  = UVIEF,
        uv_index_error            = UVIEFerr,
        
        uv_dose_eryth_clear       = UVDEF,
        uv_dose_eryth_clear_error = UVDEFerr,
        uv_dose_eryth_cloud       = UVDEC,
        uv_dose_eryth_cloud_error = UVDECerr,
        
        uv_dose_vitd_clear        = UVDVF,
        uv_dose_vitd_clear_error  = UVDVFerr,
        uv_dose_vitd_cloud        = UVDVC,
        uv_dose_vitd_cloud_error  = UVDVCerr,
        
        uv_dose_dna_clear         = UVDDF,
        uv_dose_dna_clear_error   = UVDDFerr,
        uv_dose_dna_cloud         = UVDDC,
        uv_dose_dna_cloud_error   = UVDDCerr,
        
        cloud_factor = CMF
    ) %>% 
    mutate(
        year  = year(date),
        month = month(date),
        day   = day(date)
        )


####---- writing to database ----####

dbWriteTable(
    nyc_weather_db,
    "daily_uv_data",
    value = daily_uv_data,
    append = TRUE,
    temporary = FALSE
)


#==============================================================#
# Creating indexes ----
#==============================================================#

nyc_weather_db <- dbConnect(SQLite(), "./data/weather/nyc_weather_db.sqlite3")

#--------------------------------------------------------------#
# Main weather data ----
#--------------------------------------------------------------#

db_create_index(nyc_weather_db, "nyc_weather", "usaf_wban")
db_create_index(nyc_weather_db, "nyc_weather", "name")
db_create_index(nyc_weather_db, "nyc_weather", "callsign")
db_create_index(nyc_weather_db, "nyc_weather", "report_type")

db_create_index(nyc_weather_db, "nyc_weather", "year")
db_create_index(nyc_weather_db, "nyc_weather", "month")
db_create_index(nyc_weather_db, "nyc_weather", "day")
db_create_index(nyc_weather_db, "nyc_weather", "hour")
db_create_index(nyc_weather_db, "nyc_weather", "minute")
db_create_index(nyc_weather_db, "nyc_weather", "date")
db_create_index(nyc_weather_db, "nyc_weather", "date_time_hourly")

db_create_index(nyc_weather_db, 
                "nyc_weather", 
                c("year", "month", "day"),
                name = "nyc_weather_ymd")

db_create_index(nyc_weather_db,
                "nyc_weather",
                c("year", "month", "day", "hour"),
                name = "nyc_weather_ymdh")

db_create_index(nyc_weather_db,
                "nyc_weather",
                c("year", "month", "day", "hour", "minute"),
                name = "nyc_weather_ymdhm")

db_create_index(nyc_weather_db, 
                "nyc_weather", 
                c("hour", "minute"), 
                name = "nyc_weather_hm")

#--------------------------------------------------------------#
# UVI data - Geneva, NY ----
#--------------------------------------------------------------#

db_create_index(nyc_weather_db, "three_min_uv", "year")
db_create_index(nyc_weather_db, "three_min_uv", "month")
db_create_index(nyc_weather_db, "three_min_uv", "day")
db_create_index(nyc_weather_db, "three_min_uv", "hour")
db_create_index(nyc_weather_db, "three_min_uv", "minute")
db_create_index(nyc_weather_db, "three_min_uv", "date")
db_create_index(nyc_weather_db, "three_min_uv", "date_time_hourly")

db_create_index(nyc_weather_db, 
                "three_min_uv", 
                c("year", "month", "day"),
                name = "three_min_uv_ymd")

db_create_index(nyc_weather_db,
                "three_min_uv",
                c("year", "month", "day", "hour"),
                name = "three_min_uv_ymdh")

db_create_index(nyc_weather_db,
                "three_min_uv",
                c("year", "month", "day", "hour", "minute"),
                name = "three_min_uv_ymdhm")

db_create_index(nyc_weather_db, 
                "three_min_uv", 
                c("hour", "minute"),
                name = "three_min_uv_hm")

#--------------------------------------------------------------#
# UVI data - Clark, NJ ----
#--------------------------------------------------------------#

db_create_index(nyc_weather_db, "daily_uv", "year")
db_create_index(nyc_weather_db, "daily_uv", "month")
db_create_index(nyc_weather_db, "daily_uv", "day")

db_create_index(nyc_weather_db, 
                "daily_uv", 
                c("year", "month", "day"),
                name = "daily_uv_ymd")

#--------------------------------------------------------------#
# Finishing up ----
#--------------------------------------------------------------#

dbGetQuery(nyc_weather_db, "SELECT * FROM sqlite_master WHERE type = 'index'")

dbExecute(nyc_weather_db, "VACUUM")

dbDisconnect(nyc_weather_db)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #             ---- THIS IS THE END ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
