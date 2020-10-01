###########################################################################################-
###########################################################################################-
##
## Building a database of NYC weather ----
##
###########################################################################################-
###########################################################################################-

# Downloading weather data from key NYC stations, and saving in an SQLite database

#=========================================================================================#
# Setting up
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Loading libraries
#-----------------------------------------------------------------------------------------#

library(jsonlite)
library(tidyverse)
library(lubridate)
library(DBI)
library(dbplyr)
library(RSQLite)
library(fs)
library(here)
library(glue)
library(httr)
library(weathermetrics)
library(geosphere)
library(keyring)

#-----------------------------------------------------------------------------------------#
# Download parameters
#-----------------------------------------------------------------------------------------#

base_url <- "https://www.ncei.noaa.gov/access/services/data/v1"

#-----------------------------------------------------------------------------------------#
# Connecting to database
#-----------------------------------------------------------------------------------------#

nyc_weather_db <- dbConnect(SQLite(), "data/weather/nyc_weather_db.sqlite3")

#-----------------------------------------------------------------------------------------#
# Setting stations
#-----------------------------------------------------------------------------------------#

station_codes <- str_c("KNYC" = 72505394728, "KLGA" = 72503014732, sep = ",")

#-----------------------------------------------------------------------------------------#
# Getting date of most recent data
#-----------------------------------------------------------------------------------------#

database_has_nyc_weather <- nyc_weather_db %>% db_has_table("nyc_weather")

if (database_has_nyc_weather) {
    
    query <- 
        sql(
            glue(
                "SELECT `year`, `month`, `day`",
                "FROM `nyc_weather`",
                "ORDER BY `year` DESC, `month` DESC, `day` DESC",
                .sep = " "
            )
        )
    
    most_recent_day <- 
        nyc_weather_db %>% 
        db_collect(query, n = 1) %>% 
        mutate(date = str_c(year, month, day, sep = "-") %>% as_date()) %>% 
        pull(date)
    
} else {
    
    most_recent_day <- as_date("2013-01-01")
    
}

yesterday <- today() - days(1)


#=========================================================================================#
# Downloading ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Getting data for selected dates
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Query the API
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

nyc_weather <-
    GET(
        glue(
            "{base_url}",
            "?dataset=global-hourly&",
            "startDate={most_recent_day}&",
            "endDate={yesterday}&",
            "stations={station_codes}&",
            "includeAttributes=false&",
            "includeStationName=false&",
            "includeStationLocation=false&",
            "format=json"
        ),
        timeout(100 * 60),
        add_headers(token = key_get("noaa_token"))
    ) %>%
    pluck("content") %>%
    rawToChar() %>% 
    fromJSON() %>% 
    as_tibble()


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Extract measurements and put into new variables
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

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
        AA1
        # GM1
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
    
    # need to specify different missing values, because rescaling above
    
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
        date_time        = date_time %>% as_datetime() %>% with_tz("US/Eastern"),
        date             = as_date(date_time),
        date_time_hourly = floor_date(date_time, "hours"),
        year             = year(date_time) %>% as.integer(),
        month            = month(date_time) %>% as.integer(),
        day              = day(date_time) %>% as.integer(),
        hour             = hour(date_time) %>% as.integer(),
        minute           = minute(date_time) %>% as.integer()
    )

gc()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# writing to database
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

dbWriteTable(
    nyc_weather_db,
    "nyc_weather",
    value = nyc_weather_clean,
    append = TRUE,
    temporary = FALSE
)

dbDisconnect(nyc_weather_db)


#-----------------------------------------------------------------------------------------#
# Getting UVI data ----
#-----------------------------------------------------------------------------------------#

nyc_weather_db <- dbConnect(SQLite(), "data/weather/nyc_weather_db.sqlite3")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Getting date of most recent data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

database_has_daily_uv <- nyc_weather_db %>% db_has_table("daily_uv")

if (database_has_daily_uv) {
    
    query_uv <- 
        sql(
            glue(
                "SELECT `year`, `month`, `day`",
                "FROM `daily_uv`",
                "ORDER BY `year` DESC, `month` DESC, `day` DESC",
                .sep = " "
            )
        )
    
    most_recent_day_uv <- 
        nyc_weather_db %>% 
        db_collect(query_uv, n = 1) %>% 
        mutate(date = str_c(year, month, day, sep = "-") %>% as_date()) %>% 
        pull(date)
    
} else {
    
    most_recent_day_uv <- as_date("2013-01-01")
    
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Daily - Clark, NJ
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

daily_uv_data <- 
    read_lines(
        "http://www.temis.nl/uvradiation/archives/v2.0/overpass/uv_Clark_New_Jersey_USA.dat", 
        skip = 28
    ) %>% 
    str_remove(pattern = "#") %>% 
    str_trim() %>% 
    str_replace_all(pattern = regex("\\h+"), replacement = ",") %>% 
    str_split(pattern = ",", simplify = TRUE) %>% 
    as_tibble() %>% 
    set_names(nm = slice(., 1)) %>% 
    filter(row_number() != 1) %>% 
    type_convert() %>% 
    mutate(date = ymd(YYYYMMDD)) %>% 
    select(-YYYYMMDD) %>% 
    filter(date > most_recent_day_uv) %>% 
    rename(
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

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# writing to database
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

dbWriteTable(
    nyc_weather_db,
    "daily_uv",
    value = daily_uv_data,
    append = TRUE,
    temporary = FALSE
)

dbDisconnect(nyc_weather_db)


#=========================================================================================#
# Creating indexes ----
#=========================================================================================#

nyc_weather_db <- dbConnect(SQLite(), "data/weather/nyc_weather_db.sqlite3")

#-----------------------------------------------------------------------------------------#
# Main weather data
#-----------------------------------------------------------------------------------------#

index_tbl <- 
    dbGetQuery(
        nyc_weather_db, 
        "SELECT * FROM sqlite_master WHERE type = 'index'"
    ) %>% 
    as_tibble()

if (all(index_tbl$tbl_name != "nyc_weather")) {
    
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
    
    db_create_index(
        nyc_weather_db, 
        "nyc_weather", 
        c("year", "month", "day"),
        name = "nyc_weather_ymd"
    )
    
    db_create_index(
        nyc_weather_db,
        "nyc_weather",
        c("year", "month", "day", "hour"),
        name = "nyc_weather_ymdh"
    )
    
    db_create_index(
        nyc_weather_db,
        "nyc_weather",
        c("year", "month", "day", "hour", "minute"),
        name = "nyc_weather_ymdhm"
    )
    
    db_create_index(
        nyc_weather_db, 
        "nyc_weather", 
        c("hour", "minute"), 
        name = "nyc_weather_hm"
    )
    
}

#-----------------------------------------------------------------------------------------#
# UVI data - Clark, NJ
#-----------------------------------------------------------------------------------------#

if (all(index_tbl$tbl_name != "daily_uv")) {
    
    db_create_index(nyc_weather_db, "daily_uv", "year")
    db_create_index(nyc_weather_db, "daily_uv", "month")
    db_create_index(nyc_weather_db, "daily_uv", "day")
    
    db_create_index(
        nyc_weather_db, 
        "daily_uv", 
        c("year", "month", "day"),
        name = "daily_uv_ymd"
    )
    
}


#-----------------------------------------------------------------------------------------#
# Cleaning up
#-----------------------------------------------------------------------------------------#

if (all(index_tbl$tbl_name != "daily_uv") | all(index_tbl$tbl_name != "nyc_weather")) {
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # vacuuming
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    dbExecute(nyc_weather_db, "VACUUM")
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # checking indexes
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    dbGetQuery(
        nyc_weather_db, 
        "SELECT * FROM sqlite_master WHERE type = 'index'"
    ) %>% 
        as_tibble()
    
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# disconnecting
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

dbDisconnect(nyc_weather_db)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
