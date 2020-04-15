# NYC Citi Bike Trips
Project exploring Citi Bike trips in New York City

Trip data can be retrieved from the Citi Bike [system data page](https://www.citibikenyc.com/system-data) (or from the [AWS data repository](https://s3.amazonaws.com/tripdata/index.html)).

## Downloading

I use R to download, clean, and store trip data, using this [`code`](https://github.com/cgettings/Citi-Bike-Trips/blob/master/code/trips_database.R).

## Visualizations

### Mapping trip counts with `leaflet`

This map displays the top-10 start stations for citi bike trips taken in April of 2019, and the end stations for each those trips. Each station's color indicates the relative* number of trips that ended at that station. The default layer displays each station in the system, along with its capacity.

*_Relative to every other end station, for trips starting at each of the top-10 start stations._

See map at https://cgettings.github.io/Citi-Bike-Trips/

([`code`](https://github.com/cgettings/Citi-Bike-Trips/blob/master/code/station_trips_leaflet.R))

### Tracking the most-used bike on 7/13/2017:

This is a graphic following a single bike around NYC for one day. 

This bike was used for 40 separate trips on Thursday, July 13th 2017. It was picked up in the East Village at 6:25 AM and dropped off for the final time on the Upper West Side at 11:40 PM. The highlighted area on the timeline spans a trip's start time and end time. The relative width of the arrow corresponds to the relative duration of that trip.

Most of the trips taken on this bike were between stations in midtown, with clusters during the AM and PM rush hours.

See slides at https://cgettings.github.io/Citi-Bike-Trips/

([`code`](https://github.com/cgettings/Citi-Bike-Trips/blob/master/code/one_bike_stops_slides.R))

***To-do:** Visualize projected routes for each trip using the [OSRM API](http://project-osrm.org/)*

## Modeling Citi Bike trips for April of 2019

Code for fitting a generalized linear mixed effects model to trip and weather data. from the raw data files.

***Coming soon***
