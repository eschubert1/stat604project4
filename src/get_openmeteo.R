# Functions for downloading open meteo data
library(jsonlite)
library(dplyr)
library(lubridate)

get_meteo_historical = function(file="data/raw/openmeteo_historical.RData", json_url=NULL) {
  if(is.null(json_url))
    json_url = "https://historical-forecast-api.open-meteo.com/v1/forecast?latitude=35.39543,35.46573,38.06678,38.20738,38.34798,38.98067,39.33216,39.47276,40.03515,40.38664,41.51142,41.58172,46.64323&longitude=-86.77338,-82.61539,-84.67026,-78.09717,-88.90393,-74.95889,-81.43448,-76.18259,-82.44754,-79.85916,-71.32562,-85.84415,-88.5507&start_date=2021-01-01&end_date=2025-11-08&hourly=temperature_2m&temperature_unit=fahrenheit"

  options(timeout=600)
  meteo_historical = fromJSON(url(json_url))

  save(meteo_historical, file=file)
}

clean_meteo_historical = function(file) {
  load(file)
  
  forecast_stations = meteo_historical %>% select(location_id, latitude, longitude, timezone)
  forecast_stations[which(is.na(forecast_stations$location_id)),"location_id"] = 0
  
  forecast_units = data.frame(time_units = meteo_historical$hourly_units$time,
                              temp_units = meteo_historical$hourly_units$temperature_2m)
  
  forecast_temp = data.frame(datetime = meteo_historical$hourly$time,
                             temp = meteo_historical$hourly$temperature_2m)
  
  hours = nrow(forecast_temp)
  
  forecast_temp = forecast_temp %>% 
    pivot_longer(starts_with("datetime"), names_to = "date_name", values_to = "datetime") %>% 
    pivot_longer(starts_with("temp"), names_to = "temp_name", values_to = "temp_F") %>% 
    select(datetime, temp_name, temp_F) %>% 
    unique() %>% 
    select(datetime, temp_F) %>% 
    mutate(location_id = rep(c(0:12),hours),
           date_ept = date(datetime),
           hour24_ept = hour(hm(substr(datetime, 12, nchar(datetime))))) %>% 
    select(location_id, date_ept, hour24_ept, temp_F)
  
  return(forecast_temp)
}

get_meteo_forecast = function(json_url=NULL) {
  if(is.null(json_url))
    json_url = "https://api.open-meteo.com/v1/forecast?latitude=35.39543,35.46573,38.06678,38.20738,38.34798,38.98067,39.33216,39.47276,40.03515,40.38664,41.51142,41.58172,46.64323&longitude=-86.77338,-82.61539,-84.67026,-78.09717,-88.90393,-74.95889,-81.43448,-76.18259,-82.44754,-79.85916,-71.32562,-85.84415,-88.5507&hourly=temperature_2m&forecast_days=2&temperature_unit=fahrenheit"
  
  meteo_json = fromJSON(url(json_url))
  forecast_stations = meteo_json %>% select(location_id, latitude, longitude, timezone)
  forecast_stations[which(is.na(forecast_stations$location_id)),"location_id"] = 0
  
  forecast_units = data.frame(time_units = meteo_json$hourly_units$time,
                              temp_units = meteo_json$hourly_units$temperature_2m)
  
  forecast_temp = data.frame(datetime = meteo_json$hourly$time,
                             temp = meteo_json$hourly$temperature_2m)
  
  hours = nrow(forecast_temp)
  
  forecast_temp = forecast_temp %>% 
    pivot_longer(starts_with("datetime"), names_to = "date_name", values_to = "datetime") %>% 
    pivot_longer(starts_with("temp"), names_to = "temp_name", values_to = "temp_F") %>% 
    select(datetime, temp_name, temp_F) %>% 
    unique() %>% 
    select(datetime, temp_F) %>% 
    mutate(location_id = rep(c(0:12),hours),
           date_ept = date(datetime),
           hour24_ept = hour(hm(substr(datetime, 12, nchar(datetime))))) %>% 
    select(location_id, date_ept, hour24_ept, temp_F)
  
  return(forecast_temp)
}


