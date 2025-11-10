
library(dplyr)
library(lubridate)

# Load temperature data
temp_norms = read.csv('data/raw/us-climate-normals_2006-2020_daily_temperature_by-variable_c20230403/dly-temp-normal.csv')

colnames(temp_norms)

# Convert GHCN_IDs to useful information
temp_norms = temp_norms %>% mutate(country_FIPS = substr(GHCN_ID, 1, 2),
                                   network_code = substr(GHCN_ID, 3, 3),
                                   station_id = substr(GHCN_ID, 4, 11))

# Filter to US states
temp_norms = temp_norms %>% filter(country_FIPS == 'US')

colnames(temp_norms) = tolower(colnames(temp_norms))

get_weather_stations = function() {
  ghcn_stations = read.csv(url("https://www.ncei.noaa.gov/oa/global-historical-climatology-network/hourly/doc/ghcnh-station-list.txt"))
  ghcn_stations = apply(ghcn_stations, 1, strsplit, split=" ")
  ghcn_stations = lapply(ghcn_stations, FUN=function(x){unlist(x)[unlist(x)!=""]})
  ghcn_stations = lapply(ghcn_stations, FUN=function(x){unlist(x)[1:5]})
  ghcn_stations = as.data.frame(t(data.frame(ghcn_stations)))
  colnames(ghcn_stations) = c("ghcn_id", "latitude", "longitude", "elevation", "state")
  rownames(ghcn_stations) = NULL
  ghcn_stations = ghcn_stations %>% filter(substr(ghcn_id,1,2)=='US')
  ghcn_stations = ghcn_stations %>% filter(nchar(state)<=2)
  ghcn_stations
}

ghcn_stations = get_weather_stations()

temp_norms = left_join(temp_norms, ghcn_stations, by=join_by(ghcn_id))

# Filter to data with state variables
temp_norms = temp_norms %>% filter(!is.na(state))

# Add day of year variable
temp_norms = temp_norms %>% group_by(ghcn_id) %>% arrange(month, day) %>% mutate(dayofyear = row_number())

# Save cleaned data
save(temp_norms, file="data/processed/weather_clean.RData")

# Load and clean open meteo temperature data
source("src/get_openmeteo.R")
meteo_temp = clean_meteo_historical("data/raw/openmeteo_historical.RData")
save(forecast_temp, file="data/processed/open_meteo_historical_temp.RData")

#openmeteo = read.csv("data/raw/open-meteo-35.40N86.77W244m.csv")

#stations = openmeteo[1:13,]
#temp_colnames = openmeteo[14,1:3]
#meteo_temp = openmeteo[15:nrow(openmeteo),1:3]
#colnames(meteo_temp) = temp_colnames
#
#meteo_temp$date_ept = date(meteo_temp$time)
#meteo_temp$hour24_ept = substr(meteo_temp$time, 12, nchar(meteo_temp$time)) %>% 
#  hm() %>% hour()
#
#meteo_temp = meteo_temp %>% rename(temp_F = `temperature_2m (°F)`)
#meteo_temp = meteo_temp %>% left_join(stations, by=join_by(location_id))
#meteo_temp = meteo_temp %>% mutate(temp_F = as.numeric(temp_F))
#save(meteo_temp, file="data/processed/open_meteo_historical_temp.RData")

