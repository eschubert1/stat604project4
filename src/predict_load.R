# File for computing predictions

suppressMessages(library(lubridate))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

# If pre-trained models throw error, switch to averages of historical data
tryCatch({
  suppressMessages(library(mgcv))
  suppressMessages(library(nnet))

# Load metered data
load("data/processed/metered_clean.RData")

# Load temperature data
load("data/processed/open_meteo_historical_temp.RData")
meteo_temp = forecast_temp

# Create/download forecast data sets
prediction_date = as.character(Sys.Date()+1)
thanksgivings = c("2021-11-25", "2022-11-24", "2023-11-23", "2024-11-28", "2025-11-27")
black_fridays = c("2021-11-26", "2022-11-25", "2023-11-24", "2024-11-29", "2025-11-28")
zones = unique(metered_clean$load_area)
pred_load_data = function(pred_date, zones, thanksgivings, black_fridays) {
  hours = c(0:23)
  df = expand.grid(hours, zones)
  colnames(df) = c("hour24_ept", "load_area")
  df$date_ept = as.Date(pred_date)
  df = df %>% mutate(mw = 0,
                     dayofyear_ept = yday(date_ept),
                     hourofyear_ept = 24*(dayofyear_ept-1)+hour24_ept,
                     year_ept = year(date_ept),
                     hour12_ept = hour24_ept%%12,
                     weekday_ept = weekdays(date_ept),
                     week_num_ept = week(date_ept),
                     month_num_ept = month(date_ept),
                     day_num_ept = day(date_ept),
                     is_peak_hour = 0,
                     is_peak_day_in_week = 0,
                     is_thanksgiving = date_ept %in% thanksgivings,
                     is_blackfriday = date_ept %in% black_fridays,
                     isweekend = weekday_ept %in% c("Saturday", "Sunday")
                     )
  df$hour12_ept[which(df$hour12_ept==0)] = 12
  return(df)
}

pred_load_df = pred_load_data(prediction_date, zones, thanksgivings, black_fridays)

# Get temperature forecast
suppressMessages(source("src/get_openmeteo.R"))
forecast_temp = get_meteo_forecast()
forecast_temp = forecast_temp %>% filter(date_ept == prediction_date)

# Make predictions
suppressMessages(source("src/models.R"))

zones = unique(metered_clean$load_area)
mw_preds = NULL
ph_preds = NULL
pd_preds = NULL
suppressMessages(
for(i in 1:length(zones)) {
  zone = zones[i]
  mw_path = paste("models/mw_",zone,".RData", sep="")
  ph_path = paste("models/ph_",zone,".RData", sep="")
  pd_path = paste("models/pd_",zone,".RData", sep="")
  load(file=mw_path)
  mwp = predict_mw(gm[[1]], metered_clean, meteo_temp, pred_load_df, forecast_temp, gm[[2]], gm[[3]])
  mw_preds = c(mw_preds, mwp$predicted_mw)
  load(file=ph_path)
  php = predict_ph(nn[[1]], pred_load_df, forecast_temp, nn[[2]], nn[[3]], mwp)
  ph_preds = c(ph_preds, php$predicted_ph)
  load(file=pd_path)
  pdp = predict_pd(nn[[1]], pred_load_df, forecast_temp, nn[[2]], nn[[3]], mwp)
  pd_preds = c(pd_preds, as.numeric(pdp$predicted_pd>0.2))
})

predictions = c(round(mw_preds), ph_preds, pd_preds)
cat(prediction_date, predictions, sep=",")
}, error = function(e) {
suppressMessages(load('data/processed/metered_clean.RData'))

default_model = function(df, predict_date) {
  # Make predictions by taking averages of loads for same day/month in different years
  predict_month = month(predict_date)
  predict_day = day(predict_date)
  
  predict_loads = df %>% filter(month_num_ept == predict_month, 
                                day_num_ept == predict_day) %>%
    group_by(load_area, hour24_ept) %>% 
    summarize(mw = mean(mw), 
              peak_hour = mean(is_peak_hour),
              peak_day = mean(is_peak_day_in_week)) %>% 
    arrange(load_area, hour24_ept)
  
  predict_peaks = predict_loads %>% select(load_area, peak_hour, peak_day) %>% 
    group_by(load_area) %>% summarize(peak_hour = (which.max(peak_hour)-1),
                                      peak_day = (mean(peak_day)>0.2)) %>% 
    arrange(load_area)
  
  
  predictions = c(round(predict_loads$mw), predict_peaks$peak_hour, predict_peaks$peak_day)
  predictions
}

predictions = suppressMessages(default_model(metered_clean, prediction_date))

cat(prediction_date, predictions, sep=",")
}
)
