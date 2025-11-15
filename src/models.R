library(rrpack)
library(tidyr)
library(tidymodels)
library(ggplot2)
library(mgcv)
library(nnet)
library(tseries)

make_lags = function(df) {
  df = df %>% group_by(dayofyear_ept, year_ept) %>% 
    arrange(hour24_ept, .by_group = T) %>% 
    mutate(last_hour_mw = lag(mw, n=1, default = mean(mw))) %>% ungroup()
  
  df
}

# Load metered data
#load("data/processed/metered_clean.RData")

# Load temperature data
#load("data/processed/open_meteo_historical_temp.RData")

# Load climate normals
#load("data/processed/weather_clean.RData")

# Linear model with PE only and standardized variables:
#metered_pe = metered_clean %>% filter(load_area == "PE")
#metered_pe = metered_pe %>% 
#  left_join(pivot_wider(id_cols = c(date_ept, hour24_ept),
#                        meteo_temp, 
#                        names_from = location_id, 
#                        values_from=temp_F,
#                        names_prefix = "tempF"), 
#            join_by(date_ept, hour24_ept))
#metered_pe = metered_pe %>% mutate(hour24_shifted = hour24_ept-12) %>% 
#                        mutate(across(starts_with("temp"), ~(.x-45)^2, .names="{.col}_sq"))


load_join_temp = function(load_data, temp_data, zone) {
  z = zone
  prepped = load_data %>% filter(load_area == z)
  prepped = prepped %>% 
    left_join(pivot_wider(id_cols = c(date_ept, hour24_ept),
                          temp_data, 
                          names_from = location_id, 
                          values_from=temp_F,
                          names_prefix = "tempF"), 
              join_by(date_ept, hour24_ept))
  prepped = prepped %>% mutate(hour24_shifted = hour24_ept-12) %>% 
    mutate(across(starts_with("temp"), ~(.x-45)^2, .names="{.col}_sq"))
  
  return(prepped)
}

standardize_features = function(prepped, summaries=NULL) {
  if(is.null(summaries)) {
    prepped = prepped %>%
      select(mw, date_ept, dayofyear_ept, hourofyear_ept, year_ept, starts_with("temp"), 
             month_num_ept,
             hour12_ept, hour24_ept, weekday_ept, is_peak_hour, 
             is_peak_day_in_week, is_thanksgiving, is_blackfriday, isweekend) %>% 
      #mutate(mw = mw-mean(mw)) %>% 
      #mutate(dayofyear_ept = dayofyear_ept/max(dayofyear_ept), 
      #       year_ept = year_ept/max(year_ept), 
      #       hour6_ept = (hour12_ept %% 6)/5, 
      #       hour12_ept = hour12_ept/max(hour12_ept),
      #       hour24_ept = hour24_ept/max(hour24_ept),
      #       weights = (dayofyear_ept+1)/mean(dayofyear_ept+1)) %>% 
      #mutate(mw = mw-mean(mw)) %>% 
      #group_by(year_ept) %>%
      #mutate(old_mw = mw-mean(mw)) %>% ungroup() %>% 
      #group_by(hourofyear_ept) %>% 
      #mutate(old_mw = mean(old_mw)) %>% ungroup() %>% 
      make_lags()
  } else {
    prepped = prepped %>%
      select(mw, date_ept, dayofyear_ept, hourofyear_ept, year_ept, starts_with("temp"), 
             month_num_ept,
             hour12_ept, hour24_ept, weekday_ept, is_peak_hour, 
             is_peak_day_in_week, is_thanksgiving, is_blackfriday, isweekend) %>% 
      #mutate(dayofyear_ept = dayofyear_ept/summaries[1], 
      #       year_ept = year_ept/summaries[2], 
      #       hour6_ept = (hour12_ept %% 6)/summaries[3], 
      #       hour12_ept = hour12_ept/summaries[4],
      #       hour24_ept = hour24_ept/summaries[5],
      #       weights = (dayofyear_ept+1)/summaries[6]) %>% 
      #group_by(year_ept) %>%
      #mutate(old_mw = mw-mean(mw)) %>% ungroup() %>% 
      #group_by(hourofyear_ept) %>% 
      #mutate(old_mw = mean(old_mw)) %>% ungroup() %>% 
      make_lags()
  }
  return(prepped)
}

mw_prepdata = function(load_data, temp_data, zone) {
  prepped = load_join_temp(load_data, temp_data, zone)
  
  #summary_vars = c(max(prepped$dayofyear_ept),
  #                 max(prepped$year_ept),
  #                 5, # max of hour6_ept computed later
  #                 max(prepped$hour12_ept),
  #                 max(prepped$hour24_ept),
  #                 mean(prepped$dayofyear_ept+1))
  
  summary_vars = mean(prepped$mw)
  
  prepped = standardize_features(prepped, summaries=NULL)
  
  return(list(prepped, summary_vars))
}

train_mw = function(load_data, temp_data, zone, subset=NULL) {
  prepped = mw_prepdata(load_data, temp_data, zone)
  prepped_data = prepped[[1]]
  summary_vars = prepped[[2]]
  gm = gam(mw ~ hourofyear_ept + dayofyear_ept + year_ept + weekday_ept
           + s(tempF1) + s(tempF2) + s(tempF3) + s(tempF4) + s(tempF5) + s(tempF6)
           + s(tempF7) + s(tempF8) + s(tempF9) + s(tempF10) + s(tempF11) + s(tempF12) + s(tempF0)
           + s(tempF1_sq) + s(tempF2_sq) + s(tempF3_sq) 
           + s(tempF4_sq) + s(tempF5_sq) + s(tempF6_sq)
           + s(tempF7_sq) + s(tempF8_sq) + s(tempF9_sq)
           + s(tempF10_sq) + s(tempF11_sq) + s(tempF12_sq) + s(tempF0_sq)
           + last_hour_mw
           + is_thanksgiving + is_blackfriday + isweekend, 
           family=gaussian(), data=prepped_data, weight=weights, subset=subset)
  
  return(list(gm, zone, summary_vars))
}

predict_mw = function(gm, oldload, oldtemp, newload, newtemp, zone, summary_vars) {
  old_data = load_join_temp(oldload, oldtemp, zone) %>% 
    group_by(year_ept) %>%
    mutate(past_mw = mw-median(mw)) %>% ungroup() %>% 
    group_by(hourofyear_ept) %>% 
    summarize(past_mw = median(past_mw)) %>% ungroup()
  #  select(date_ept, hour24_ept, old_mw = mw, month_num_ept, day_num_ept, hour24_utc)
  #old_data = old_data %>% group_by(month_num_ept, day_num_ept, hour24_ept) %>% 
  #  mutate(old_mw = mean(old_mw, na.rm=T)) %>% ungroup() %>% 
  #  mutate(date_ept = date_ept + years(1))
  #old_data = old_data %>% mutate(date_ept = date_ept + years(1))
  pred_data = load_join_temp(newload, newtemp, zone)
  #print(colnames(pred_data))
  #print(colnames(old_data))
  #pred_data = left_join(pred_data, old_data, by=join_by(hourofyear_ept))
  #pred_standard = standardize_features(pred_data, summaries=summary_vars)
  pred_standard = pred_data
  
  # Predict once on last year's mw
  preds = predict(gm, newdata=pred_standard)
  
  # Predict again using updated predictions
  #pred_data = pred_data %>% mutate(mw = preds)
  #pred_standard = standardize_features(pred_data, summaries=summary_vars)
  #preds = predict(gm, newdata=pred_standard)
  
  preds = data.frame(predicted_mw = preds, pred_data)
  #preds = left_join(preds, old_data, by=join_by(hourofyear_ept))
  #preds = preds %>% 
  #  mutate(predicted_mw = predicted_mw + past_mw)
  
  # Default to last year's mw if prediction is NA
  ii = which(is.na(preds$predicted_mw))
  preds$predicted_mw[ii] = preds$old_mw[ii]
  return(preds)
}

mw_ensemble = function(load_data, temp_data, zone, n_models=10, subset_length=24000, seed=NULL) {
  all_data = mw_prepdata(load_data, temp_data, zone)
  n = nrow(all_data[[1]])
  if(!is.null(seed))
    set.seed(seed)
  breaks = seq(from=1, to=(n-subset_length), length.out=n_models)
  mw_mods = list()
  for(i in 1:n_models) {
    sub = breaks[i]:(subset_length+breaks[i])
    newmod = train_mw(load_data, temp_data, zone, sub)
    mw_mods[[i]] = newmod
  }
  return(mw_mods)
}

# Does not seem to work as well
mw_ensemble2 = function(load_data, temp_data, zone, n_models=10, seed=NULL) {
  full_model = train_mw(load_data, temp_data, zone)
  fits = full_model[[1]]$fitted.values
  resids = full_model[[1]]$residuals
  n = length(resids)
  
  z = zone
  ii = which(load_data$load_area == z)
  if(!is.null(seed))
    set.seed(seed)
  mw_mods = list()
  for(i in 1:n_models) {
    newresid = sample(resids, n, replace=T)
    newload = load_data
    newload$mw[ii] = newresid+fits
    newmod = train_mw(newload, temp_data, zone)
    mw_mods[[i]] = newmod
  }
  return(mw_mods)
}

mw_ensemble_predict = function(mw_mods, newload, newtemp) {
  n_mod = length(mw_mods)
  preds = NULL
  for(i in 1:n_mod) {
    gm = mw_mods[[i]][1][[1]]
    zone = unlist(mw_mods[[i]][2])
    summary_vars = unlist(mw_mods[[i]][3])
    newpred = predict_mw(gm, newload, newtemp, zone, summary_vars)
    np = newpred[[1]]
    if(is.null(preds))
      preds = np
    else
      preds = cbind(preds, np)
  }
  final_preds = rowMeans(preds)
  return(final_preds)
}

#load_train = metered_clean %>% filter(date_ept < '2025-01-01')
#temp_train = meteo_temp %>% filter(date_ept < '2025-01-01')
#load_test = metered_clean %>% filter(date_ept >= '2025-01-01')
#temp_test = meteo_temp %>% filter(date_ept >= '2025-01-01')

#gm2 = train_mw(load_train, temp_train, zone="PE")

#single_mod_preds = predict_mw(gm2[[1]], load_train, temp_train, load_test, temp_test, zone=gm2[[2]], summary_vars=gm2[[3]])
#single_mod_mse = mean(unlist((single_mod_preds$predicted_mw-single_mod_preds$mw)^2), na.rm=T)

## Neural net peak hours

ph_standardize = function(prepped, summary_vars) {
  prepped = prepped %>% 
    mutate(mw = mw/summary_vars[1],
           day_sq = (dayofyear_ept-200)^2) %>%
    mutate(tempF0 = tempF0/summary_vars[2],
           tempF1 = tempF1/summary_vars[3],
           tempF2 = tempF2/summary_vars[4],
           tempF3 = tempF3/summary_vars[5],
           tempF4 = tempF4/summary_vars[6],
           tempF5 = tempF5/summary_vars[7],
           tempF6 = tempF6/summary_vars[8],
           tempF7 = tempF7/summary_vars[9],
           tempF8 = tempF8/summary_vars[10],
           tempF9 = tempF9/summary_vars[11],
           tempF10 = tempF10/summary_vars[12],
           tempF11 = tempF11/summary_vars[13],
           tempF12 = tempF12/summary_vars[14],
           tempF0_sq = tempF0/summary_vars[15],
           tempF1_sq = tempF1/summary_vars[16],
           tempF2_sq = tempF2/summary_vars[17],
           tempF3_sq = tempF3/summary_vars[18],
           tempF4_sq = tempF4/summary_vars[19],
           tempF5_sq = tempF5/summary_vars[20],
           tempF6_sq = tempF6/summary_vars[21],
           tempF7_sq = tempF7/summary_vars[22],
           tempF8_sq = tempF8/summary_vars[23],
           tempF9_sq = tempF9/summary_vars[24],
           tempF10_sq = tempF10/summary_vars[25],
           tempF11_sq = tempF11/summary_vars[26],
           tempF12_sq = tempF12/summary_vars[27],
           year_ept = year_ept/summary_vars[28],
           dayofyear_ept = dayofyear_ept/summary_vars[29],
           month_num_ept = month_num_ept/summary_vars[30],
           week_num_ept = week_num_ept/summary_vars[31],
           day_sq = day_sq/summary_vars[32]) %>% 
    group_by(date_ept, dayofyear_ept, year_ept, month_num_ept, day_num_ept,
             week_num_ept, weekday_ept, day_sq, peakhour, 
             is_thanksgiving, is_blackfriday, isweekend) %>% 
    summarize(max_mw = max(mw),
              med_mw = median(mw),
              min_mw = min(mw),
              mean_mw = mean(mw),
              tempF0_min = min(tempF0),
              tempF1_min = min(tempF1),
              tempF2_min = min(tempF2),
              tempF3_min = min(tempF3),
              tempF4_min = min(tempF4),
              tempF5_min = min(tempF5),
              tempF6_min = min(tempF6),
              tempF7_min = min(tempF7),
              tempF8_min = min(tempF8),
              tempF9_min = min(tempF9),
              tempF10_min = min(tempF10),
              tempF11_min = min(tempF11),
              tempF12_min = min(tempF12),
              tempF0_max = max(tempF0),
              tempF1_max = max(tempF1),
              tempF2_max = max(tempF2),
              tempF3_max = max(tempF3),
              tempF4_max = max(tempF4),
              tempF5_max = max(tempF5),
              tempF6_max = max(tempF6),
              tempF7_max = max(tempF7),
              tempF8_max = max(tempF8),
              tempF9_max = max(tempF9),
              tempF10_max = max(tempF10),
              tempF11_max = max(tempF11),
              tempF12_max = max(tempF12),
              tempF0_sq_max = max(tempF0_sq),
              tempF1_sq_max = max(tempF1_sq),
              tempF2_sq_max = max(tempF2_sq),
              tempF3_sq_max = max(tempF3_sq),
              tempF4_sq_max = max(tempF4_sq),
              tempF5_sq_max = max(tempF5_sq),
              tempF6_sq_max = max(tempF6_sq),
              tempF7_sq_max = max(tempF7_sq),
              tempF8_sq_max = max(tempF8_sq),
              tempF9_sq_max = max(tempF9_sq),
              tempF10_sq_max = max(tempF10_sq),
              tempF11_sq_max = max(tempF11_sq),
              tempF12_sq_max = max(tempF12_sq),
              tempF0_sq_min = min(tempF0_sq),
              tempF1_sq_min = min(tempF1_sq),
              tempF2_sq_min = min(tempF2_sq),
              tempF3_sq_min = min(tempF3_sq),
              tempF4_sq_min = min(tempF4_sq),
              tempF5_sq_min = min(tempF5_sq),
              tempF6_sq_min = min(tempF6_sq),
              tempF7_sq_min = min(tempF7_sq),
              tempF8_sq_min = min(tempF8_sq),
              tempF9_sq_min = min(tempF9_sq),
              tempF10_sq_min = min(tempF10_sq),
              tempF11_sq_min = min(tempF11_sq),
              tempF12_sq_min = min(tempF12_sq)) %>% ungroup()
  return(prepped)
}

ph_prepdata = function(load_data, temp_data, zone, preds=NULL) {
  prepped = load_join_temp(load_data, temp_data, zone)
  
  prepped = prepped %>% 
  group_by(date_ept) %>% 
    arrange(hour24_ept, .by_group = TRUE) %>% 
    mutate(peakhour = (ceiling(which.max(mw) / 2) - 1)*2+1) %>% ungroup() 
  
  if(!is.null(preds)) {
    prepped = prepped %>% mutate(mw = preds)
  }
  
  summaries = prepped %>% mutate(day_sq = (dayofyear_ept-200)^2) %>% 
    summarize(mw = max(mw),
              tempF0 = max(tempF0),
              tempF1 = max(tempF1),
              tempF2 = max(tempF2),
              tempF3 = max(tempF3),
              tempF4 = max(tempF4),
              tempF5 = max(tempF5),
              tempF6 = max(tempF6),
              tempF7 = max(tempF7),
              tempF8 = max(tempF8),
              tempF9 = max(tempF9),
              tempF10 = max(tempF10),
              tempF11 = max(tempF11),
              tempF12 = max(tempF12),
              tempF0_sq = max(tempF0),
              tempF1_sq = max(tempF1),
              tempF2_sq = max(tempF2),
              tempF3_sq = max(tempF3),
              tempF4_sq = max(tempF4),
              tempF5_sq = max(tempF5),
              tempF6_sq = max(tempF6),
              tempF7_sq = max(tempF7),
              tempF8_sq = max(tempF8),
              tempF9_sq = max(tempF9),
              tempF10_sq = max(tempF10),
              tempF11_sq = max(tempF11),
              tempF12_sq = max(tempF12),
              year_ept = mean(year_ept),
              dayofyear_ept = max(dayofyear_ept),
              month_num_ept = max(month_num_ept),
              week_num_ept = max(week_num_ept),
              day_sq = mean(day_sq)
    ) %>% unlist()
  
  prepped = ph_standardize(prepped, summary_vars=summaries)
  
  #prepped = prepped %>%
  #  arrange(date_ept) %>% 
  #  mutate(last_peakhour = lag(peakhour, n=1, default = 17))
  
  return(list(prepped, summaries))
}

train_ph = function(load_data, temp_data, zone, mw_preds=NULL) {
  prepped = ph_prepdata(load_data, temp_data, zone, preds=mw_preds)
  prepped_data = prepped[[1]]
  summary_vars = prepped[[2]]
  nn = nnet(factor(peakhour) ~ dayofyear_ept + weekday_ept + week_num_ept
            + year_ept + month_num_ept + day_num_ept + day_sq
            + tempF0_min + tempF1_min + tempF2_min + tempF3_min + tempF4_min + 
              + tempF5_min + tempF6_min + tempF7_min + tempF8_min + tempF9_min + 
              + tempF10_min + tempF11_min + tempF12_min 
            + tempF0_max + tempF1_max + tempF2_max + tempF3_max + tempF4_max + 
              + tempF5_max + tempF6_max + tempF7_max + tempF8_max + tempF9_max + 
              + tempF10_max + tempF11_max + tempF12_max
            + tempF1_sq_max + tempF2_sq_max + tempF3_sq_max 
            + tempF4_sq_max + tempF5_sq_max + tempF6_sq_max
            + tempF7_sq_max + tempF8_sq_max + tempF9_sq_max
            + tempF10_sq_max + tempF11_sq_max + tempF12_sq_max + tempF0_sq_max
            + tempF1_sq_min + tempF2_sq_min + tempF3_sq_min 
            + tempF4_sq_min + tempF5_sq_min + tempF6_sq_min
            + tempF7_sq_min + tempF8_sq_min + tempF9_sq_min
            + tempF10_sq_min + tempF11_sq_min + tempF12_sq_min + tempF0_sq_min
            + max_mw + med_mw + min_mw + mean_mw,
            data=prepped_data, weights=NULL, size=10, maxit=2000, decay=1.2)
  
  return(list(nn, zone, summary_vars))
}

predict_ph = function(nn, newload, newtemp, zone, summary_vars, mw_preds) {
  pred_data = load_join_temp(newload, newtemp, zone)
  #pred_data = pred_data %>% mutate(mw = mw_preds$predicted_mw,
  #                                 peakhour = 0)
  pred_data = pred_data %>% mutate(peakhour = 0)
  pred_standard = ph_standardize(pred_data, summary_vars)
  
  # Predict once on last year's mw
  nn_preds = predict(nn, newdata=pred_standard)
  
  #mw_3 = mw_preds %>% 
  #  mutate(mw = predicted_mw) %>% 
  #  group_by(date_ept) %>% 
  #  arrange(hour24_ept, .by_group=TRUE) %>% 
  #  summarize(mw0 = max(mw[c(0,1,2)]),
  #            mw8 = max(mw[c(7,8,9)]),
  #            mw11 = max(mw[c(10,11,12)]),
  #            mw14 = max(mw[c(13,14,15)]),
  #            mw17 = max(mw[c(16,17,18)]),
  #            mw20 = max(mw[c(19,20,21)])) %>% ungroup()
  
  #nn_mw_cols = apply(nn_preds*mw_3[,-1], 1, which.max)
  nn_mw_cols = apply(nn_preds, 1, which.max)
  nn_mw_classes = colnames(nn_preds)[nn_mw_cols] %>% as.numeric()
  
  return(data.frame(predicted_ph = nn_mw_classes, pred_standard))
}

#nn1 = train_ph(load_train, temp_train, zone="PE", gm2[[1]]$fitted.values)
#nn_preds = predict_ph(nn1[[1]], load_test, temp_test, 
#                      zone=nn1[[2]], summary_vars = nn1[[3]], mw_preds = single_mod_preds)

#true_peaks = metered_pe %>% filter(date_ept >= '2025-01-01') %>% 
#  group_by(date_ept) %>% arrange(hour24_ept, .by_group=TRUE) %>% 
#  summarize(peak_hour = which.max(mw)-1)

#nn_misclass = mean(abs(nn_preds$predicted_ph - true_peaks$peak_hour)>1)

## Neural net peak days
pd_standardize = function(prepped, summary_vars) {
  prepped = prepped %>% 
    mutate(pred_mw = pred_mw/summary_vars[1]) %>%
    mutate(tempF0 = tempF0/summary_vars[2],
           tempF1 = tempF1/summary_vars[3],
           tempF2 = tempF2/summary_vars[4],
           tempF3 = tempF3/summary_vars[5],
           tempF4 = tempF4/summary_vars[6],
           tempF5 = tempF5/summary_vars[7],
           tempF6 = tempF6/summary_vars[8],
           tempF7 = tempF7/summary_vars[9],
           tempF8 = tempF8/summary_vars[10],
           tempF9 = tempF9/summary_vars[11],
           tempF10 = tempF10/summary_vars[12],
           tempF11 = tempF11/summary_vars[13],
           tempF12 = tempF12/summary_vars[14],
           tempF0_sq = tempF0/summary_vars[15],
           tempF1_sq = tempF1/summary_vars[16],
           tempF2_sq = tempF2/summary_vars[17],
           tempF3_sq = tempF3/summary_vars[18],
           tempF4_sq = tempF4/summary_vars[19],
           tempF5_sq = tempF5/summary_vars[20],
           tempF6_sq = tempF6/summary_vars[21],
           tempF7_sq = tempF7/summary_vars[22],
           tempF8_sq = tempF8/summary_vars[23],
           tempF9_sq = tempF9/summary_vars[24],
           tempF10_sq = tempF10/summary_vars[25],
           tempF11_sq = tempF11/summary_vars[26],
           tempF12_sq = tempF12/summary_vars[27],
           year_ept = year_ept/summary_vars[28],
           dayofyear_ept = dayofyear_ept/summary_vars[29]) %>% 
    group_by(date_ept, dayofyear_ept, year_ept, week_num_ept, weekday_ept, is_peak_day_in_week, 
             is_thanksgiving, is_blackfriday, isweekend) %>% 
    summarize(max_mw = max(mw),
              peak_in_day = max(pred_mw),
              med_mw = median(pred_mw),
              min_mw = min(pred_mw),
              mean_mw = mean(pred_mw),
              tempF0_min = min(tempF0),
              tempF1_min = min(tempF1),
              tempF2_min = min(tempF2),
              tempF3_min = min(tempF3),
              tempF4_min = min(tempF4),
              tempF5_min = min(tempF5),
              tempF6_min = min(tempF6),
              tempF7_min = min(tempF7),
              tempF8_min = min(tempF8),
              tempF9_min = min(tempF9),
              tempF10_min = min(tempF10),
              tempF11_min = min(tempF11),
              tempF12_min = min(tempF12),
              tempF0_max = max(tempF0),
              tempF1_max = max(tempF1),
              tempF2_max = max(tempF2),
              tempF3_max = max(tempF3),
              tempF4_max = max(tempF4),
              tempF5_max = max(tempF5),
              tempF6_max = max(tempF6),
              tempF7_max = max(tempF7),
              tempF8_max = max(tempF8),
              tempF9_max = max(tempF9),
              tempF10_max = max(tempF10),
              tempF11_max = max(tempF11),
              tempF12_max = max(tempF12),
              tempF0_sq_max = max(tempF0_sq),
              tempF1_sq_max = max(tempF1_sq),
              tempF2_sq_max = max(tempF2_sq),
              tempF3_sq_max = max(tempF3_sq),
              tempF4_sq_max = max(tempF4_sq),
              tempF5_sq_max = max(tempF5_sq),
              tempF6_sq_max = max(tempF6_sq),
              tempF7_sq_max = max(tempF7_sq),
              tempF8_sq_max = max(tempF8_sq),
              tempF9_sq_max = max(tempF9_sq),
              tempF10_sq_max = max(tempF10_sq),
              tempF11_sq_max = max(tempF11_sq),
              tempF12_sq_max = max(tempF12_sq),
              tempF0_sq_min = min(tempF0_sq),
              tempF1_sq_min = min(tempF1_sq),
              tempF2_sq_min = min(tempF2_sq),
              tempF3_sq_min = min(tempF3_sq),
              tempF4_sq_min = min(tempF4_sq),
              tempF5_sq_min = min(tempF5_sq),
              tempF6_sq_min = min(tempF6_sq),
              tempF7_sq_min = min(tempF7_sq),
              tempF8_sq_min = min(tempF8_sq),
              tempF9_sq_min = min(tempF9_sq),
              tempF10_sq_min = min(tempF10_sq),
              tempF11_sq_min = min(tempF11_sq),
              tempF12_sq_min = min(tempF12_sq)) %>% ungroup()
  
  return(prepped)
}

pd_prepdata = function(load_data, temp_data, zone, preds=NULL) {
  prepped = load_join_temp(load_data, temp_data, zone)
  
  if(!is.null(preds)) {
    prepped = prepped %>%
      arrange(date_ept, hour24_ept) %>% 
      mutate(pred_mw = preds)
  }
  
  summaries = prepped %>%
    summarize(mw = max(pred_mw),
              tempF0 = max(tempF0),
              tempF1 = max(tempF1),
              tempF2 = max(tempF2),
              tempF3 = max(tempF3),
              tempF4 = max(tempF4),
              tempF5 = max(tempF5),
              tempF6 = max(tempF6),
              tempF7 = max(tempF7),
              tempF8 = max(tempF8),
              tempF9 = max(tempF9),
              tempF10 = max(tempF10),
              tempF11 = max(tempF11),
              tempF12 = max(tempF12),
              tempF0_sq = max(tempF0),
              tempF1_sq = max(tempF1),
              tempF2_sq = max(tempF2),
              tempF3_sq = max(tempF3),
              tempF4_sq = max(tempF4),
              tempF5_sq = max(tempF5),
              tempF6_sq = max(tempF6),
              tempF7_sq = max(tempF7),
              tempF8_sq = max(tempF8),
              tempF9_sq = max(tempF9),
              tempF10_sq = max(tempF10),
              tempF11_sq = max(tempF11),
              tempF12_sq = max(tempF12),
              year_ept = max(year_ept),
              dayofyear_ept = max(dayofyear_ept)
    ) %>% unlist()
  
  prepped = pd_standardize(prepped, summary_vars=summaries)
  
  return(list(prepped, summaries))
}

train_pd = function(load_data, temp_data, zone, mw_preds=NULL) {
  prepped = pd_prepdata(load_data, temp_data, zone, preds=mw_preds)
  prepped_data = prepped[[1]]
  summary_vars = prepped[[2]]
  nn = nnet(factor(is_peak_day_in_week) ~ dayofyear_ept + year_ept + weekday_ept
            + week_num_ept
            + tempF1 + tempF2 + tempF3 + tempF4 + tempF5 + tempF6
            + tempF7 + tempF8 + tempF9 + tempF10 + tempF11 + tempF12 + tempF0
            + tempF1_sq + tempF2_sq + tempF3_sq 
            + tempF4_sq + tempF5_sq + tempF6_sq
            + tempF7_sq + tempF8_sq + tempF9_sq
            + tempF10_sq + tempF11_sq + tempF12_sq + tempF0_sq
            + peak_in_day + med_mw + mean_mw + min_mw,
            data=prepped_data, weights=NULL, size=10, maxit=1000)
  
  return(list(nn, zone, summary_vars))
}

predict_pd = function(nn, newload, newtemp, zone, summary_vars, mw_preds) {
  pred_data = load_join_temp(newload, newtemp, zone)
  pred_data = pred_data %>% 
    mutate(mw = mw_preds$predicted_mw,
           pred_mw = mw_preds$predicted_mw)
  
  pred_standard = pd_standardize(pred_data, summary_vars=summary_vars)
  
  nn_preds = predict(nn, newdata=pred_standard)
  
  return(data.frame(predicted_pd = nn_preds, pred_standard))
}

#nn2 = train_pd(load_train, temp_train, zone="PE", gm2[[1]]$fitted.values)
#nn_preds = predict_pd(nn2[[1]], load_test, temp_test, 
#                      zone=nn2[[2]], summary_vars = nn2[[3]], mw_preds = single_mod_preds)

#nn_peaks = nn_preds$predicted_pd > 0.5
#nn_classdiff = nn_peaks - nn_preds$is_peak_day_in_week
#nn_misclass = nn_classdiff
#nn_misclass[which(nn_misclass==-1)] = 4
#mean(nn_misclass)
#plot(nn_misclass)

