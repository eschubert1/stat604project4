# Script for cleaning data
library(dplyr)
library(lubridate)

# Read in the data
read_metered = function(dir='data/raw/metered/') {
  metered_data = data.frame()
  for(file in list.files(dir)) {
    newdata = read.csv(file=file.path(dir, file))
    metered_data = rbind(metered_data, newdata)
  }
  return(metered_data)
}

# Split datetimes to date, time, am/pm
split_datetimes = function(datetime, suffix) {
  datecols = vapply(datetime, FUN=function(x) {unlist(strsplit(x, " "))}, 
                FUN.VALUE = c("a","b","c"))
  datecols = t(datecols)
  datecols = data.frame(datecols)
  colnames(datecols) = paste(c("date", "hour12", "ampm"), suffix, sep="_")
  rownames(datecols) = NULL
  datecols[,1] = mdy(datecols[,1])
  datecols[,2] = as.numeric(hms(datecols[,2]), units='hours')
  datecols
}

make24hr = function(hourcol, hourname, ampm) {
  newcolname = paste('hour24', substring(hourname, 7), sep="")
  newvalues = hourcol
  pms = which((ampm=="PM" & hourcol != 12) | (ampm=="AM" & hourcol == 12))
  newvalues[pms] = (hourcol[pms]+12) %% 24
  df24 = data.frame(newvalues)
  colnames(df24) = newcolname
  return(df24)
}

# Convert datetime columns to dates and times
process_dates = function(metered_df) {
  utcdates = split_datetimes(metered_df[, "datetime_beginning_utc"], "utc")
  eptdates = split_datetimes(metered_df[, "datetime_beginning_ept"], "ept")
  utc24 = make24hr(utcdates[,2], colnames(utcdates)[2], utcdates[,3])
  ept24 = make24hr(eptdates[,2], colnames(eptdates)[2], eptdates[,3])
  utcdates = cbind(utcdates, utc24)
  utcdates$day_num_utc = day(utcdates$date_utc)
  utcdates$month_num_utc = month(utcdates$date_utc)
  utcdates$year_utc = year(utcdates$date_utc)
  utcdates$week_num_utc = week(utcdates$date_utc)
  eptdates = cbind(eptdates, ept24)
  eptdates$day_num_ept = day(eptdates$date_ept)
  eptdates$month_num_ept = month(eptdates$date_ept)
  eptdates$year_ept = year(eptdates$date_ept)
  eptdates$week_num_ept = week(eptdates$date_ept)
  eptdates$weekday_ept = weekdays(eptdates$date_ept)
  metered_df = cbind(metered_df, utcdates, eptdates)
  metered_df = metered_df %>% mutate(isweekend = (weekday_ept %in% c("Saturday", "Sunday")))
  metered_df = metered_df %>% group_by(load_area, year_ept) %>% 
    arrange(month_num_ept, day_num_ept, .by_group = TRUE) %>% 
    mutate(hourofyear_ept = row_number()) %>% ungroup()
  metered_df = metered_df %>% mutate(dayofyear_ept = yday(date_ept))
  return(metered_df)
}

thanksgivings = c("2021-11-25", "2022-11-24", "2023-11-23", "2024-11-28", "2025-11-27")
black_fridays = c("2021-11-26", "2022-11-25", "2023-11-24", "2024-11-29", "2025-11-28")

process_holidays = function(df) {
  df = df %>% mutate(is_thanksgiving = date_ept %in% thanksgivings,
                     is_blackfriday = date_ept %in% black_fridays)
  df
}

# Remove summary RTO rows
remove_rto = function(metered_df) {
  rto_inds = which(metered_df[,"load_area"]=="RTO")
  metered_df[-rto_inds,]
}

# Find peak hours
find_peak_hours = function(metered_df) {
  peakloads = metered_df %>% group_by(date_ept, load_area) %>% 
    arrange(hour24_ept, .by_group = TRUE) %>% 
    summarize(n = n(), maxload = which.max(mw)) %>% ungroup()
  ii = cumsum(peakloads$n)-peakloads$n + peakloads$maxload
  metered_peak = metered_df %>% arrange(date_ept, load_area, hour24_ept, .by_group = TRUE)
  metered_peak$is_peak_hour = 0
  metered_peak$is_peak_hour[ii] = 1
  metered_peak = ungroup(metered_peak)
  return(metered_peak)
}

# Find peak days in a week
find_peak_daybyweek = function(metered_df) {
  # Filter data to peak hours only
  peakloads = metered_df %>% filter(is_peak_hour == 1)
  nonpeakloads = metered_df %>% filter(is_peak_hour == 0)
  
  # Define function to get 2nd largest load
  top2mw = function(x) {which(x == sort(x, decreasing=T)[2])[1]}
  
  # Determine peak days in week
  peakdays = peakloads %>% group_by(year_ept, load_area, week_num_ept) %>% 
    arrange(day_num_ept, .by_group = TRUE) %>%
    summarize(n = n(), maxload = which.max(mw)) %>% ungroup
  peakdays2 = peakloads %>% group_by(year_ept, load_area, week_num_ept) %>% 
    arrange(day_num_ept, .by_group = TRUE) %>% 
    summarize(n = n(), maxload = top2mw(mw)) %>% ungroup()
  
  ii = cumsum(peakdays$n)-peakdays$n + peakdays$maxload
  
  peakdays2 = peakdays2 %>% filter(!is.na(maxload))
  ii2 = cumsum(peakdays2$n)-peakdays2$n + peakdays2$maxload
  metered_peak = arrange(peakloads, year_ept, load_area, week_num_ept, day_num_ept)
  metered_peak$is_peak_day_in_week = 0
  metered_peak$is_peak_day_in_week[ii] = 1
  metered_peak$is_peak_day_in_week[ii2] = 1
  nonpeakloads$is_peak_day_in_week = 0
  metered_peak = rbind(metered_peak, nonpeakloads)
}

# Do all processing
process_data = function(metered_df) {
  metered_df = remove_rto(metered_df)
  metered_df = process_dates(metered_df)
  metered_df = find_peak_hours(metered_df)
  metered_df = find_peak_daybyweek(metered_df)
  metered_df = process_holidays(metered_df)
  metered_df
}

metered_data = read_metered(dir='data/raw/metered')
metered_clean = process_data(metered_data)
metered_clean = metered_clean %>% filter(date_ept >= '2021-01-01')
save(metered_clean, file='data/processed/metered_clean.RData')
