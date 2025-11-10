library(dplyr)
library(ggplot2)


# Load data
load(file='data/processed/metered_clean.RData')
load(file='data/processed/weather_clean.RData')


metered_clean = metered_clean %>% 
  group_by(load_area, year_ept) %>% 
  arrange(month_num_ept, day_num_ept) %>% 
  mutate(hourofyear_ept = row_number())

metered_clean = metered_clean %>% mutate(dayofyear_ept = floor(hourofyear_ept/24)+1)


med_PE = metered_clean %>% filter(load_area == 'PE') %>% group_by(dayofyear_ept) %>% 
  summarize(med_mw = median(mw))

med_PE$dayofyear = med_PE$dayofyear_ept
pa_data = temp_norms %>% filter(state == 'PA')

# Plot of weather and megawatts
temp_norms %>% filter(state == 'PA') %>%
  ggplot(aes(x=dayofyear)) +
  geom_boxplot(aes(y=dly.tavg.normal, group=dayofyear)) +
  scale_y_continuous(name = 'Temperature (F)', 
                   sec.axis = sec_axis(~(.+50)*42, name='Megawatts')) +
  geom_line(data=med_PE, mapping=aes(y=med_mw/42-50), color='orange')

# Abs temp
temp_norms %>% filter(state == 'PA', between(dayofyear, 200, 220)) %>%
  ggplot(aes(x=dayofyear)) +
  geom_boxplot(aes(y=abs(dly.tavg.normal-45), group=dayofyear)) +
  scale_y_continuous(name = 'Temperature (F)', 
                     sec.axis = sec_axis(~(.+80)*42, name='Megawatts')) +
  geom_line(data=filter(med_PE, between(dayofyear, 200, 220)),
            mapping=aes(y=med_mw/42-80), color='orange')

# Square temp
temp_norms %>% filter(state == 'PA') %>%
  ggplot(aes(x=dayofyear)) +
  geom_boxplot(aes(y=(dly.tavg.normal-45)^2, group=dayofyear)) +
  scale_y_continuous(name = 'Temperature squared (F)', 
                     sec.axis = sec_axis(~(.+1800)*2, name='Megawatts')) +
  geom_line(data=med_PE, mapping=aes(y=med_mw/2-1800), color='orange')


# Boxplots of MW by weekday
metered_clean %>% mutate(weekday = weekdays(date_ept)) %>% 
  filter(load_area == "PE", month_num_ept==6) %>% 
  ggplot(aes(x = weekday, y = mw, group=weekday)) +
  geom_boxplot()

metered_clean %>% filter(load_area=='OVEC', year_ept==2023) %>% 
  ggplot(aes(x=hourofyear_ept, y=mw)) +
  geom_line()

# Plot of megawatts by region
med_load = metered_clean %>% group_by(load_area, dayofyear_ept) %>% 
  summarize(med_mw = median(mw)) %>% ungroup()

med_load = med_load %>% group_by(load_area) %>% 
  mutate(med_mw = med_mw/max(med_mw))

med_load %>% ggplot(aes(x = dayofyear_ept, y = med_mw)) +
  geom_line() +
  geom_line(data=filter(med_load, load_area=='OVEC'), color='pink')

# Plot of megawatts by hour
med_load = metered_clean %>% group_by(load_area, hour24_ept) %>% 
  summarize(med_mw = median(mw)) %>% ungroup()

med_load = med_load %>% group_by(load_area) %>% 
  mutate(med_mw = med_mw/max(med_mw)) %>% ungroup()

med_load = med_load %>% group_by(hour24_ept) %>% 
  mutate(med_mw = median(med_mw))

med_load %>% 
  ggplot(aes(x = hour24_ept, y = med_mw)) +
  geom_line() +
  labs(title='Median relative MW by Hour of Day over all days, zones')

# Plot of megawatts by hour of year and hour of day
med_load = metered_clean %>% group_by(hour24_ept, dayofyear_ept) %>% 
  summarize(med_mw = median(mw)) %>% ungroup()

med_load %>% ggplot(aes(x = dayofyear_ept, y = med_mw, group=hour24_ept)) +
  geom_line() +
  geom_line(data=filter(med_load, hour24_ept==0), color='pink') +
  geom_line(data=filter(med_load, hour24_ept==6), color='orange') +
  geom_line(data=filter(med_load, hour24_ept==12), color='green') +
  geom_line(data=filter(med_load, hour24_ept==18), color='lightblue')


med_load %>% filter(hour24_ept %in% c(4, 6, 12, 18)) %>% 
  ggplot(aes(x = dayofyear_ept, y = med_mw, color=factor(hour24_ept))) +
  geom_line()

