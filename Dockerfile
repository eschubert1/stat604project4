FROM jupyter/datascience-notebook:x86_64-r-4.3.1


COPY --chown=1000 Makefile Makefile
COPY --chown=1000 src/predict_load.R src/predict_load.R
COPY --chown=1000 src/get_openmeteo.R src/get_openmeteo.R
COPY --chown=1000 src/models.R src/models.R
COPY --chown=1000 src/process_raw_load.R src/process_raw_load.R
COPY --chown=1000 src/process_weather.R src/process_weather.R
COPY --chown=1000 data/processed/metered_clean.RData data/processed/metered_clean.RData
COPY --chown=1000 data/processed/open_meteo_historical_temp.RData data/processed/open_meteo_historical_temp.RData
COPY --chown=1000 data/processed/weather_clean.RData data/processed/weather_clean.RData

