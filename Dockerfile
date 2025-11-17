FROM jupyter/r-notebook

COPY --chown=1000 README.md README.md
COPY --chown=1000 Makefile Makefile
COPY --chown=1000 data/processed/metered_clean.RData data/processed/metered_clean.RData
COPY --chown=1000 data/processed/open_meteo_historical_temp.RData data/processed/open_meteo_historical_temp.RData
COPY --chown=1000 openmeteo_stations.csv openmeteo_stations.csv
COPY --chown=1000 data/raw/metered/*.csv data/raw/metered/
COPY --chown=1000 data/raw/openmeteo_historical.RData data/raw/openmeteo_historical.RData
COPY --chown=1000 data/raw/canvas.zip data/raw/canvas.zip
COPY --chown=1000 data/raw/hrl_load_metered_2016-2025.tar.gz data/raw/hrl_load_metered_2016-2025.tar.gz
COPY --chown=1000 src/*.R src/
COPY --chown=1000 models/*.RData models/
COPY --chown=1000 train_models.Rmd train_models.Rmd
COPY --chown=1000 test_models.Rmd test_models.Rmd
COPY --chown=1000 cv_models.Rmd cv_models.Rmd
COPY --chown=1000 eda.Rmd eda.Rmd

CMD ["/bin/bash"]
