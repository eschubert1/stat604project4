This is a codebase for predicting power load on the PJM grid.
Data used for forecasting are metered data available from PJM's website
and forecast data available from https://open-meteo.com. Weather stations
used for temperature are recorded in the open_meteo_stations.csv file,
where 1 station was used for each state in the PJM grid.

In this project, the src directory contains scripts and functions for
redownloading the raw data, processing data, training models, and
predicting on future dates. EDA, training, cross validation, and testing
for the forecast models are available in the Rmd files. These Rmd
files can also be converted to Jupyter notebook format by running
```
make notebooks
```

Other commands which may be useful are:
```
make rawdata # Redownloads raw data
make processdata # Processes raw data
make predictions # Predicts load, peak hours and peak days for tomorrow
make all # Processes raw data, runs eda, model training, validation, and testing
``` 
