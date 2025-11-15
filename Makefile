# Required
all:
	echo "Not implemented yet"

# Required
predictions:
	@Rscript src/predict_load.R

getraw:
	mkdir data/raw/metered
	wget -O data/raw/canvas.zip https://files.osf.io/v1/resources/Py3u6/providers/osfstorage/?zip=
	unzip data/raw/canvas.zip -d data/raw/ | tar -xvzf data/raw/hrl_load_metered_2016-2025.tar.gz -C data/raw/metered 

train:
	Rscript -e "rmarkdown::render('train_models.Rmd')"

# Required
clean:
	rm -r data/raw/*
	rm -r data/processed/*

eda:
	echo "Not implemented yet"

# Required
rawdata: getraw
	@Rscript -e 'source("src/get_openmeteo.R"); get_meteo_historical()'

processdata:
	@Rscript src/process_raw_load.R
	@Rscript -e 'source("src/get_openmeteo.R"); clean_meteo_historical("data/raw/openmeteo_historical.RData")'


