predictions:
	@Rscript src/predict_load.R

getraw:
	wget -O data/raw/canvas.zip https://files.osf.io/v1/resources/Py3u6/providers/osfstorage/?zip=
	unzip data/raw/canvas.zip -d data/raw/ | tar -xvzf data/raw/hrl_load_metered_2016-2025.tar.gz -C data/raw/metered 







