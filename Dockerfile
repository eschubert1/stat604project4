FROM jupyter/datascience-notebook

COPY --chown=1000 Makefile Makefile
COPY --chown=1000 src/predict_load.R src/predict_load.R

