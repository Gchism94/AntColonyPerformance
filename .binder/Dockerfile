FROM rocker/binder:latest
LABEL maintainer='Greg Chism'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}



RUN wget https://github.com/Gchism94/AntColonyPerformance/raw/main/DESCRIPTION && R -e "options(repos = list(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2022-07-09/')); devtools::install_deps()"

RUN rm DESCRIPTION.1; exit 0
