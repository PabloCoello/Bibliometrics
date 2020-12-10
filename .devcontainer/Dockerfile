FROM ubuntu:latest

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y \
    git \
    cmake \
    software-properties-common \
    libpq-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    default-jre \
    default-jdk \
    locales \
    pandoc

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/'
RUN apt-get update && apt-get install -y r-base

RUN R CMD javareconf

RUN R -e "install.packages('readxl',dependencies=TRUE, repos='http://cran.rstudio.com/')"\
    && R -e "install.packages('dplyr',dependencies=TRUE, repos='http://cran.rstudio.com/')" \
    && R -e "install.packages('tidyr',dependencies=TRUE, repos='http://cran.rstudio.com/')" \
    && R -e "install.packages('digest',dependencies=TRUE, repos='http://cran.rstudio.com/')" \
    && R -e "install.packages('bibliometrix',dependencies=TRUE, repos='http://cran.rstudio.com/')" \
    && R -e "install.packages('xml2',dependencies=TRUE, repos='http://cran.rstudio.com/')" \
    && R -e "install.packages('quanteda',dependencies=TRUE, repos='http://cran.rstudio.com/')" \
    && R -e "install.packages('ggplot2',dependencies=TRUE, repos='http://cran.rstudio.com/')" \
    && R -e "install.packages('xml2', dependencies=TRUE, INSTALL_opts = c('--no-lock'))" \
    && R -e "install.packages('lubridate',dependencies=TRUE, repos='http://cran.rstudio.com/')" \
    && R -e "install.packages('sf',dependencies=TRUE, repos='http://cran.rstudio.com/')" \

WORKDIR /opt/bibliometrics/