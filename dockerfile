# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    libxml2-dev libxslt1-dev libcairo2-dev libsqlite3-dev \
    libpq-dev libssh2-1-dev unixodbc-dev libcurl4-openssl-dev libssl-dev \
    python3 python3-pip python3-venv python3-dev \
    build-essential pkg-config \
    # geospatial for sf/leaflet/s2
    libgdal-dev libgeos-dev libproj-dev libudunits2-dev gdal-bin proj-bin \
    cmake libabsl-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*
# copy necessary files
## app folder
COPY . /app

# set working directory
WORKDIR /app

# create Python virtual environment and install packages
RUN python3 -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"
RUN pip install --no-cache-dir -r requirements.txt

# set reticulate to use the virtual environment
ENV RETICULATE_PYTHON="/opt/venv/bin/python"

# install R packages
## 分步安装 R 包，按依赖顺序
RUN R -e "install.packages(c('units','s2'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('sf', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('leaflet', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('rnaturalearth', repos='https://cloud.r-project.org')"
 RUN R -e "install.packages('shinydashboard', repos='https://cloud.r-project.org')"
    RUN R -e "install.packages('rnaturalearthdata', repos='https://cloud.r-project.org')"
RUN R -e "install.packages(c('shiny','dplyr','tidyr','readr','shinyWidgets','ggplot2','DT','shinyjs','future','promises','zoo','stringr','purrr','data.table','reticulate'), repos='https://cloud.r-project.org/')"

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app/web.R', host = '0.0.0.0', port = 3838)"]