# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# copy necessary files
COPY /app ./app

COPY shiny-customized.config /etc/shiny-server/shiny-server.conf

# install renv & restore packages
RUN Rscript -e 'install.packages("shiny")'
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'install.packages("shinyjs")'
RUN Rscript -e 'install.packages("readr")'
RUN Rscript -e 'install.packages("readxl")'
RUN Rscript -e 'install.packages("plotly")'
RUN Rscript -e 'install.packages("kableExtra")'
RUN Rscript -e 'install.packages("shinydashboard")'
RUN Rscript -e 'install.packages("flexdashboard")'
RUN Rscript -e 'install.packages("shinycssloaders")'
RUN Rscript -e 'install.packages("shinyWidgets")'
RUN Rscript -e 'install.packages("queueing")'

# install all packages in subfolders
# RUN grep library $1/*.R | cut -d \( -f 2 | cut -d \) -f 1 | while read line; do R -e "install.packages('$line', repos='https://cran.rstudio.com/')"; done

RUN Rscript -e 'renv::consent(provided = TRUE)'
RUN Rscript -e 'renv::restore()'

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]