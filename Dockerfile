# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:latest

# system libraries of general use
RUN apt-get update && apt-get install -y && \
    apt-get install libcurl4-openssl-dev libv8-dev libxml2-dev tk -y &&\
    mkdir -p /var/lib/shiny-server/bookmarks/shiny

# install R packages required
# (change it dependeing on the packages you need)
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'ggplot2', 'fields', 'calibrate', 'sm', 'fda', 'devtools' ), repos='http://cran.rstudio.com/')"
RUN R -e "devtools::install_github('moviedo5/fda.usc')"

COPY r/functions.R /srv/shiny-server/
COPY r/app.R /srv/shiny-server/
#COPY R /srv/shiny-server/R
#COPY data /srv/shiny-server/data

# select port
EXPOSE 3838

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server && chmod -R 755 /srv/shiny-server

# run app
CMD ["/usr/bin/shiny-server"]
