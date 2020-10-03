FROM rocker/shiny

# Install Ubuntu packages
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libxml2-dev
    
# Install R packages that are required
# TODO: add further package if you need!
RUN R -e "install.packages(c('bsplus', 'colourpicker', 'corpus', 'dplyr', 'DT', 'haven', 'quanteda', 'RColorBrewer', 'readr', 'readxl', 'rvest', 'shiny', 'shinyalert', 'shinyjs', 'shinyWidgets', 'stringr', 'textreadr', 'textstem', 'tm', 'wordcloud2', 'writexl'), repos='http://cran.rstudio.com/')"

# Copy configuration files into the Docker image
COPY data /srv/shiny-server/data
COPY functions.R /srv/shiny-server/
COPY global.R /srv/shiny-server/
COPY server.R /srv/shiny-server/
COPY ui.R /srv/shiny-server/
COPY www /srv/shiny-server/www

# Make the ShinyApp available at port 3838
EXPOSE 3838

RUN sudo chown -R shiny:shiny /srv/shiny-server

CMD ["/usr/bin/shiny-server.sh"]