FROM openanalytics/r-base

MAINTAINER Garrett Dancik

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
#    libssh2-1-dev \
#    libssl1.0.0 \
    libxml2-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/ \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'dplyr', 'devtools'), repos='https://cloud.r-project.org/')" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# pmc2nc
RUN R -e "library(devtools); install_github('gdancik/pmc2nc')" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# copy the app to the image
RUN mkdir /root/pmc2nc
COPY app.R /root/pmc2nc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/pmc2nc', host = '0.0.0.0', port = 3838)"]


