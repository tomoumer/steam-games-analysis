FROM rocker/r-ver

# Install system dependencies required by R packages
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libglpk-dev  # Add this line

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'tidyverse', 'plotly', 'igraph', 'DT'))"

# Copy the app to the image
COPY ./shiny_app /srv/shiny-server/
COPY ./data /srv/shiny-server/data/
# Expose the port the app runs on
EXPOSE 3838

# Run the app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = 3838)"]
# comment the above CMD and uncomment the below to keep the container running so that you can inspect files etc in the container
# CMD ["tail", "-f", "/dev/null"]
