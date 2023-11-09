FROM rocker/r-ver:4.1.0

# Install system dependencies
RUN apt-get update && apt-get install -y \ 
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libglpk-dev \
    libudunits2-dev \
    python3 \
    python3-pip

# Install Python dependencies
RUN pip3 install geopandas shapely pandas

# Install R packages
RUN R -e "install.packages(c('remotes', 'targets', 'magrittr', 'sf', 'future', 'future.callr', 'stplanr', 'dplyr', 'odjitter', 'cyclestreets', 'tibble', 'zonebuilder', 'stringr', 'tidyr', 'data.table', 'glue', 'zip', 'jsonlite', 'gert', 'collapse', 'pct', 'readr', 'future.batchtools', 'bs4Dash', 'DT', 'gt', 'pingr', 'shinybusy', 'shinyWidgets'), repos = 'https://cloud.r-project.org')"

# Install R package stplanr from its development version
RUN R -e "remotes::install_dev('stplanr')"

# Install Rust and Cargo (required to install odjitter)
RUN apt-get update && apt-get install -y cargo

# Install odjitter using cargo from a specific Git revision
RUN cargo install --git https://github.com/dabreegster/odjitter --rev 32fb58bf7f0d68afd3b76b88cf6b1272c5c66828


# Copy the R project files into the image
WORKDIR /usr/local/src/myscripts
COPY . /usr/local/src/myscripts

# Set the command to run your R script when the container starts
CMD ["Rscript", "_targets.R"]
