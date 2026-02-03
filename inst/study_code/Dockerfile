# OMOP Study Docker Image - RStudio Server
FROM rocker/rstudio:4.2.3

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev \
    unixodbc-dev \
    libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /home/rstudio/study

# Copy dependency files for package restoration
COPY renv.lock ./
COPY renv ./renv

# Install renv and restore packages from renv.lock ONLY
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org/')" \
    && if [ -f "renv.lock" ]; then \
         R -e "renv::restore()"; \
       fi \
    && rm -rf /tmp/*

# Copy study code (separate layer for better caching)
COPY . /home/rstudio/study

# Set proper permissions for RStudio user
RUN chown -R rstudio:rstudio /home/rstudio/study

# Expose RStudio Server port
EXPOSE 8787

# Start RStudio Server
CMD ["/init"]
