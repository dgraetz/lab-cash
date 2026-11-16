# Hugging Face Space for the Lab Cash app (bills version).
# The app code is not stored in the Space: it is downloaded from the public
# GitHub repo when the Space is built. To get a newer version of the app after
# pushing to GitHub, use "Factory rebuild" in the Space's settings.
# The private values (Google key, sheet ID, PIN, secret link key, ...) are set
# as Secrets in the Space's settings, never in these files.

FROM rocker/r-ver:4.4.1

# System libraries the R packages need, plus git to download the app
RUN apt-get update && apt-get install -y --no-install-recommends \
      git libcurl4-openssl-dev libssl-dev libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# R packages (rocker installs ready-made binaries, so this is quick)
RUN install2.r --error --skipinstalled shiny bslib googlesheets4 httr

# Hugging Face runs the app as user 1000 with its own home folder
RUN useradd -m -u 1000 user
USER user
ENV HOME=/home/user
WORKDIR /home/user

# Download the app from GitHub
RUN git clone --depth 1 https://github.com/dgraetz/lab-cash.git app
WORKDIR /home/user/app

EXPOSE 7860
CMD ["R", "--quiet", "-e", "shiny::runApp('app_bills.R', host = '0.0.0.0', port = 7860)"]
