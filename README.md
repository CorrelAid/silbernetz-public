Silbernetz Dashboard
================

[Shiny](https://shiny.posit.co/) Dashboard for Silbernetz e.V. 
Initially developed as part of a [CorrelAid data4good volunteering project](https://correlaid.org/daten-nutzen/projektdatenbank/2021-07-SIL/) between 2021 and 2022. Revisited in autumn 2025 to update the app. 

### App
- `app.R`
- functions in `R` are automatically loaded at startup

### Data
#### Call data
- call data is stored in `data/raw/annual/`. Each year has its own csv file. 
- Fake data is provided in `data/raw/annual_fake/`.
- Real data is not part of this repository due to data protection concerns. 
- Real data goes back to `2020-09-05`.

#### Geo data
- `data/geo/DEU_admin`: shapefile for Bundesländer
- `mapping_bula.csv`: mapping containing additional information for each Bundesland
- `mastertable_vorw_bdl.csv`: maps Ortskennzahl ("Vorwahl") to Bundesland.

### additional files
- `qmd/create_map.qmd`: create map without starting app
- `qmd/diagnostics.qmd`: some data quality analyses (e.g. duplicates, data coverage)

# Setup
## General setup
1. install R if not already installed on your system
2. Install [Font Open Sans](https://fonts.google.com/specimen/Open+Sans) on your operating system. You can find the `.tff` files in `assets/font`. Google "install tff on [windows|mac|linux]" to find out how to install a font on your operating system.

## Setup with fake data

To get started without accessing the data via the API, you can use the fake data provided with the repository in the folder `data/raw/annual_fake/`


1. Clone or download this repository and place it into a suitable location on your computer.
2. install dependencies

```r
install.packages("renv")
renv::restore()
```
3. Copy the `.env-template` to `.env` and set the `DATA_FOLDER` variable as follows:

```
DATA_FOLDER="data/raw/annual_fake/"
```

4. Start the app

```
shiny::runApp("app.R")
```

:warning: _In order to work with real data and/or work on the "Zahlen aktualisieren" (Update) functionality, you need authentication details for the hotline provider API. Please see instructions on "Setup with real data"._

## Setup with real data
1. Clone or download this repository and place it into a suitable location on your computer.
2. Obtain a historical dump of the real data and put the csv files into `data/raw/annual/`
3. install dependencies

```r
install.packages("renv")
renv::restore()
```
4. Create `.env` file: Copy `.env-template` to `.env` and fill in values under "Authentication for API". Values can be obtained from your contact at CorrelAid.
5. Start the app:

```
shiny::runApp("app.R")
```

# Package management 
Package management is done with [renv](https://rstudio.github.io/renv/articles/renv.html).

To install the environment:

```
install(renv)
renv::restore()
```

To add new packages:

```
renv::install("packagename") # installs new package 
renv::snapshot() # adds new package to the renv.lock file  
```


For other potentially useful functionality (updating packages...) of `renv`, refer to the [renv documentation](https://rstudio.github.io/renv/reference/index.html).

# Deployment 

## With bash file

1.  Write a bash file that allows to start the app without having to
    start RStudio looking something like this with the appropriate
    paths:

<!-- -->

    "C:/Program Files/R/R-3.6.3/bin/Rscript.exe" -e "shiny::runApp('C:/Users/USER/Desktop/cards', launch.browser = TRUE)"


# Known issues / Limitations

#### OneDrive on Windows-Machines

We encountered the problem that if the default user directory is
completely set up with OneDrive so that all files get synchronized, R
can run into problems when installing new packages and we weren’t able
to install the silbRnetz package. The best way to work around this, was
to make sure that R is installed in a directory not linked to OneDrive
