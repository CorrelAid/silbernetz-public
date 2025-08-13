Project name
================

## Basic Setup

This repository contains two main pieces of code:

- the `/silbRnetz` package
- the shiny app interface in `/shinyapp` and the setup follows this
  order:

0.  Install R and Positron or RStudio
1.  Download/clone the repository
2.  Install Font Open Sans


### Data
All the data is obtained from the API using the authentication provided
by the `api_spec.R`. It then gets processed to a level that preserves
the anonymitiy of all callers to an appropriate level and gets stored
locally under `/data/raw`.

#### Fake data
To get started without accessing the data via the API, you can use the fake data provided with the repository. 

1. rename `data/raw/current_data_fake.csv` to `data/raw/current_data.csv`.
2. start the app:

```
shiny::runApp("shinyapp/app.R")
```

You won't be able to use the "Zahlen aktualisieren" button.

#### Real data

1. obtain the `api_spec.R` file and place this file under `/R`. The
    valid keys can be obtained from Inopla and the final file should
    look like this:

<!-- -->

    url_base <- "url"
    api_urls <- list()
    api_urls[['Dest']] <-  paste(url_base, "hash1/Statistics/EVN/Destinations", sep = '')
    api_urls[['Dest_Count']] <- paste(url_base, "hash2/Statistics/EVN/Destinations/COUNT", sep = '')
    api_urls[['Numbers']] <- paste(url_base, "hash3/Statistics/EVN/Numbers", sep = '')
    api_urls[['Numbers_Count']] <- paste(url_base, "hash4/Statistics/EVN/Numbers/COUNT", sep = '')
    api_urls[['Callerlists']] <- paste(url_base, "hash5/Lists/Callerlists/1870/Items", sep = '')

2.  Setting up the data can be done in two ways: The first possibility
    is just to copy an existing file called `current_data.csv` from an
    old installation into `data/raw`. The second option is to call the
    `create_current_data` file from the package manually and give it the
    corresponding path where to write the `current_data.csv` file (should be `data/raw`).

3. start the app:


```
shiny::runApp("shinyapp/app.R")
```


### Package management
Packages are very outdated but a (hopefully working) state has been
collected in an renv environment.

To install:

```
install(renv)
renv::restore()
```

To add new packages

```
renv::install("packagename") # installs new package 
renv::snapshot() # adds new package to the renv.lock file  
```


For other potentially useful functionality (updating packages...) of `renv`, refer to the [renv documentation](https://rstudio.github.io/renv/reference/index.html).

#### Working with the silbRnetz package
The folder `silbRnetz` contains an internal R package. 

it will be installed when using `renv::restore()` from the local tarball `silbRnetz_0.0.12.tar.gz`.

Over the course of the project, you will potentiall write new code and/or refactor old code in the silbRnetz package. To make this available to the app, you need to load the changed functions into R. To do this, execute in the console whenever you changed something:

```
devtools::load_all("silbRnetz")
```

### Starting the app


To start the app, you can either use the RStudio interface in `shinyapp/app.R`
or in the console:

    shiny::runApp("shinyapp/app.R")


#### Deployment setup with bash file

1.  Write a bash file that allows to start the app without having to
    start RStudio looking something like this with the appropriate
    paths:

<!-- -->

    "C:/Program Files/R/R-3.6.3/bin/Rscript.exe" -e "shiny::runApp('C:/Users/USER/Desktop/cards', launch.browser = TRUE)"


### Known issues:

#### OneDrive on Windows-Machines

We encountered the problem that if the default user directory is
completely set up with OneDrive so that all files get synchronized, R
can run into problems when installing new packages and we weren’t able
to install the silbRnetz package. The best way to work around this, was
to make sure that R is installed in a directory not linked to OneDrive

#### Fonts on Mac

The map-drawing function with the corresponding fonts still causes
trouble on Mac.
