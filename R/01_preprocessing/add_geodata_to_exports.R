# script to add geodata to raw data exports

#####################
#### Data import ####
#####################
# load packages
library(tidyverse)
library(lubridate)
library(fuzzyjoin)
library(testthat)

# specify raw data export to be read
rawdata <- "evn-export_01.06.2021_30.06.2021.csv"

# import data
df <- df_initial <- read_csv(paste0("data/raw/", rawdata)) # raw data file
geo <- read_csv("data/geo/mastertable_vorw_bdl.csv")

####################
#### Processing ####
####################
# clean and add columns
df <- df %>% 
filter(Datum != "Gesamt") %>% # remove summary row
mutate(Datum = dmy(Datum), # replace character by date format
       Festnetz = str_detect(Anrufer, "00491", negate = TRUE)) # if call is land line

# merge all data sets
df <- df %>% 
  fuzzy_left_join(., geo, 
                  by = c("Anrufer" = "Ortsnetzkennzahl"), match_fun = str_detect)

# some data checks
test_that("no new rows have been added during merging",{
  expect_equal(nrow(df), nrow(df_initial)-1)})
test_that("each festnetz has a bundesland",{
  expect_equal(0, df %>% filter(is.na(Bundesland) & Festnetz) %>% nrow)})

################
#### Export ####
################
# export integrated data set
write_csv(df, paste0("data/preprocessed/", str_remove(rawdata, ".csv"), "_with_geodata.csv"))
