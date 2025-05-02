# script to create elke's table for "herkunft"

#####################
#### Data import ####
#####################
# load packages
library(tidyverse)
library(lubridate)

# specify raw data export with geodata to be read
rawdata_with_geodata <- "evn-export_01.06.2021_30.06.2021_with_geodata.csv"

# import data raw data with geo data added
df <- read_csv(paste0("data/preprocessed/", rawdata_with_geodata))

# select time frame
period_start <- "2021-06-01"
period_stop <- "2021-06-30"

####################
#### Processing ####
####################
# create large table with anrufe and anrufer weekly by bundesland
herkunft_table <- df %>% 
  filter(Datum >= period_start & Datum <= period_stop) %>% 
  mutate(Bundesland = case_when( # use abbreviations for better table
    Bundesland == "Baden-Wuerttemberg" ~ "BW",
    Bundesland == "Bayern" ~ "BY",
    Bundesland == "Berlin" ~ "BE",
    Bundesland == "Brandenburg" ~ "BB",
    Bundesland == "Bremen" ~ "BR",
    Bundesland == "Hamburg" ~ "HH",
    Bundesland == "Hessen" ~ "HE",
    Bundesland == "Mecklenburg-Vorpommern" ~ "MV",
    Bundesland == "Niedersachsen" ~ "NI",
    Bundesland == "Nordrhein-Westfalen" ~ "NW",
    Bundesland == "Rheinland-Pfalz" ~ "RP",
    Bundesland == "Saarland" ~ "SL",
    Bundesland == "Sachsen" ~ "SN",
    Bundesland == "Sachsen-Anhalt" ~ "ST",
    Bundesland == "Schleswig-Holstein" ~ "SH",
    Bundesland == "Thueringen" ~ "TH",
    Festnetz == FALSE ~ "mobil"
  )) %>% 
  #filter(Verbunden=="WAHR") %>% # should be filtered or not?
  mutate(Kalenderwoche = isoweek(Datum))%>% 
  group_by(Kalenderwoche) %>% 
  mutate(Kalenderwoche = paste0(Kalenderwoche, " (", min(Datum), " - ", max(Datum), ")")) %>% 
  ungroup %>% 
  group_by(Kalenderwoche, Bundesland) %>% 
  summarise(Anrufer = length(unique(Anrufer)),
            Anrufe = n(), .groups = "drop") %>% 
  pivot_wider(names_from = Bundesland, values_from = c(Anrufer, Anrufe),
              names_glue = "{Bundesland}_{.value}") %>% 
  mutate(Alle_Anrufer = rowSums(across(ends_with("_Anrufer")), na.rm = TRUE),
         Alle_Anrufe = rowSums(across(ends_with("_Anrufe")), na.rm = TRUE)) %>% 
  mutate(across(contains("Anrufe"), ~ifelse(is.na(.x), 0, .x)))

# order columns and create summary row
herkunft_table <- herkunft_table %>% 
  select(Kalenderwoche, 
         contains("Alle"),
         contains("mobil"),
         order(colnames(herkunft_table))) %>% 
  bind_rows(tibble(
    Kalenderwoche = "Gesamt",
    herkunft_table %>% summarise(across(where(is.numeric), sum))
  ))

################
#### Export ####
################
# export integrated data set
write_csv(herkunft_table, 
          paste0("results/herkunft_table_", period_start, "_", period_stop, ".csv"))
