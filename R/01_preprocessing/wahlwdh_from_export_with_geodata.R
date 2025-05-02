# script to build wahlwiederholer-statistik from export with geodata

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

####################
#### Processing ####
####################
# aggregate data
export_summary <- df %>% 
  filter(Datum >= "2021-06-07" & Datum <= "2021-06-13") %>% # filter for dates of wahlwdh_stat sample
  group_by(Anrufer) %>% 
  summarise(Festnetz = first(Festnetz),
            Bundesland = first(Bundesland),
            Dauer = duration(sum(`Sekunden (In)`)),
            Anrufe = n(),
            Erfolgreich = sum(Verbunden=="TRUE"),
            Anteil_Erfolgreich = mean(Verbunden=="TRUE"),
            Durchschnitt_Dauer = duration(mean(`Sekunden (In)`))) %>% 
  mutate(across(.cols = c(Dauer, Durchschnitt_Dauer), function(x){
    x <- as.period(round(x,0))
    sprintf("%02d:%02d:%02d", hour(x), minute(x), second(x))})) %>% 
  arrange(desc(Anrufe))
  
################
#### Export ####
################
# export aggregated data set
write_csv(export_summary, paste0("data/preprocessed/", str_remove(rawdata_with_geodata, "_with_geodata.csv"), "_wahlwdh_stat.csv"))
