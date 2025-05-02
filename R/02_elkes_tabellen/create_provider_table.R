# script to create elke's table for "Provider"

# under construction # 

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

# str_replace(string, pattern, replacement)
df$Datum = as.Date(df$Datum, tryFormats = c("%d.%m.%Y"), optional = FALSE)
df$Datum = as.POSIXct(df$Datum, tz = "Europe/Berlin", format="%Y-%m-%d")

# select time frame
period_start <- "2021-06-01"
period_stop <- "2021-06-30"

# create large table with anrufe duration that month

# Kontakte ab 60 s zählen als Gespräch 
# Anrufe	abgelehnt					Kontakte					versch.	Wahlwdh
# von	bis	ges.	gesamt	lose	Clear	failing	busy	gesamt	andere	<90s	>=90s	max.  s	Anrufe	Schnitt

index <- which(df$Verbunden=="TRUE")
index_60 <- which(df$`Sekunden (Out)`[index]<60)

####################
#### Processing ####
####################

df = subset(df, !is.na(df$Anrufer)) # remove last row 

provider_table <- df %>% 
  # group_by(Verbunden==TRUE) %>%
  filter(Datum >= period_start & Datum <= period_stop) %>% 
  mutate(Kalenderwoche = isoweek(Datum)) %>%
  group_by(Kalenderwoche) %>%
  mutate(Kalenderwoche = paste0(Kalenderwoche,
                                " (", min(Datum),
                                " - ", max(Datum), ")")) %>% 
  ungroup %>% 
  group_by(Kalenderwoche) %>% 
  summarise(
    Anrufe = length(which(!is.na(Anrufer))),
    gesamt = length(which(Verbunden == "TRUE")),
    '<60s' = length(which(`Sekunden (Out)` < 60&Verbunden == "TRUE")),
    '>=60s' = length(which(`Sekunden (Out)` >= 60&Verbunden == "TRUE")), 
    'max.s'	= max(`Sekunden (Out)`), 
    versch.Anrufer = length(unique(Anrufer)), #  this is still wrong - Tine: actually I don't think it is
    # Anrufer = length(unique(Anrufer[which(Verbunden=="TRUE")])) #  this is still wrong 
    Wahlw.Schnitt = Anrufe/versch.Anrufer
    #Erstanrufer = # ignore for now ignore for now as we dont have the data
  )

provider_table = map_df(provider_table, rev)

################
#### Export ####
################
# export integrated data set - Not sure how to do this yet
write_csv(provider_table, 
          paste0("results/provider_table_", period_start, "_", period_stop, ".csv"))
