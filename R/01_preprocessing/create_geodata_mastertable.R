# script to create master table with all geodata (vorwahlen, bundeslaender, ...)

#####################
#### Data import ####
#####################
# load packages
library(tidyverse)
# import data from bundesnetzagentur
vorw <- read_csv2("data/geo/vorwahlen.csv")
# import data from https://www.xn--datenbrse-57a.net/item/Postleitzahlen-Datenbank_Deutschland (accessed 2021-07-19)
bdl <- read_csv2("data/geo/bundesland.csv") %>% filter(!is.na(Vorwahl))
# import data from https://www.suche-postleitzahl.org/downloads (Volkszaehlung Zensus im Jahr 2011)
enw <- read_csv("data/geo/plz_einwohner.csv")

####################
#### Processing ####
####################
# align phone number format
vorw <- vorw %>% 
  mutate(Ortsnetzkennzahl = paste0("0049", Ortsnetzkennzahl)) %>% 
  select(-KennzeichenAktiv)
bdl <- bdl %>% 
  mutate(Vorwahl = str_replace(Vorwahl, "0", "0049")) %>% 
  mutate(Bundesland = ifelse(Bundesland == "Schlewig-Holstein",
                             "Schleswig-Holstein", Bundesland)) # correct bundesland name

# merge haan with solingen, as latter vorwahl (0212) is subset of former (02129),
# to avoid overlaps in merging
vorw <- vorw %>% 
  filter(!vorw == "00492129")

# only keep a single bundesland for each vorwahl 
# (determined by population size, and if, equal or both NA, randomly)
vorw_multiple_bdl <- bdl %>% # check if there are vorwahlen with > 1 bundesland
  group_by(Vorwahl) %>% 
  summarise(n_bdl_per_vorw = length(unique(Bundesland)), .groups = "drop") %>% 
  filter(n_bdl_per_vorw>1) %>% 
  pull(Vorwahl)
  
set.seed(1234)
vorw_selected_bdl <- left_join(bdl, enw, by = c("Plz" = "plz")) %>% 
  filter(Vorwahl %in% vorw_multiple_bdl) %>%
  mutate(einwohner = ifelse(is.na(einwohner), 0, einwohner)) %>% # replace NA by zero
  group_by(Vorwahl, Bundesland) %>% 
  summarise(n_einw = sum(einwohner), .groups = "drop_last") %>%
  ungroup %>% 
  group_by(Vorwahl) %>% 
  filter(n_einw == max(n_einw, na.rm = TRUE)) %>% # extract bundesland with larger population (or that without NA, if one has NA)
  sample_n(1) %>% # extract bundesland randomly where population is equal or both unknown
  ungroup %>% 
  mutate(Bundesland_selected = Bundesland) %>% 
  select(-(c("n_einw", "Bundesland")))

bdl <- bdl %>% 
  left_join(., vorw_selected_bdl, by = "Vorwahl") %>% 
  mutate(Bundesland = ifelse(!is.na(Bundesland_selected), Bundesland_selected,
                             Bundesland)) %>% 
  mutate(Bundesland = case_when(
    grepl("Baden-", Bundesland) ~ "Baden-Wuerttemberg",
    grepl("ringen", Bundesland) ~ "Thueringen",
    TRUE ~ Bundesland
  )) %>% 
  group_by(Vorwahl) %>% 
  summarise(Bundesland = first(Bundesland),
            PLZ = paste(Plz, collapse ="; ")) %>% 
  arrange(Vorwahl)

# merge and add remaining missing bundeslaender (data from http://wogibtes.info)
vorw_bdl <- left_join(vorw, bdl, by = c("Ortsnetzkennzahl" = "Vorwahl")) %>% 
  mutate(Bundesland = case_when(
    # 07, 08, 06
    is.na(Bundesland) & grepl("00497", Ortsnetzkennzahl) ~ "Baden-Wuerttemberg",
    is.na(Bundesland) & grepl("00498", Ortsnetzkennzahl) ~ "Bayern",
    is.na(Bundesland) & grepl("00499349", Ortsnetzkennzahl) ~ "Baden-Wuerttemberg",
    is.na(Bundesland) & grepl("00499377", Ortsnetzkennzahl) ~ "Baden-Wuerttemberg",
    is.na(Bundesland) & grepl("00499397", Ortsnetzkennzahl) ~ "Baden-Wuerttemberg",
    is.na(Bundesland) & grepl("00499", Ortsnetzkennzahl) ~ "Bayern",
    is.na(Bundesland) & grepl("004968", Ortsnetzkennzahl) ~ "Saarland",
    is.na(Bundesland) & grepl("0049625", Ortsnetzkennzahl) ~ "Hessen",
    is.na(Bundesland) & grepl("0049628", Ortsnetzkennzahl) ~ "Baden-Wuerttemberg",
    is.na(Bundesland) & grepl("00496", Ortsnetzkennzahl) ~ "Hessen",

    # 02
    is.na(Bundesland) & grepl("004926", Ortsnetzkennzahl) ~ "Rheinland-Pfalz",
    is.na(Bundesland) & grepl("0049277", Ortsnetzkennzahl) ~ "Hessen",
    is.na(Bundesland) & grepl("00492", Ortsnetzkennzahl) ~ "Nordrhein-Westfalen",
    
    # 03
    is.na(Bundesland) & grepl("004933", Ortsnetzkennzahl) ~ "Brandenburg",
    is.na(Bundesland) & grepl("004934299", Ortsnetzkennzahl) ~ "Sachsen",
    is.na(Bundesland) & grepl("0049356", Ortsnetzkennzahl) ~ "Brandenburg",
    is.na(Bundesland) & grepl("0049357", Ortsnetzkennzahl) ~ "Sachsen",
    is.na(Bundesland) & grepl("0049358", Ortsnetzkennzahl) ~ "Sachsen",
    is.na(Bundesland) & grepl("0049359", Ortsnetzkennzahl) ~ "Sachsen",
    is.na(Bundesland) & grepl("004936", Ortsnetzkennzahl) ~ "Thueringen",
    is.na(Bundesland) & grepl("004937", Ortsnetzkennzahl) ~ "Sachsen",
    is.na(Bundesland) & grepl("0049382", Ortsnetzkennzahl) ~ "Mecklenburg-Vorpommern",
    is.na(Bundesland) & grepl("004939", Ortsnetzkennzahl) ~ "Brandenburg",
    
    # 05
    is.na(Bundesland) & grepl("004952", Ortsnetzkennzahl) ~ "Nordrhein-Westfalen",
    is.na(Bundesland) & grepl("0049548", Ortsnetzkennzahl) ~ "Nordrhein-Westfalen",
    is.na(Bundesland) & grepl("0049564", Ortsnetzkennzahl) ~ "Nordrhein-Westfalen",
    is.na(Bundesland) & grepl("0049569", Ortsnetzkennzahl) ~ "Hessen",
    is.na(Bundesland) & grepl("0049597", Ortsnetzkennzahl) ~ "Nordrhein-Westfalen",
    is.na(Bundesland) & grepl("00495", Ortsnetzkennzahl) ~ "Niedersachsen",
    
    # 04
    is.na(Bundesland) & grepl("004945", Ortsnetzkennzahl) ~ "Schleswig-Holstein",
    is.na(Bundesland) & grepl("00494", Ortsnetzkennzahl) ~ "Niedersachsen",
    
    TRUE ~ Bundesland)
    )

################
#### Export ####
################
write_csv(vorw_bdl, "data/geo/mastertable_vorw_bdl.csv")
