# script to create map with bundeslaender from export with geodata

#####################
#### Data import ####
#####################
# load packages
library(tidyverse)
library(lubridate)
library(patchwork)
library(rgdal)
library(maptools)
library(extrafont)
library(scales)
library(geosphere)
font_import(paths = "R/00_utils/font") # only run first time on machine
loadfonts(device = "win")

# define corporate colors
source("R/00_utils/colors.R")

# define corporate font
font <- "Open Sans"

# retrieve shapes for germany and bundeslaender (http://www.diva-gis.org/)
germany_lev1 <- readOGR("data/geo/DEU_adm/DEU_adm1.shp")

# select raw data export with geodata to be read
rawdata_with_geodata <- "evn-export_01.06.2021_30.06.2021_with_geodata.csv"

# import data raw data with geo data added
df <- read_csv(paste0("data/preprocessed/", rawdata_with_geodata))

# select time frame
period_start <- "2021-06-01"
period_stop <- "2021-06-20"

# select variable to be plotted
variable <- "Anrufer" # one of "Anrufe" or "Anrufer"

####################
#### Processing ####
####################
# turn imported shape file into dataframe
df_germany_lev1 <- fortify(germany_lev1 , region = "NAME_1") %>% 
  mutate(across(.cols=c(id, group), ~str_replace(.x, "Ã¼", "ue")))

# aggregate calls by bundesland
anrufe_by_bdl_in_period <- df %>% 
  #filter(Verbunden=="WAHR") %>% # should be filtered or not? Elke's choice
  mutate(Bundesland = ifelse(Festnetz == FALSE, "mobil", Bundesland)) %>% 
  filter(Datum >= period_start & Datum <= period_stop) %>% 
  group_by(Bundesland) %>% 
  summarise(Anrufer = length(unique(Anrufer)),
            Anrufe = n(), .groups = "drop")
anrufe_by_bdl_in_period
# merge spatial data and call data
df_germany_lev1 <- left_join(df_germany_lev1, 
                             anrufe_by_bdl_in_period %>% filter(Bundesland != "mobil"), 
                             by = c("id" = "Bundesland"))

# create new dataframe with coordinates for bundesland names
coord_names <- df_germany_lev1 %>% 
  group_by(id) %>% 
  summarise(centr_long = centroid(cbind(long,lat))[1],
            centr_lat  = centroid(cbind(long,lat))[2],
            !!as.symbol(variable) := first(!!as.symbol(variable))) %>% 
  mutate(centr_lat = case_when( # manual adjustments for berlin, brandenburg and bremen
    id == "Berlin" ~ centr_lat -0.2,
    id == "Brandenburg" ~ centr_lat + 0.45,
    TRUE ~ centr_lat
  )) 

##################
#### Plotting ####
##################
plot_map_bdl <- ggplot(df_germany_lev1) +
  geom_polygon(aes(x = long, y = lat, group = id, subgroup = group, fill = !!as.symbol(variable)),
               col = "black") + 
  theme_void(14) + 
  labs(title = paste0("Anzahl der ", variable, " im Zeitraum\n", 
                      period_start, " bis ", period_stop),
       fill = NULL) + 
  geom_text(data = coord_names,
            aes(x = centr_long, y = centr_lat, label = !!as.symbol(variable)),
            fontface = "bold", col = "black", family = font) + 
  scale_fill_gradient(low = corporate_rose,
                      high = corporate_red,
                      breaks = pretty_breaks(n = 4)) + 
  theme(text = element_text(family = font),
        plot.title = element_text(hjust = 1/2),
        legend.position = "bottom",
        plot.margin = margin(t = 1, r = 50, b = 1, l = 1),
        legend.text = element_text(size = 8))

plot_map_mobil <- ggplot() + 
  theme_void(14) + 
  geom_point(aes(x = 0, y = 0), size = 35, col = corporate_yellow) + 
  geom_text(aes(x = 0, 
                y = 0, 
                label = paste0(
                  "Mobilfunk:\n",
                  anrufe_by_bdl_in_period %>%
                    filter(Bundesland == "mobil") %>%
                    pull(!!as.symbol(variable)))),
            family = font,
            fontface = "bold")

plot_map <- plot_map_bdl + inset_element(plot_map_mobil,
                               left = 0.75, bottom = 0.7, right = 1, top = 0,
                               align_to = "full")

################
#### Export ####
################
png(paste0("results/map_", variable, "_", period_start, "_", period_stop, ".png"), 
    width = 3600, height = 5000, res = 650)
print(plot_map)
dev.off()
