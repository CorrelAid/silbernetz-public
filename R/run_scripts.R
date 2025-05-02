# a script to source all other scripts.
# all the important stuff happens locally in the other scripts:
# loading of  packages and data, configuring the functions,
# exporting the results, ...

##########################
#### 01 Preprocessing ####
##########################

#### merge all geographic info tables into a single geodata master table
source("R/01_preprocessing/create_geodata_mastertable.R")
# input:
# - data/geo/vorwahlen.csv
# - data/geo/bundesland.csv
# - data/geo/plz_einwohner.csv
# output:
# - data/geo/mastertable_vorw_bdl.csv

####  merge the geodata master table with a desired evn-export of calling data
source("R/01_preprocessing/add_geodata_to_exports.R")
# input:
# - data/raw/evn-export_01.06.2021_30.06.2021.csv (or any other export!)
# - data/geo/mastertable_vorw_bdl.csv
# output:
# - data/preprocessed/evn-export_01.06.2021_30.06.2021_with_geodata.csv

#### build wahlwiederholer statistik
source("R/01_preprocessing/wahlwdh_from_export_with_geodata.R")
# input:
# - data/preprocessed/evn-export_01.06.2021_30.06.2021_with_geodata.csv (or any other export with geodata!)
# output:
# - data/evn-export_01.06.2021_30.06.2021_wahlwdh_stat.csv

###########################
#### 02 Elkes Tabellen ####
###########################

#### creates a table that splits calls and callers by bundesland for each week
source("R/02_elkes_tabellen/create_herkunft_table.R")
# input: 
# - data/preprocessed/evn-export_01.06.2021_30.06.2021_with_geodata.csv (or any other export with geodata!)
# output:
# - results/herkunft_table_2021-06-01_2021-06-15.csv (or any other selected time period)

#### creates a table that <???>
source("R/02_elkes_tabellen/create_provider_table.R")
# input: 
# - data/preprocessed/evn-export_01.06.2021_30.06.2021_with_geodata.csv (or any other export with geodata!)
# output:
# - results/provider_table_2021-06-01_2021-06-30.csv (or any other selected time period)

#### creates a table that <???>
source("R/02_elkes_tabellen/create_monate_table.R")
# input: 
# - data/preprocessed/evn-export_01.06.2021_30.06.2021_with_geodata.csv (or any other export with geodata!)
# - data/raw/20210706_Gespr\u00E4chs_Statistik.xlsx (or any other gespraechsstatistik)
# output
# - data/raw/20210706_Gespr\u00E4chs_Statistik.xlsx (with new rows added)

##########################
#### 03 Visualization ####
##########################

#### plot map of germany with calls or callers in a given time by bundesland
source("R/03_visualisation/create_map_bundeslaender.R", encoding="utf-8")
# input
# - data/preprocessed/evn-export_01.06.2021_30.06.2021_with_geodata.csv (or any other export with geodata!)
# - data/geo/DEU_adm/DEU_adm1.shp
# - R/00_utils/colors.R
# - fonts in R/00_utils/font
# output
# - results/map_Anrufe_2021-06-01_2021-06-20.png (or any other selected time period or callers instead of calls)

#### plot <???>
source("R/03_visualisation/Vis_Elkes_Tables.R")
# input
# <...>
# output
# <...>
