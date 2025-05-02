# script to create elke's table for "Monate"

# under construction # 

#####################
#### Data import ####
#####################
# load packages
library(tidyverse) # data wrangling
library(lubridate) # handle date and time
library(openxlsx) # export to excel

# specify raw data export with geodata to be read
rawdata_with_geodata <- "evn-export_01.06.2021_30.06.2021_with_geodata.csv"
excel_table <- "20210706_Gespr\u00E4chs_Statistik.xlsx"
# import data raw data with geo data added
df <- read_csv(paste0("data/preprocessed/", rawdata_with_geodata)) 
#df <- read_csv2(paste0("data/preprocessed/", rawdata_with_geodata)) #needs read_csv2 for Lara

# select time frame
period_start <- "2021-06-01"
period_stop <- "2021-06-15"

####################
#### Processing ####
####################
# create large table with anrufe duration that month

# Anrufe*	Kontakte** versch.	Wahlwdh gesamt	<60s*	>=60s	max.  s	Anrufer	Schnitt

df = subset(df, !is.na(df$Anrufer)) # remove last row 
index = which(df$Verbunden=="TRUE")

months_table <- df %>% 
 # group_by(Verbunden==TRUE) %>%
 #   filter(Verbunden == "TRUE") %>% 
  summarise(
    Anrufe = length(which(!is.na(Anrufer))),
    gesamt = length(which(Verbunden=="TRUE")),
    '<60s' = length(which(`Sekunden (Out)`[index]<60)),	
    '>=60s' = length(which(df$`Sekunden (Out)`[index]>=60)),
    'max.s'	= max(df$`Sekunden (Out)`[index]),
    versch.Anrufer = length(unique(Anrufer[index])), #  this is still wrong - it's neither n unique of Verbunden = TRUE nor of TRUE and FALSE combined Tine: Again, I don't think this is wrong
    Wahlw.Schnitt = Anrufe/versch.Anrufer,
    Erstanrufer = NA,
    #Erstanrufer = # ignore for now ignore for now as we dont have the data
    Vonovia = sum(Bezeichnung == "Vonovia Kooperation")
  )

################
#### Export ####
################
# export integrated data set - Not sure how to do this yet

# add month information
current_month <- as.character(month(period_start, label = TRUE, abbr = TRUE))
new_data <- bind_cols(current_month, months_table)

# read whole excel table
wb <- loadWorkbook(paste0("data/raw/", excel_table))
# get month table in order to determine the last row
df_excel <- read.xlsx(wb, sheet = "Monate", skipEmptyRows = FALSE)
new_row <- nrow(df_excel) + 3
new_col <- 2

# add the information to excel sheet
writeData(wb = wb, sheet = "Monate", x = new_data, 
          startCol = new_col, startRow = new_row, colNames = FALSE)
# for debugging: open excel file
openXL(wb)


# write_csv(months_table, 
#           paste0("results/months_table_", ".csv"))
