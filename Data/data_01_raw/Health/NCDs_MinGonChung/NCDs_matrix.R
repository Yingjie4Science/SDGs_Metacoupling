"
  This script aim to clean and format data for SDG indicator 3.4.1. 
  
  SDG 3.4.1 - Mortality rate attributed to cardiovascular disease, cancer, diabetes, or chronic respiratory disease
"


#   Setup ==========================================================================================

### To clear your environment 
remove(list = ls())

### library 
library(readr)
library(dplyr)
library(tidyr)
library(writexl)


### set work dir
path <- rstudioapi::getSourceEditorContext()$path
dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
setwd(dir)
getwd()
path_root <- dirname(dirname(getwd())) %>% dirname() %>% dirname(); path_root

dir.eora_cleaned <- paste0(path_root, '/Data/data_02_intermediate/dt02_flows/eora_cleaned/')




#  Data ===========================================================================================

## Helper data -----------------
f <- paste0(path_root, '/Data/_eora_190country_iso3.xlsx'); f
ctr_eora <- readxl::read_xlsx(path = f)

## Metadata - country list -----
ctr_list <- readxl::read_excel('./country_list/country_list_MCSDGs_fillna.xlsx')




## NCD death embodied in trade matrix (tm) ---------------------------------------------------------

# "The unit is 'persons' of death"

csv <- 'GHDx_RPM_death_change_matrix_121621.csv'
dt <- readr::read_csv(file = csv) 
names(dt)

df <- dt %>%
  dplyr::rename(year = Year) %>%
  dplyr::filter(year %in% c(2000, 2005, 2010, 2015, 2018)) %>% 
  dplyr::select(-`death.change.lower.exp`, -`death.change.upper.exp`) %>%
  tidyr::spread(key = `Importer.ISO3`, value = `death.change.exp`) %>%
  as.data.frame()


df %>%
  group_by(year) %>% tally() ## 148 countries are in the raw data


### Check the example by Min Gon Chung --> which is correct. 
df_check <- df %>%
  dplyr::rename(iso3 = Exporter.ISO3) %>%
  dplyr::filter(year %in% c(2015, 2018)) %>%
  gather(key = 'to', value = 'value', 3:ncol(.)) %>%
  dplyr::filter(iso3 %in% c('CAN'), to %in%c('CHN'))




### Format the data in the same way as Eora cleaned
f <- paste0(path_root, '/Data/data_02_intermediate/dt02_flows/', 'template_eora.RData'); f
load(f)

### To generate the same format as Eora in Matrix
df_formatted <- df %>%
  dplyr::rename(iso3 = Exporter.ISO3) %>%
  dplyr::filter(year %in% c(2000, 2005, 2010, 2015)) %>%
  tidyr::gather(key = 'to', value = 'value', 3:ncol(.)) %>%
  merge(x = temp_eora, y = ., by.x = c('year', 'iso3', 'to'), by.y = c('year', 'iso3', 'to'), all.x = T)

df_formatted <- df_formatted %>%
  ungroup() %>%
  arrange(year, iso3, to) %>%
  dplyr::select(year, ctr, everything()) %>%
  tidyr::spread(key = to, value = value) %>%
  arrange(year, iso3)



### save to the `eora_cleaned` folder
df_formatted %>% group_by(year) %>% tally()
ind <- "TradeMatrix_NCD_death_number"
fname <- paste0(dir.eora_cleaned, ind, '.xlsx'); fname
writexl::write_xlsx(x = df_formatted, path = fname)



### Set the path to the root dir of the Repo. 
setwd(dir = path_root)
getwd()
