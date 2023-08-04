"
  This script aim to clean and format data for SDG indicator 3.4.1. 
  
  SDG 3.4.1 - Mortality rate attributed to cardiovascular disease, cancer, diabetes, or chronic respiratory disease
"


#   Setup ==========================================================================================

### To clear your environment 
# remove(list = ls())


### set work dir
# path <- rstudioapi::getSourceEditorContext()$path
# dir.here  <- dirname(rstudioapi::getSourceEditorContext()$path); dir.here
# dir.NCD.root <- dirname(dirname(dir.here)) %>% dirname(); dir.NCD.root


library(here)
dir.NCD.root <- here::here('NCDs.R') %>% dirname()
dir.NCD.root

getwd()
root <- dir.NCD.root %>% dirname() %>% dirname()%>% dirname()%>% dirname(); root
source(file = paste0(root, '/Code/_path of data.R'))



### library 
library(readr)
library(dplyr)
library(writexl)




#  Data ===========================================================================================

## Helper data -----------------
f <- paste0(root, '/Data/_eora_190country_iso3.xlsx'); f
ctr_eora <- readxl::read_xlsx(path = f)

## Metadata - country list -----
ctr_list <- readxl::read_excel(path = paste0(dir.NCD.root, '/country_list/country_list_MCSDGs_fillna.xlsx'))




## Total NCD death ---------------------------------------------------------------------------------
# "The unit is 'persons' of death"

csv <- 'GHDx_meat_risk_1990_2019.csv'

dt <- readr::read_csv(file = paste0(dir.NCD.root, '/', csv) )

names(dt)
# Hmisc::describe(dt) ## too slow
# ulst <- lapply(dt, unique)
# uls  <- dt %>% summarise_each(funs(n_distinct))
unique(dt$cause_name)
unique(dt$location_name) %>% length() ## 204 countries are included
dt %>% dplyr::distinct(measure_id,measure_name)
dt %>% dplyr::distinct(cause_id, cause_name) %>% arrange(cause_id)
dt %>% dplyr::distinct(sex_id,   sex_name)
dt %>% dplyr::distinct(metric_id, metric_name)



## To get the aggregated number of death due to meat-related NCDs. Ignore age, gender, disease type/cause etc. 
## --> According to Min, this data only included we included three chronic diet-related NCDs regarding red and processed meat consumption (Chung et al 2021):  
##      - 441 (colon and rectum cancer), 
##      - 493 (Ischemic heart disease).
##      - 976 (diabetes mellitus type 2)

##  

death_NCD_total <- dt %>%
  dplyr::filter(cause_id %in% c(441, 976, 493)) %>% ## only included three causes, not all causes.
  dplyr::filter(metric_name == 'Number') %>%
  group_by(measure_name, location_id, location_name,
           # rei_id, rei_name, 
           year) %>%
  dplyr::summarise(val_total = sum(val, na.rm = T)) %>%
  dplyr::filter(year %in% c(2000, 2005, 2010, 2015)) %>%
  ungroup() %>%
  merge(x = ., y = ctr_list, by.x = 'location_id', by.y = 'Location.ID', all.x = T) %>%
  dplyr::select(location_name, ISO3, location_id, year, val_total) %>%
  arrange(!is.na(ISO3), ISO3) %>%
  as.data.frame()


  
  

## NCD embodied in trade ---------------------------------------------------------------------------
# # R: red meat
# # P: processed meat
# # death.change.RPM : death_NCD_imports (including both red and processed meat)
# # death_percent    : death_NCD_imports/death_NCD_total * 100
 


# dat <- readr::read_csv('GHDx_RPM_death_change_country_062221.csv')
# names(dat)
# dat_rpm <- dat %>%
#   dplyr::select(!matches('lower|upper')) %>%
#   dplyr::filter(year %in% c(2000, 2005, 2010, 2015)) %>%
#   dplyr::rename(death_NCD_total  = val,
#                 death_NCD_imports = death.change.RPM)
# names(dat_rpm)


## compare the data with Table 1 in BMJ paper --> good match! 
# dat_rpm_bmj <- dat %>%
#   dplyr::select(!matches('lower|upper')) %>%
#   dplyr::filter(year >= 2016 & year <= 2018) %>%
#   group_by(ISO3) %>%
#   dplyr::summarise(death_percent_mean = mean(death_percent, na.rm = TRUE)) %>%
#   arrange(desc(death_percent_mean))
  


### Compare `dat_rpm` with `death_NCD_total` regarding the total NCD death in each country --> the same!
# dat_rpm_VS_death_NCD_total <- merge(x = dat_rpm, y = death_NCD_total, by = c('ISO3', 'year'), all.x = T) %>%
#   dplyr::mutate(check1 = round(death_NCD_total/val_total, digits = 2), ## --> 'death_NCD_total' is the same as val_total
#                 check2 = round(death_NCD_imports/death_NCD_total*100 - death_percent, digits = 2)) %>% 
#   as.data.frame()



### data for MC-SDG --------------------------------------------------------------------------------
NCD_death_number_of_person <- death_NCD_total %>%
  ## format data
  dplyr::select(-location_id) %>%
  spread(key = year, value = val_total) %>%
  dplyr::rename(iso3 = ISO3, ctr = location_name) %>%
  dplyr::select(iso3, ctr, everything()) %>%
  merge(x = ctr_eora, y = ., by.x = 'iso3_eora', by.y = 'iso3', all.x = T) %>%
  dplyr::select(iso3_eora, country_eora, `2000`, `2005`, `2010`, `2015`)  %>%
  dplyr::rename(iso3 = iso3_eora, ctr = country_eora) %>%
  arrange(iso3) %>%
  as.data.frame()

print(paste0('There are ', round(sum(is.na(NCD_death_number_of_person))/(190*4)*100, digits = 1), '% NA'))
fname   <- paste0(dir.cleaned, 'NCD_death_number_of_person_4yrs.xlsx'); fname
writexl::write_xlsx(NCD_death_number_of_person, fname)



### Set the path to the root dir of the Repo. 
setwd(dir = root)
getwd()
