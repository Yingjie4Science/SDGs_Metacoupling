

# Setup -----

### To clear your environment 
# remove(list = ls())

### set work dir
path <- rstudioapi::getSourceEditorContext()$path
dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
setwd(dir)
setwd('..') # set directory by one folder up
getwd()


dir.raw      <- './Data/data_01_raw/'
dir.fishstat <- './Data/data_01_raw/FAOSTAT/FishStatJ_exportCSV/'
dir.cleaned      <- './Data/data_02_intermediate/dt01_ctr_profile/xlsx/cleaned/'
getwd()
source('./Code/_package list.R')

library(janitor) ## Simple Tools for Examining and Cleaning Dirty Data





#   Data ===========================================================================================

## FishStatJ Metadata - Fish name & code -----
# xlsx <- "Metadata_Production_ASFIS species.xlsx"
# xlsx <- paste0('./Data/data_01_raw/FAOSTAT/FishStatJ_exportCSV/', xlsx)
# fish_p_code <- readxl::read_excel(path = xlsx)
# ### --> no use at this moment, as there is no 'key' column for matching with the trade code - `fish_hs17_hs92_desc`
# 
# 
## FishStatJ Metadata - Country code ---------
ctr_p <- readxl::read_excel(paste0(dir.fishstat, 'Metadata_Production_Country.xlsx'))





## Production --------------------------------

  # Production can come from (1) wild capture, and (2) aquaculture. Each source also include productions from "Marine waters" or "Inland waters". 
  # 
  # Here, for SDG 14, we mainly focus on *wild capture from marine waters*.


### data 1 - Global production by production source ----------------------------------

csv <- 'Global production by production source - Quantity (1950 - 2019).csv'
p1 <- readr::read_csv(file = paste0('./Data/data_01_raw/FAOSTAT/FishStatJ_exportCSV/', csv)) %>%
  gather(key = 'year', value = 'value', 6:ncol(.)) 

p1 <- p1 %>%
  # dplyr::select(-c(3, 4, 5)) %>%
  dplyr::mutate(year  = gsub('\\[|\\]', '', year),
                # value = gsub(" E| N| T| I", '', value),
                # value = trimws(value),
                # value = as.numeric(value),
                year  = as.numeric(year)
  ) %>%
  # dplyr::filter(year == 2015) %>%
  dplyr::filter(year >= 2000) %>%
  janitor::clean_names() %>%
  dplyr::filter(detailed_production_source_name == "Capture production") %>%
  arrange(country_name, asfis_species_name) %>%
  dplyr::rename(p = value)

unique(p1$detailed_production_source_name)
str(p1)




### data 2 - Global capture production - Quantity -------------------------------------------
# f <- "Global capture production - Quantity (1950 - 2019).csv"
# f <- paste0('./Data/data_01_raw/FAOSTAT/FishStatJ_exportCSV/', f)
# p2 <- readr::read_csv(file = f) %>%
#   gather(key = 'year', value = 'value', 6:ncol(.)) %>%
#   janitor::clean_names() %>%
#   dplyr::mutate(year  = gsub('\\[|\\]', '', year),
#                 # value = gsub(" E| N| T| I", '', value),
#                 # value = trimws(value),
#                 # value = as.numeric(value),
#                 year  = as.numeric(year)) %>%
#   dplyr::filter(year == 2015) %>%
#   arrange(country_name, asfis_species_name) %>%
#   dplyr::rename(p = value)
# str(p2)
### --> this is the same result as `p1`




### filter production from 'marine capture' data only (i.e., remove "Inland waters“)
pm <- p1 %>%
  dplyr::filter(str_detect(string = fao_major_fishing_area_name, pattern = "Inland waters", negate = T)) %>%
  as.data.frame()

# pinland <- p1 %>%
#   dplyr::filter(str_detect(string = fao_major_fishing_area_name, pattern = "Inland waters", negate = F))
# nrow(pm) + nrow(pinland)
str(pm)


pmarine <- pm %>%
  as.data.frame() %>%
  dplyr::mutate(fao_major_fishing_area_name = 'Marine areas') %>% ## rename all the areas as this, according to "FishstatJ"
  dplyr::ungroup() %>%
  dplyr::group_by(country_name, year, detailed_production_source_name, unit_name) %>%
  dplyr::summarise(p = sum(p, na.rm = T)) %>%
  dplyr::filter(str_detect(string = unit_name, pattern = "Tonnes - live weight", negate = F))



### --> add country code to the data

### fix country names
pmarine <- pmarine %>%
  dplyr::mutate(country_name = ifelse(str_starts(country_name, pattern = 'Cura'), "Curaçao", country_name),
                country_name = ifelse(str_starts(country_name, pattern = 'Saint Barth'), "Saint Barthélemy", country_name),
                country_name = ifelse(str_ends(country_name,   pattern = 'Ivoire'), "Côte d'Ivoire", country_name),
                country_name = ifelse(str_ends(country_name,   pattern = 'union'), "Réunion", country_name)
)




### count the NAs over multiple columns
sapply(pmarine, function(x) sum(is.na(x)))


pmarine_4yr <- pmarine %>%
  dplyr::filter(year %in% seq(2000, 2015, 5)) %>%
  # distinct(reporter) %>%
  merge(x = ., 
        y = ctr_p %>% dplyr::select(`NAME.EN`, ISO_3_CODE), 
        by.x = 'country_name', by.y = 'NAME.EN', all.x = T) %>%
  dplyr::rename(iso3 = ISO_3_CODE) %>%
  dplyr::select(country_name, iso3, year, p)


### compare with World Bank data
xlsx <- "./Data/data_02_intermediate/dt01_ctr_profile/xlsx/cleaned/Total fisheries production (metric tons)_4yrs.xlsx"
fish_wb_fao <- readxl::read_excel(xlsx) %>%
  gather(key = year, value = value, 3:ncol(.)) 

fish <- merge(x = pmarine_4yr, y = fish_wb_fao, by = c('iso3', 'year'), all = T) 
fish %>%
  ggplot(aes(x = value, y = p)) +
  geom_point() +
  geom_abline(color = 'red') +
  theme_bw()


### --> almost the same! But the world bank data might include inland production. 

### format the dataframe
iname <- "Fisheries production - marine capture in tons"
ctr_eora <- readxl::read_xlsx('./Data/_eora_190country_iso3.xlsx') 

dfwb_fillna_4yrs <- pmarine_4yr %>%
  spread(key = year, value = p) %>%
  left_join(x = ctr_eora, y = ., by = c("iso3_eora" = "iso3")) %>%
  dplyr::select(iso3_eora, country_eora, `2000`, `2005`, `2010`, `2015`)  %>%
  dplyr::mutate_at(c(3:6), ~replace(., is.na(.), 0)) %>%
  arrange(iso3_eora)
names(dfwb_fillna_4yrs) <- c('iso3', 'ctr', "2000", "2005", "2010", "2015")
fname <- paste0(dir.cleaned, iname, '_4yrs.xlsx'); print(fname) 
writexl::write_xlsx(x = dfwb_fillna_4yrs, path = fname)
print(paste0('There are ', round(sum(is.na(dfwb_fillna_4yrs))/(190*4)*100, digits = 1), '% NA'))













## Trade flows -------------------------------------------------------------------------------------

###  FishStatJ Metadata - HS code  ---------------
# ###   This file is downloaded from the Metadata moduel in `FishStatJ` software 
# xlsx <- paste0(dir.fishstat, 'Metadata_Trade_ISSCFC-new_cleaned.xlsx')
# fao_comm_code <- readxl::read_excel(xlsx) %>%
#   dplyr::mutate(hs17 = paste0('X', `HS 2017`, 'Y'),
#                 hs17 = gsub('\\.|X', '', hs17),
#                 hs17 = str_pad(hs17, 7, pad = "0"),
#                 hs17 = gsub('Y', '', hs17)) %>%
#   dplyr::rename(fao_ISSCFC_desc = `FAO ISSCFC description in English`) %>%
#   dplyr::mutate(fao_ISSCFC_desc = stringr::str_replace_all(string = fao_ISSCFC_desc, pattern = fixed("\n"), replacement = " "),      # erase new lines
#                 fao_ISSCFC_desc = stringr::str_replace_all(string = fao_ISSCFC_desc, pattern = fixed("\r"), replacement = " ")) %>%  # erase carriage return)
#   dplyr::mutate(fao_ISSCFC_desc = stringr::str_squish(string = fao_ISSCFC_desc)) %>%
#   ### need to correct the format difference in descriptions for better data join
#   dplyr::mutate(fao_ISSCFC_desc = ifelse(id=="170", "Anchovies (Engraulis spp), fresh or chilled", fao_ISSCFC_desc),
#                 fao_ISSCFC_desc = ifelse(id=="281", "Common sole (Solea Solea), frozen", fao_ISSCFC_desc),
#                 fao_ISSCFC_desc = ifelse(id=="924", "Herring,achovy,sardine,sardinella,brisling/sprat,mackerel,Indian mackerel,seerfish,jack&horse mackerel,jack,crevalle,cobia,silver pomfret,Pacif.saury,scad,capelin,etc,dried,salted or not,not smoked", fao_ISSCFC_desc),
#                 fao_ISSCFC_desc = ifelse(id=="1048", "Shark fins, smoked, dried, whether or not salted, etc.", fao_ISSCFC_desc),
#                 fao_ISSCFC_desc = ifelse(id=="282", "Soles (Solea spp), frozen, nei", fao_ISSCFC_desc),
#                 fao_ISSCFC_desc = ifelse(id=="20", "Southern bluefin tuna (Thunnus maccoyii), live", fao_ISSCFC_desc)
#                 
#   )
# 
# names(fao_comm_code)
# 
# fish_hs17 <- fao_comm_code %>%
#   dplyr::distinct(id, hs17, fao_ISSCFC_desc)
# 
# 
# ### HS17 --> HS92
### https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# xlsx <- paste0('./Data/data_01_raw/UN_Comtrade/', 'HS2017toHS1992ConversionAndCorrelationTables.xlsx')
# hs_t <- readxl::read_excel(xlsx) %>%
#   dplyr::rename(hs92 = `To HS 1992`, hs17 = `From HS 2017`)
# 
# 
# fish_hs17_hs92_desc <- merge(fish_hs17, hs_t, by = 'hs17', all.x = T) %>%
#   ### - only keep fishery-related hs17 code
#   dplyr::filter(!is.na(id))
#   
# fish_hs17_hs92 <- fish_hs17_hs92_desc %>%  
#   dplyr::filter(!is.na(hs92)) %>%
#   distinct(hs17, hs92)
# 
# save(fish_hs17_hs92, file = paste0('./Data/_fish_hs17_hs92.RData'))


### Trade Values -----------
###   in 1000 USD
# csv <- 'trade_value_FAOSTAT_group.csv'
# csv <- 'trade_value_HS2017.csv'
# csv <- 'Global fish trade_value.csv'
# fao_v <- readr::read_csv(file = paste0('./Data/data_01_raw/FAOSTAT/FishStatJ_exportCSV/', csv)) %>%
#   gather(key = 'year', value = 'v', 6:ncol(.)) 
# 
# fao_v1 <- fao_v %>%
#   dplyr::select(-c(`Unit (Name)`, Unit)) %>%
#   dplyr::mutate(year  = gsub('\\[|\\]', '', year),
#                 v = gsub(" E| N| T| I", '', v),
#                 v = trimws(v),
#                 v = as.numeric(v),
#                 year  = as.numeric(year)
#                 )
# names(fao_v1)
# names(fao_v1) <- c('reporter', 'commodity', 'trade', 'year', 'v')
# 
# 
# 
# 
# ### Trade Quantity ------------ 
# ###   in Tonnes
# csv <- 'trade_quantity_FAOSTAT_group.csv'
# csv <- 'trade_quantity_HS2017.csv'
# csv <- 'Global fish trade_quantity.csv'
# fao_q <- readr::read_csv(file = paste0('./Data/data_01_raw/FAOSTAT/FishStatJ_exportCSV/', csv)) %>%
#   gather(key = 'year', value = 'q', 6:ncol(.)) 
# 
# fao_q1 <- fao_q %>%
#   dplyr::select(-c(`Unit (Name)`, Unit)) %>%
#   dplyr::mutate(year  = gsub('\\[|\\]', '', year),
#                 q = gsub(" E| N| T| I", '', q),
#                 q = trimws(q),
#                 q = as.numeric(q),
#                 year  = as.numeric(year)
#   )
# 
# names(fao_q1)
# names(fao_q1) <- c('reporter', 'commodity', 'trade', 'year', 'q')
# 
# 
# 
# 
# ### USD per tonne ----------------- 
# #### merge data ----------
# fao_vq <- merge(fao_v1, fao_q1, by = c('reporter', 'commodity', 'trade', 'year'), all = T)
# 
# 
# #### join with HS code ----
# fao_vq_hs2017 <- fao_vq %>% distinct(commodity) %>%
#   dplyr::mutate(commodity = stringr::str_squish(string = commodity)) %>%
#   merge(x = ., y = fao_comm_code_hs2017, by.x = 'commodity', by.y = 'fao_ISSCFC_desc', all.x = T)  %>% 
#   arrange(!is.na(hs2017)) %>%
#   as.data.frame()
# 
# ### how many failed to link with HS2017? 
# fao_vq_hs2017 %>%
#   dplyr::filter(is.na(hs2017)) %>%
#   nrow(.)
# ### --> 24 --> 6 --> 0
# 
# 
# #### price per tonne ----
# fao_vq_per <- fao_vq %>%
#   dplyr::mutate(commodity = stringr::str_squish(string = commodity)) %>%
#   merge(x = ., y = fao_comm_code_hs2017, by.x = 'commodity', by.y = 'fao_ISSCFC_desc', all.x = T)  %>% 
#   arrange(!is.na(hs2017)) %>%
#   dplyr::mutate(usd_per_ton = v*1000/q) %>%
#   # arrange(year, reporter, commodity, trade) %>%
#   as.data.frame()
# 
# 
# ### count the NAs over multiple columns
# sapply(fao_vq_per, function(x) sum(is.na(x)))
# 
# 
# fao_vq_per1 <- fao_vq_per %>%
#   dplyr::select(-id) %>%
#   group_by(reporter, commodity, hs2017, year, trade) %>%
#   dplyr::summarise_if(is.numeric, mean, na.rm = TRUE)
# 
# # fao_vq_per %>%
# #   group_by(commodity, year) %>%
# #   ggplot() +
# #   geom_point(aes(x = year, y = usd_per_ton)) +
# #   facet_wrap(.~commodity)



