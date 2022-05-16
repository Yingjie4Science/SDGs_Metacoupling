
### set work dir
path <- rstudioapi::getSourceEditorContext()$path
dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
setwd(dir)
setwd('..') # set directory by one folder up
getwd()


dir.raw     <- './Data/data_01_raw/'
getwd()
source('./Code/_package list.R')
# install.packages("devtools")
library(devtools)
# devtools::install_github("ropensci/comtradr")
library(comtradr)
### https://docs.ropensci.org/comtradr/
### https://github.com/ropensci/comtradr/
### https://comtrade.un.org/data/Doc/api/ex/r
### https://cran.r-project.org/web/packages/comtradr/vignettes/comtradr-vignette.html


## Comtrade package ################################################################################

### Countries that included in UN Comtrade ----------------------------------------------------------
## -> see https://comtrade.un.org/Data/cache/reporterAreas.json
xls <- paste0(dir.raw, 'UN_Comtrade/reporterAreas.xlsx')
ctr_reporter <- readxl::read_excel(path = xls) %>%
  dplyr::mutate(head = str_squish(head),
                head = gsub(':|\"|id|;', '', head),
                head = str_squish(head)) %>%
  separate(col = 'head', into = c('id', 'text'), sep = 'text', remove = F) %>%
  dplyr::mutate(id = str_squish(id), text = str_squish(text)) %>%
  dplyr::select(-head) %>%
  dplyr::filter(id != 'all') %>%
  as.data.frame()


## remove "All" from the list
ctr_list <- ctr_reporter$text; ctr_list


xls <- paste0(dir.raw, 'UN_Comtrade/Comtrade Country Code and ISO list.xlsx')
ctr_comtrade <- readxl::read_excel(path = xls) %>%
  dplyr::rename(id = `Country Code`, 
                iso3 = `ISO3-digit Alpha`,
                name      = `Country Name, Abbreviation`,
                name_full = `Country Name, Full`) %>%
  dplyr::select(id, iso3, name, name_full) %>%
  as.data.frame()

### Merge the 2 list 
ctr_comtrade_iso <- merge(ctr_reporter, ctr_comtrade, by = 'id', all = T) %>%
  dplyr::filter(iso3 != 'N/A') %>%
  ### check names
  dplyr::mutate(check = text == name) %>%
  dplyr::mutate(name = ifelse(id == 652, 'Saint Barthelemy', name)) %>%
  dplyr::mutate(check = text == name) %>%
  ###
  # dplyr::select(id, iso3, name) %>%
  as.data.frame()



### Get to know the functions -----------------------------------------------------------------------

ct_country_lookup("korea", ignore.case = T)
#> [1] "Dem. People's Rep. of Korea" "Rep. of Korea"
ct_country_lookup("korea", "reporter")
ct_country_lookup("bolivia", "partner")

ct_commodity_lookup(c("tomato", "Pesticide"), verbose = TRUE, ignore.case = T)
ct_commodity_lookup(c("cide"), verbose = TRUE, ignore.case = T)
ct_commodity_lookup(c("Insecticide"), verbose = TRUE, ignore.case = T)

### Since we want South Korea, we'll use "Rep. of Korea" within the API query.
# shrimp_codes <- ct_commodity_lookup("shrimp", return_code = TRUE, return_char = TRUE); shrimp_codes
shrimp_codes <- ct_commodity_lookup(c("Insecticide"), return_code = TRUE, return_char = TRUE); shrimp_codes

example1 <- ct_search(reporters = 'China', #"China",   
                      partners = 'All', # c("Rep. of Korea", "USA", "Mexico"), 
                      commod_codes = shrimp_codes,  
                      # trade_direction = "exports",
                      start_date = 2015, end_date = 2016,
                      freq = 'annual')

# Inspect the return data
str(example1)

ct_update_databases()
# ct_update_databases(commodity_type = "SITC") # this will download the commodity table that follows the “Standard International Trade Classification” scheme).
ct_commodity_db_type()

colnames(example1)
### Apply polished col headers
example2 <- ct_use_pretty_cols(example1)
colnames(example2)



### Loop download data ------------------------------------------------------------------------------

dataset <- data.frame()

for (ctr in ctr_list) {
  print(ctr)
  
  
  dt <- ct_search(reporters = ctr,   
                  partners = 'All', 
                  commod_codes = shrimp_codes,  
                  # trade_direction = "exports", ## default is 'all'
                  # start_date = 2015, end_date = 2019,
                  freq = 'annual')
  
  
  # dataset <- rbind(dataset, dt)
  
  writexl::write_xlsx(dt, path = paste0(dir.raw, 'UN_Comtrade/Insecticides/', ctr, '.xlsx'))
  
}



### the most recent country this code worked on 
ctr 
ctr_list
id_upto <- which(ctr_list == ctr); id_upto ## get the index number
ctr_list_update <- ctr_list[id_upto:length(ctr_list)]; ctr_list_update
ctr_list        <- ctr_list_update

### continue ...
for (ctr in ctr_list) {
  print(ctr)
  
  dt <- ct_search(reporters = ctr,   
                  partners = 'All', 
                  commod_codes = shrimp_codes,  
                  # trade_direction = "exports", ## default is 'all'
                  # start_date = 2015, end_date = 2019,
                  freq = 'annual')
  
  # dataset <- rbind(dataset, dt)
  writexl::write_xlsx(dt, path = paste0(dir.raw, 'UN_Comtrade/Insecticides/', ctr, '.xlsx'))
}



# ct_register_token()
ct_get_remaining_hourly_queries() ## return the number of remaining queries for the current hour.
ct_get_reset_time() ##will return the date/time in which the current hourly time limit will reset.






## Put data from each country together
ls_xlsx <- list.files(path = paste0(dir.raw, 'UN_Comtrade/Insecticides/'), pattern = 'xlsx$', full.names = T); 
ls_xlsx
length(ls_xlsx)

ls_xlsx_df <- data.frame(name = basename(ls_xlsx)) %>%
  dplyr::mutate(name = gsub('.xlsx', '', name)) %>%
  merge(., ctr_comtrade_iso %>% dplyr::select(name, iso3), 
        by = 'name', all.x = T)
# file.rename(from = ls_xlsx, to = ?)



dfs <- data.frame()
for (xlsx in ls_xlsx) {
  print(basename(xlsx))
  d   <- readxl::read_excel(path = xlsx)
  dfs <- rbind(dfs, d)
}


## Clean up the full data set
names(dfs)
unique(dfs$is_leaf_code)
unique(dfs$second_partner)
unique(dfs$customs)
unique(dfs$customs_proc_code)
unique(dfs$mode_of_transport)
unique(dfs$alt_qty)
unique(dfs$alt_qty_unit)
unique(dfs$gross_weight_kg)
unique(dfs$cif_trade_value_usd)
unique(dfs$fob_trade_value_usd)
dfs %>% ggplot() + geom_point(aes(x = qty, y = netweight_kg)) ## this 2 looks the same?

unique(dfs$qty_unit)

dfs %>%
  distinct(classification, year) %>%
  arrange(year) %>%
  as.data.frame()

dfs_c <- dfs %>%
  dplyr::select(-c(period, period_desc,second_partner_code, reporter_code, partner_code, second_partner, second_partner_iso, 
                   customs, customs_proc_code, mode_of_transport, mode_of_transport_code, alt_qty, alt_qty_unit, alt_qty_unit_code,
                   gross_weight_kg, cif_trade_value_usd, fob_trade_value_usd,
                   flag))





### Manual downloads ################################################################################
## - This dataset is not complete 
csv <- paste0(dir.raw, 'UN_Comtrade/comtrade_Insecticides.csv')
# dt0 <- read_csv(file = csv) 
# 
# names(dt0)
# names(dt0) <- names(example1)
# dt1 <- dt0 %>%
#   dplyr::select(-c("period", "period_desc", "aggregate_level", "is_leaf_code", 
#                    "reporter_code", "partner_code",
#                    "second_partner_code", "second_partner", "second_partner_iso", 
#                    "customs_proc_code", "customs", "mode_of_transport_code",
#                    "qty_unit_code","qty_unit","alt_qty_unit_code","alt_qty_unit","qty","alt_qty", "gross_weight_kg",
#                    "trade_value_usd", "cif_trade_value_usd", "fob_trade_value_usd", "flag"))
#   
# 
# names(dt1)
# names(example2)
# unique(dt1$trade_flow)
# unique(dt1$mode_of_transport)






## FishStatJ #######################################################################################

### HS code --------------
###   This file is downloaded from the Metadata moduel in `FishStatJ` software 
xlsx <- paste0('./Data/data_01_raw/FAOSTAT/FishStatJ_exportCSV/', 'ISSCFC-new_cleaned.xlsx')
fao_comm_code <- readxl::read_excel(xlsx) %>%
  dplyr::mutate(hs2017 = paste0('X', `HS 2017`, 'Y'),
                hs2017 = gsub('\\.|X', '', hs2017),
                hs2017 = str_pad(hs2017, 7, pad = "0"),
                hs2017 = gsub('Y', '', hs2017)) %>%
  dplyr::rename(fao_ISSCFC_desc = `FAO ISSCFC description in English`) %>%
  dplyr::mutate(fao_ISSCFC_desc = stringr::str_replace_all(string = fao_ISSCFC_desc, pattern = fixed("\n"), replacement = " "),      # erase new lines
                fao_ISSCFC_desc = stringr::str_replace_all(string = fao_ISSCFC_desc, pattern = fixed("\r"), replacement = " ")) %>%  # erase carriage return)
  dplyr::mutate(fao_ISSCFC_desc = stringr::str_squish(string = fao_ISSCFC_desc)) %>%
  ### need to correct the format difference in descriptions for better data join
  dplyr::mutate(fao_ISSCFC_desc = ifelse(id=="170", "Anchovies (Engraulis spp), fresh or chilled", fao_ISSCFC_desc),
                fao_ISSCFC_desc = ifelse(id=="281", "Common sole (Solea Solea), frozen", fao_ISSCFC_desc),
                fao_ISSCFC_desc = ifelse(id=="924", "Herring,achovy,sardine,sardinella,brisling/sprat,mackerel,Indian mackerel,seerfish,jack&horse mackerel,jack,crevalle,cobia,silver pomfret,Pacif.saury,scad,capelin,etc,dried,salted or not,not smoked", fao_ISSCFC_desc),
                fao_ISSCFC_desc = ifelse(id=="1048", "Shark fins, smoked, dried, whether or not salted, etc.", fao_ISSCFC_desc),
                fao_ISSCFC_desc = ifelse(id=="282", "Soles (Solea spp), frozen, nei", fao_ISSCFC_desc),
                fao_ISSCFC_desc = ifelse(id=="20", "Southern bluefin tuna (Thunnus maccoyii), live", fao_ISSCFC_desc)
                
  )
  
names(fao_comm_code)

fao_comm_code_hs2017 <- fao_comm_code %>%
  dplyr::distinct(id, hs2017, fao_ISSCFC_desc)


### Values  ----
###   in 1000 USD
csv <- 'trade_value_FAOSTAT_group.csv'
csv <- 'trade_value_HS2017.csv'
csv <- 'trade_value.csv'
fao_v <- readr::read_csv(file = paste0('./Data/data_01_raw/FAOSTAT/FishStatJ_exportCSV/', csv)) %>%
  gather(key = 'year', value = 'v', 6:ncol(.)) 

fao_v1 <- fao_v %>%
  dplyr::select(-c(`Unit (Name)`, Unit)) %>%
  dplyr::mutate(year  = gsub('\\[|\\]', '', year),
                v = gsub(" E| N| T| I", '', v),
                v = trimws(v),
                v = as.numeric(v),
                year  = as.numeric(year)
                )
names(fao_v1)
names(fao_v1) <- c('reporter', 'commodity', 'trade', 'year', 'v')


### Quantity ----
###   in Tonnes
csv <- 'trade_quantity_FAOSTAT_group.csv'
csv <- 'trade_quantity_HS2017.csv'
csv <- 'trade_quantity.csv'
fao_q <- readr::read_csv(file = paste0('./Data/data_01_raw/FAOSTAT/FishStatJ_exportCSV/', csv)) %>%
  gather(key = 'year', value = 'q', 6:ncol(.)) 

fao_q1 <- fao_q %>%
  dplyr::select(-c(`Unit (Name)`, Unit)) %>%
  dplyr::mutate(year  = gsub('\\[|\\]', '', year),
                q = gsub(" E| N| T| I", '', q),
                q = trimws(q),
                q = as.numeric(q),
                year  = as.numeric(year)
  )

names(fao_q1)
names(fao_q1) <- c('reporter', 'commodity', 'trade', 'year', 'q')


### USD per tonne --------

#### merge data ----------
fao_vq <- merge(fao_v1, fao_q1, by = c('reporter', 'commodity', 'trade', 'year'), all = T)


#### join with HS code ----
fao_vq_hs2017 <- fao_vq %>% distinct(commodity) %>%
  dplyr::mutate(commodity = stringr::str_squish(string = commodity)) %>%
  merge(x = ., y = fao_comm_code_hs2017, by.x = 'commodity', by.y = 'fao_ISSCFC_desc', all.x = T)  %>% 
  arrange(!is.na(hs2017)) %>%
  as.data.frame()

### how many failed to link with HS2017? 
fao_vq_hs2017 %>%
  dplyr::filter(is.na(hs2017)) %>%
  nrow(.)
### --> 24 --> 6 --> 0


#### price per tonne ----
fao_vq_per <- fao_vq %>%
  dplyr::mutate(commodity = stringr::str_squish(string = commodity)) %>%
  merge(x = ., y = fao_comm_code_hs2017, by.x = 'commodity', by.y = 'fao_ISSCFC_desc', all.x = T)  %>% 
  arrange(!is.na(hs2017)) %>%
  dplyr::mutate(usd_per_ton = v*1000/q) %>%
  # arrange(year, reporter, commodity, trade) %>%
  as.data.frame()


### count the NAs over multiple columns
sapply(fao_vq_per, function(x) sum(is.na(x)))


fao_vq_per1 <- fao_vq_per %>%
  dplyr::select(-id) %>%
  group_by(reporter, commodity, hs2017, year, trade) %>%
  dplyr::summarise_if(is.numeric, mean, na.rm = TRUE)

# fao_vq_per %>%
#   group_by(commodity, year) %>%
#   ggplot() +
#   geom_point(aes(x = year, y = usd_per_ton)) +
#   facet_wrap(.~commodity)




### Production -------------------------------------------------------------------------------------
csv <- 'production_ASFISdetailed.csv'
fao_p <- readr::read_csv(file = paste0('./Data/data_01_raw/FAOSTAT/FishStatJ_exportCSV/', csv)) %>%
  gather(key = 'year', value = 'value', 6:ncol(.)) 

fao_p1 <- fao_p %>%
  dplyr::select(-c(3, 4, 5)) %>%
  dplyr::mutate(year  = gsub('\\[|\\]', '', year),
                value = gsub(" E| N| T| I", '', value),
                value = trimws(value),
                value = as.numeric(value),
                year  = as.numeric(year)
  ) %>%
  dplyr::rename(p = value)
names(fao_p1)
names(fao_p1) <- c('reporter', 'commodity', 'year', 'p')


### To get the total tonnage of production, regardless species 
fao_p1_total <- fao_p1 %>%
  dplyr::filter(year >= 2000) %>%
  dplyr::group_by(reporter, year) %>%
  dplyr::summarise(p = sum(p, na.rm = T))










## BACI ############################################################################################
## - BACI provides disaggregated data on bilateral trade flows for more than 5000 products and 200 countries. 

# t	Year
# k	Product category (HS 6-digit code)
# i	Exporter (ISO 3-digit country code)
# j	Importer (ISO 3-digit country code)
# v	Value of the trade flow (in thousands current USD)
# q	Quantity (in metric tons)


dir.baci <- "D:/data/BACI/"

###
csv      <- paste0(dir.baci, 'country_codes_V202102.csv')
baci_ctr <- read_csv(csv) %>% as.data.frame()
 

### HS -----------------

'HS2017toHS1992ConversionAndCorrelationTables.xlsx' ## https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp

xlsx <- paste0(dir.raw, 'UN_Comtrade/HS2017toHS1992ConversionAndCorrelationTables.xlsx')
hs_t <- readxl::read_excel(xlsx) %>%
  dplyr::rename(hs92 = `To HS 1992`, hs17 = `From HS 2017`)

hs92 <- readr::read_csv(file = paste0('D:/data/BACI/', 'product_codes_HS92_V202102.csv'), col_types = 'cc') %>% 
  dplyr::mutate(code = str_pad(code, 6, pad = "0")) %>%
  dplyr::filter(str_starts(string = code, pattern = '03')) %>%
  as.data.frame()
hs17 <- readr::read_csv(file = paste0('D:/data/BACI/', 'product_codes_HS17_V202102.csv'), col_types = 'cc') %>% 
  as.data.frame() %>%
  dplyr::mutate(code = str_pad(code, 6, pad = "0")) %>%
  dplyr::filter(str_starts(string = code, pattern = '03')) %>%
  as.data.frame()

str(hs92)
str(hs17)
names(hs_t)

# hs92_17 <- 
#   merge(x = hs92, y = hs17 , by   = 'code', all = T) %>%
#   merge(x = .,    y = hs_t,  by.x = 'code', by.y = 'hs17', all = T)



### Trade data -------------------------------------------------------------------------------------
csv <- paste0(dir.baci, "BACI_HS92_V202102/", "BACI_HS92_Y2015_V202102.csv"); csv
# baci <- readr::read_csv(csv)
head(baci)

csvs <- paste0(dir.baci, "BACI_HS92_V202102/", "BACI_HS92_Y", seq(2000, 2015, 5), "_V202102.csv"); csvs

# bacis <- data.frame()
# for (csv in csvs) {
#   print(csv)
#   baci  <- readr::read_csv(csv)
#   bacis <- rbind(bacis, baci)
# }
# save(bacis, file = paste0(dir.baci, 'BACI_HS92.RData'))

load(file = paste0(dir.baci, 'BACI_HS92.RData'))



#### Fishery ---------------------------
ind <- 'Fishery'

### 1. total flows by year, by commodity, regardless of i and j
# baci_k <- bacis %>% 
#   group_by(t, k) %>%
#   dplyr::summarise_at(vars(v:q), sum, na.rm = TRUE)
# 
# baci_k_hs2017 <- baci_k %>%
#   ### - merge and add HS2017 code
#   merge(x = hs_t, y = ., by.x = 'hs92', by.y = 'k', all.y = T) %>%
#   ### - merge and only keep fishery-related HS2017 code
#   merge(x = ., y = fao_comm_code_hs2017, by.x = 'hs17', by.y = 'hs2017', all.x = T) %>%
#   dplyr::filter(!is.na(id)) %>%
#   as.data.frame()
# 
# ### count the NAs over multiple columns
# sapply(baci_k_hs2017, function(x) sum(is.na(x)))


### 2. total flows by year, by i, by j, regardless of k (commodity) 
load(file = paste0('./Data/data_02_intermediate/dt02_flows/', 'template_eora.RData')) # `temp_eora`

head(bacis)
baci_ij <- bacis %>% 
  ### - merge and add HS2017 code
  merge(x = hs_t, y = ., by.x = 'hs92', by.y = 'k', all.y = T) #%>%
  ### - merge and only keep fishery-related HS2017 code
  merge(x = ., y = fao_comm_code_hs2017, by.x = 'hs17', by.y = 'hs2017', all.x = T) %>%
  dplyr::filter(!is.na(id)) %>%
  ### - total flows among countries
  group_by(t, i, j) %>%
  dplyr::summarise_at(vars(v:q), sum, na.rm = TRUE) #%>%
  ### - add country code 
  merge(., baci_ctr %>% dplyr::select(country_code, iso_3digit_alpha), by.x = 'i', by.y = 'country_code', all.x = T) %>%
  dplyr::rename(o = iso_3digit_alpha) %>%
  merge(., baci_ctr %>% dplyr::select(country_code, iso_3digit_alpha), by.x = 'j', by.y = 'country_code', all.x = T) %>%
  dplyr::rename(d = iso_3digit_alpha) %>%
  dplyr::select(-i, -j, -v) %>%
  ### - To generate the same format as Eora in Matrix
  merge(x = temp_eora, y = ., by.x = c('year', 'iso3', 'to'), by.y = c('t', 'o', 'd'), all.x = T) %>%
  arrange(year, iso3, to) %>%
  dplyr::select(year, ctr, everything()) %>%
  spread(key = to, value = q) %>%
  arrange(year, iso3) %>%
  as.data.frame()


### save to the `eora_cleaned` folder 
df_formatted <- baci_ij 
df_formatted %>%
  group_by(year) %>% tally()
fname <- paste0(dir.eora_cleaned, ind, '.xlsx'); fname
writexl::write_xlsx(x = df_formatted, path = fname)
























