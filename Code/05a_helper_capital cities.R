
##' https://github.com/cran/r2country
##' 
# remotes::install_github("oobianom/r2country")
# #or from CRAN
# install.packages("r2country") 
##load package
# library(r2country)
# capitals <- data(country_capital) #country capitals


### To clear your environment 
remove(list = ls())

load('./Data/Ancillary_Data_ISO3code_shp.RData') ## `iso_eora`, `pop`, `gdp` (USD), `shp`, `grp`
ctr_in_eora <- pop %>% dplyr::distinct(iso3, ctr)


library(maps)
data(world.cities)
data(iso3166)

iso3_maps <- iso3166 %>%
  dplyr::mutate(
    mapname = gsub("\\s*\\([^\\)]+\\)", "", as.character(mapname)),
    mapname = case_when(
    a3 == 'GBR' ~ 'UK', 
    T ~ mapname
  )) %>%
  dplyr::select(-a2, -ISOname, -sovereignty)


## to check which country used in EORA are not included 
ctr_check <- ctr_in_eora %>%
  left_join(., iso3_maps, by = c("iso3" = "a3")) %>%
  arrange(!is.na(mapname))


capital_city <- world.cities %>%
  dplyr::rename('city_name' = 'name') %>%
  dplyr::filter(capital ==1 | city_name == 'Hong Kong') %>%
  dplyr::mutate(
    country.etc = case_when(
      country.etc == 'Korea South' ~ 'South Korea',
      country.etc == 'Korea North' ~ 'North Korea',
      country.etc == 'Congo' ~ 'Republic of Congo',
      country.etc == 'US Virgin Islands' ~ 'Virgin Islands, US',
      country.etc == 'Turks and Caicos' ~ 'Turks and Caicos Islands',
      country.etc == 'Pitcairn' ~ 'Pitcairn Islands',
      country.etc == 'Saint Kitts and Nevis' ~ 'Saint Kitts',
      country.etc == 'East Timor' ~ 'Timor-Leste',
      country.etc == 'Saint Vincent and The Grenadines' ~ 'Saint Vincent',
      country.etc == 'Congo Democratic Republic' ~ 'Democratic Republic of the Congo',
      country.etc == 'British Virgin Islands' ~ 'Virgin Islands, British',
      T ~ country.etc
    )) %>%
  rbind(
    data.frame(city_name = "Hong Kong", country.etc = "China:Hong Kong", 
               pop = 7491609, lat = 22.3193, long = 114.1694, capital=1)
  ) %>%
  ## there are two duplicates: capitals for "Costa Rica" and "Cyprus"
  dplyr::distinct(city_name, country.etc, .keep_all = T)


capital_city_data <-  capital_city %>%
  left_join(., iso3_maps, by = c("country.etc" = "mapname")) %>%
  # dplyr::filter(a3 %in% c(ctr_in_eora, NA)) %>%
  dplyr::mutate(a3 = ifelse(country.etc == 'Netherlands Antilles' & is.na(a3), 'ANT', a3)) %>%
  arrange(!is.na(a3)) %>%
  dplyr::rename('iso3' = 'a3') %>%
  as.data.frame()

### Save to a file
f <- "./Data/capital_city_data.rds"
saveRDS(capital_city_data, file = f)

### load capital city data
# f <- "./Data/capital_city_data.rds"
# capital_city_data <- readRDS(file = f)