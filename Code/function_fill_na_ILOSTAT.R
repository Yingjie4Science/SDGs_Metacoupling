

### fill na function for wb data

library(imputeTS) ### for na_locf fuction
# library(xlsx)
library(writexl)
library(tidyverse)
library(lubridate)

function_fill_na_ILOSTAT <- function(df, year_from = 1990) {
  dfs <- data.frame()
  
  n <- length(unique(df$iso3_eora))
  for (i in seq(1:n)) {
    # print(i) 
    ### loop each country code
    # print(df$iso3_eora[i]) 
    
    df1 <- df %>%
      dplyr::filter(iso3_eora == df$iso3_eora[i]) %>% ## ROW, ERI, KWT, MNE
      gather(key = 'year', value =  'value', -c(1:3)) %>%
      dplyr::mutate(year = gsub('X', '', year)) %>%
      dplyr::mutate(year = year(as.Date(year, format="%Y"))) %>%
      ## filter out old years, due to many NA in earlier years
      dplyr::filter(year >= year_from) %>%
      dplyr::mutate(value = as.numeric(value)) %>%
      arrange(year) # %>% ## order by date
   
    
    ### if too many NA, then NA; if less, use interpolation
    if (sum(is.na(df1$value)) > (length(df1$iso3_eora)-2)) {
      df1 <- df1 %>% 
        dplyr::mutate(value_ks = value)} 
    else {
      df1 <- df1 %>% 
        dplyr::mutate(value_ks = na_interpolation(x = value, option = 'stine'))} 
    
    
    ### ifelse within dplyr does NOT work well 
    # mutate(value_ks = ifelse(
    #   sum(is.na(value)) > (length(iso3_eora)-2), 
    #   value,
    #   na_interpolation(x = value, option = 'stine'))) ## linear, stine
    
    dfs <- rbind(dfs, df1)
  }
  
  
  ### plot and examine the interpolation
  # sam <- sample(1:190, size = 20, replace = F) ## replace = repeat
  plot <- dfs %>%
    ### randomly choose 20 countries and plot
    # slice(sample(1:190, size = 20, replace = F)) %>%
    # slice(1:39*10) %>%
    dplyr::filter(iso3_eora %in% df$iso3_eora[sample(1:190, size = 20, replace = T)]) %>% 
    
    ggplot()+
    geom_line(aes(x = year, y = value),    color = 'blue', size = 6) +
    geom_line(aes(x = year, y = value_ks), color = 'red', size = 2) +
    facet_wrap(~iso3_eora, scales = 'free_y') +
    # xlim(1990, 2005) +
    # ylim(0, 7*10^5)+
    theme_bw()
  #
  
  dfs_fillna <- dfs %>%
    dplyr::select(-value) %>%
    spread(key = year, value = value_ks) %>%
    left_join(x=ctr_eora, y = ., by=c('country_eora', 'iso3_eora')) ### make sure the same order 
  fname <- paste0(dir.process, iname, '_FILLNA.xlsx'); print(fname)
  # fname <- paste0(dir.process, gsub('.csv', '_FILLNA.xlsx', csv)); print(fname)
  # write.csv(x = dfs_fillna, file = fname, row.names = F)
  write_xlsx(x = dfs_fillna, path = fname)
  return(plot)
}



# ######################################################################### #
# # Fill NA - WB test ##################################################### #
# ### test 
# df1 <- df %>% 
#   filter(iso3_eora == 'ARG') %>% ## ROW, ERI, KWT, MNE
#   gather(key = 'year', value =  'value', X1990:X2018) %>%
#   mutate(year = gsub('X', '', year)) %>%
#   mutate(year = year(as.Date(year, format="%Y"))) %>%
#   arrange(year) %>% ## order by date
#   ## fill NAs
#   mutate(
#     value_lo        = na_locf(x = value), ## "simple","linear" or "exponential".
#     value_ks_linear = na_interpolation(x = value, option = 'linear'),
#     value_ks_spline = na_interpolation(x = value, option = 'spline'),
#     value_ks_stine  = na_interpolation(x = value, option = 'stine'),  # best so far
#     value_wm_linear = na_ma(x = value, k = 10, weighting = 'linear'),
#     value_z         = value)
# 
# df1 %>%
#   gather(key = 'itp', value = 'value', value:value_z) %>%
#   ggplot()+
#   geom_line(aes(x = year, y = value, color = itp), size = 2) +
#   # xlim(1990, 2005) +
#   # ylim(0, 7*10^5)+
#   theme_bw()
# 
# ### test code
# dft <- df %>%
#   filter(iso3_eora == 'ARG') %>% ## ROW, ERI, KWT, MNE
#   gather(key = 'year', value =  'value', X1990:X2018) %>%
#   mutate(year = gsub('X', '', year)) %>%
#   mutate(year = year(as.Date(year, format="%Y"))) %>%
#   arrange(year)  %>% ## order by date
#   
#   mutate(value_ks_linear = ifelse(
#     test = (sum(is.na(value)) > (length(iso3_eora)-2)),
#     yes  = value,
#     no   = na_interpolation(x = value, option = 'linear')))
# 
# ### case_when seems not work well for the na_interpolation function
# # mutate(value_ks_linear = case_when(
# #   (sum(is.na(value)) > (length(iso3_eora)-2)) ~ value,
# #   TRUE ~ na_interpolation(x = value, option = 'stine')))
# str(dft)
# 
# 
# 
# df1 <- df %>%
#   filter(iso3_eora == 'AND') %>% ## ROW, ERI, KWT, MNE
#   gather(key = 'year', value =  'value', X1990:X2018) %>%
#   mutate(year = gsub('X', '', year)) %>%
#   mutate(year = year(as.Date(year, format="%Y"))) %>%
#   arrange(year) 
# if (sum(is.na(df1$value)) > (length(df1$iso3_eora)-2)) {
#   df1 <- df1 %>% 
#     mutate(value_ks = value)} else {
#       df1 <- df1 %>% 
#         mutate(value_ks = na_interpolation(x = value, option = 'stine'))}
# 
# 
# sum(is.na(df1$value))
# length(df1$iso3_eora)

