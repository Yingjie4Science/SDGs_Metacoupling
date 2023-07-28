

### fill na function for wb data

library(imputeTS) ### for na_locf fuction
# library(xlsx)
library(writexl)
library(tidyverse)
library(lubridate)


function_fill_na <- function(df, year_from = 1990) {
  dfss <- data.frame()
  
  n <- length(unique(df$iso3))
  for (i in seq(1:n)) {
    # print(i) 
    # ### loop each country code
    # print(df$iso3[i]) 
    
    df1 <- df %>%
      dplyr::filter(iso3 == df$iso3[i]) %>% ## ROW, ERI, KWT, MNE
      gather(key = 'year', value =  'value', 3:ncol(.)) %>%
      dplyr::mutate(year = gsub('X', '', year)) %>%
      dplyr::mutate(year = year(as.Date(year, format="%Y"))) %>%
      ## filter out old years, due to many NA in earlier years
      dplyr::filter(year >= year_from) %>%
      dplyr::mutate(value = as.numeric(value)) %>%
      arrange(year) # %>% ## order by date
   
    
    ### if too many NA, then NA; if less, use interpolation
    # if (sum(is.na(df1$value)) > (length(df1$iso3)-2)) {  ## Input data needs at least 2 non-NA data point for applying na_interpolation
    #   df1 <- df1 %>% 
    #     mutate(value_ks = value)} 
    # else {
    #   df1 <- df1 %>% 
    #     mutate(value_ks = na_interpolation(x = value, option = 'stine'))} 
    
    
    if (sum(is.na(df1$value)) > 3) {  ## Input data needs at least 2 non-NA data point for applying na_interpolation
      df1 <- df1 %>% 
        dplyr::mutate(value_ks = value)} 
    else if (sum(is.na(df1$value)) > 2) {
      df1 <- df1 %>% 
        dplyr::mutate(value_ks = sum(.$value, na.rm = T))}
    else {
      df1 <- df1 %>% 
        dplyr::mutate(value_ks = na_interpolation(x = value, option = 'stine'))} 
    
    dfss <- rbind(dfss, df1)
  }
  
  
  ### plot and examine the interpolation
  # sam <- sample(1:190, size = 20, replace = F) ## replace = repeat
  plot <- dfss %>%
    ### randomly choose 20 countries and plot
    # slice(sample(1:190, size = 20, replace = F)) %>%
    # slice(1:39*10) %>%
    dplyr::filter(iso3 %in% df$iso3[sample(1:190, size = 20, replace = T)]) %>% 
    
    ggplot()+
    geom_line(aes(x = year, y = value),    color = 'blue', size = 6) +
    geom_line(aes(x = year, y = value_ks), color = 'red', size = 2) +
    facet_wrap(~iso3, scales = 'free_y') +
    # xlim(1990, 2005) +
    # ylim(0, 7*10^5)+
    theme_bw()
  #
  
  dfss_update <- dfss %>%
    dplyr::select(-value) %>%
    spread(key = year, value = value_ks) %>%
    dplyr::select(iso3, ctr, `2000`, `2005`, `2010`, `2015`) %>%
    arrange(iso3)
  fname <- paste0(dir.cleaned, iname, '_fillna_4yrs.xlsx'); print(fname)
  write_xlsx(x = dfss_update, path = fname)
  return(plot)
}


