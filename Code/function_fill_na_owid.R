

### fill na for owid data

library(imputeTS) ### for na_locf fuction
# library(xlsx)
library(writexl)
library(tidyverse)

fill_na_owid <- function(df) {
  dfs <- data.frame()
  
  n <- length(unique(df$iso3_eora))
  for (i in seq(1:n)) {
  # for (i in seq(1:length(df$iso3_eora))) {
    # print(i) 
    ### loop each country code
    # print(df$iso3_eora[i]) 
    
    df1 <- df %>%
      dplyr::filter(iso3_eora == df$iso3_eora[i]) %>% ## ROW, ERI, KWT, MNE
      # gather(key = 'year', value =  'value', X1990:X2018) %>%
      gather(key = year, value = value, -c(1:3)) %>%
      mutate(year = gsub('X', '', year)) %>%
      mutate(year = year(as.Date(year, format="%Y"))) %>%
      mutate(value = as.numeric(value)) %>%
      arrange(year) # %>% ## order by date
    ### if too many NA, then NA; if less, use interpolation
    if (sum(is.na(df1$value)) >= (length(df1$iso3_eora)-2)) {
      df1 <- df1 %>% 
        mutate(value_ks = value)} 
    else {
      df1 <- df1 %>% 
        mutate(value_ks = na_interpolation(x = value, option = 'stine'))} #
    
    ### ifelse within dplyr does NOT work well 
    # mutate(value_ks = ifelse(
    #   sum(is.na(value)) > (length(iso3_eora)-2), 
    #   value,
    #   na_interpolation(x = value, option = 'stine'))) ## linear, stine
    
    dfs <- rbind(dfs, df1) 
  }
  
  
  
  
  ### plot and examine the interpolation
  # sam <- sample(1:190, size = 20, replace = F) ## replace = repeat
  (plot <- dfs %>%
    ### randomly choose 20 countries and plot
    # slice(sample(1:190, size = 20, replace = F)) %>%
    # slice(1:39*10) %>%
    dplyr::filter(iso3_eora %in% df$iso3_eora[sample(1:190, size = 20, replace = T)]) %>% 
    
    ggplot()+
    geom_line(aes(x = year, y = value),    color = 'green3', size = 6) +
    geom_line(aes(x = year, y = value_ks), color = 'red', size = 2) +
    facet_wrap(~iso3_eora, scales = 'free_y') +
    # xlim(1990, 2005) +
    # ylim(0, 7*10^5)+
    theme_bw())
  #
  
  dfs_update <- dfs %>%
    dplyr::select(-value) %>%
    spread(key = year, value = value_ks) %>%
    left_join(x=ctr_eora, y = ., by=c('country_eora', 'iso3_eora')) ### make sure the same order 
  # fname <- paste0(dir.output, gsub('.csv', '_FILLNA.csv', csv)); print(fname)
  # write.csv(x = dfs_update, file = fname, row.names = F)
  fname <- paste0(dir.process, gsub('.csv', '_OWID_FILLNA.xlsx', basename(csv))); print(fname)
  # write.xlsx(x = dfs_update, file = fname, sheetName = csv, row.names = F, append = T)
  write_xlsx(x = dfs_update, path = fname)
  
  
  return(plot)
}




