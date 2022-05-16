


### ------------------------------------------------------------------------- ###
### This function is used to calculate the net imports of footprints
### ------------------------------------------------------------------------- ###


# functions 
Net_Import_Cal <- function(df.xlsx, matrix) {
  ### trade matrix
  s1 <- read_excel(df.xlsx, sheet = 1, col_names = T) %>%
    gather(key = 'd', value = 'value', 4:ncol(.)) %>%
    dplyr::group_by(year) %>%
    arrange(year, iso3, d) %>%
    spread(key = 'd', value = 'value') %>%
    arrange(year, iso3) 
  
  yrs  <- unique(s1$year); yrs
  n_yr <- length(yrs); n_yr
  
  s1 <- s1 %>%
    ungroup() %>%
    dplyr::select(-year, -ctr, -iso3)
  
  ### trade matrix distant or nearby
  s2 <- matrix %>%
    dplyr::bind_rows(replicate(n_yr-1, ., simplify = F))
  
  
  s3 <- s1 * s2
  
 
  ### add country name as a new col
  countries <- colnames(s2)
  # colnames(s3) <- countries; length(countries)
  
  ### add year seq to data frame 
  year_seq <- rep(yrs, each = ncol(s2))
  s3$year  <- year_seq
  
  ### total imports -------------------------------------------
  s4 <- s3 %>% group_by(year) %>% dplyr::summarise_all(.funs = sum)
  ### transpose
  s5 <- as.data.frame(t(s4[,-1]))
  colnames(s5) <- yrs
  s5 <- s5[order(row.names(s5)),]
  
  ### total exports -------------------------------------------
  s6 <- s3 %>% 
    as.data.frame() %>%
    dplyr::mutate(row.sum   = rowSums(.[1:(ncol(.)-1)], na.rm = T),
                  year      = year_seq,
                  countries = rep(countries, times = n_yr)) %>% 
    dcast(countries ~ year, value.var = 'row.sum')  %>% # year as new col names
    column_to_rownames(var = "countries")
  
  s6 <- s6[order(row.names(s6)), ]
  
  ### net imports ---------------------------------------------
  s7 <- s5 - s6
  
  return(s7)
}