

func_net_import <- function(df, yr){
  
  ## Total export
  total_ex <- df %>% 
    as.data.frame() %>%
    dplyr::mutate(total_ex = rowSums(.[4:ncol(.)], na.rm = T)) %>%
    dplyr::select(year, iso3, total_ex)
  
  ## Total import
  total_in <- df %>% 
    as.data.frame() %>%
    dplyr::select(-iso3, -ctr) %>%
    group_by(year) %>%
    summarise_all(sum, na.rm = TRUE) %>%
    column_to_rownames("year") %>% 
    t() %>%
    as.data.frame() %>%
    rownames_to_column("iso3") %>%
    gather(key = 'year', value = 'total_in', `2000`:ncol(.))
  
  ## to calculate net-import
  total_net <- merge(x = total_ex, y = total_in, by = c('year', 'iso3')) %>%
    ## define the year
    # dplyr::filter(year == yr) %>%
    dplyr::mutate(net_in      = total_in - total_ex,
                  net_in_adj  = ifelse(net_in < 0, 0, net_in), ## make an adjustment (is this correct???)
                  max         = max(net_in,     na.rm = T),
                  min         = min(net_in_adj, na.rm = T),
                  score       = (max- net_in_adj)/(max - min) * 100
    ) %>%
    arrange(desc(net_in))
  
  return(total_net)
}
