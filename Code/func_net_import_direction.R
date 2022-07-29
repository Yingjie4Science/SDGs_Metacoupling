
#' df:        trade matrix
#' direction: positive impact or negative
#' frac:      take the top `0.025` as the upper bound

func_net_import_direction <- function(df, direction, frac){
  
  ## Total export -----------------------------------
  total_ex <- df %>% 
    as.data.frame() %>%
    dplyr::mutate(total_ex = rowSums(.[4:ncol(.)], na.rm = T)) %>%
    dplyr::select(year, iso3, total_ex)
  
  ## Total import -----------------------------------
  total_in <- df %>% 
    as.data.frame() %>%
    dplyr::select(-iso3, -ctr) %>%
    group_by(year) %>%
    dplyr::summarise_all(sum, na.rm = TRUE) %>%
    column_to_rownames("year") %>% 
    t() %>%
    as.data.frame() %>%
    rownames_to_column("iso3") %>%
    gather(key = 'year', value = 'total_in', `2000`:ncol(.))
  
  ## to calculate net-import -----------------------
  total_net <- merge(x = total_ex, y = total_in, by = c('year', 'iso3')) %>%
    ## define the year
    # dplyr::filter(year == yr) %>%
    dplyr::mutate(
      net_in      = total_in - total_ex,
      net_in_adj  = ifelse(net_in < 0, 0, net_in), ## make an adjustment (is this correct???)
      x           = net_in, ## or net_in_adj?
      
      ### cal upper/lower bounds
      max0  = max(x, na.rm = T),
      min0  = min(x, na.rm = T),
      max1  = x %>% unlist() %>% na.omit() %>% sort(decreasing = T) %>% dplyr::nth(n = length(.)*frac),
      min1  = x %>% unlist() %>% na.omit() %>% sort(decreasing = T) %>% dplyr::nth(n = length(.)*(1-frac)),
      
      ### to decide which upper/lower bounds to use
      max = max1,
      min = min1,
      
      ## --> 1. higher value means better performance in generating positive spillovers ------------
      # # score = ifelse(test = direction == 1, 
      # #                yes = (x-min)/(max-min)*100, 
      # #                no  = (max-x)/(max-min)*100), ## ??? why cannot work in this way?
      # score = case_when(direction > 0    ~ (x-min)/(max-min)*100,  ## the larger the larger
      #                   is.na(direction) ~ NA_real_,
      #                   TRUE             ~ (max-x)/(max-min)*100), ## the larger the smaller
      
      ## --> 2. higher value means worse performance in generating negative spillovers -------------
      score = case_when(direction < 0    ~ (x-min)/(max-min)*100,  ## the larger the larger
                        is.na(direction) ~ NA_real_,
                        TRUE             ~ (max-x)/(max-min)*100), ## the larger the smaller
      
      ## --> to keep scaore value ranging from 0-100 -----------------------------------------------
      score = case_when(is.na(score) ~ NA_real_,
                        score > 100  ~ 100,
                        score > 0    ~ score,
                        TRUE         ~ 0)
    ) %>%
    arrange(desc(net_in))
  # 
  return(total_net)
}

