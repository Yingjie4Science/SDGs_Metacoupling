
## *A function for normalization*
# Put all four scenarios together and normalize to SDG score 0-100

library(stats)

func_norm_sdg_score_good <- function(df, trim, bottom, top){
  
  # put all data together, and --------------------------------------------------------------------------------------
  # find min and max for further normalization
  all.val <- df %>% 
    # dplyr::select(year, iso3, value_rel, value_not) %>%
    gather(key = 'scenario', value = 'val', value_rel:ncol(.)) %>%
    dplyr::mutate(val = winsor(val, trim = trim, na.rm = TRUE))

  # quantile(all.val$value, probs = c(0.025, 0.975))
  min <- quantile(all.val$val, probs = bottom, na.rm = T); min
  # min <- ifelse(min < 0, 0, min);                          min
  max <- quantile(all.val$val, probs = top,    na.rm = T); max

  all.val.norm <- all.val %>%
    dplyr::mutate(
      value_norm = ifelse(
        val < min, 0, ifelse(
          val < max, (val-min)/(max-min)*100, 100))
        )

  return(all.val.norm)
}






func_norm_sdg_score_bad <- function(df, trim, bottom, top){
  
  # put all data together, and --------------------------------------------------------------------------------------
  # find min and max for further normalization
  all.val <- df %>% 
    # dplyr::select(year, iso3, value_rel, value_not) %>%
    gather(key = 'scenario', value = 'val', value_rel:ncol(.)) %>%
    dplyr::mutate(val = winsor(val, trim = trim, na.rm = TRUE))
  
  # quantile(all.val$value, probs = c(0.025, 0.975))
  min <- quantile(all.val$val, probs = bottom, na.rm = T); min
  # min <- ifelse(min < 0, 0, min);                          min
  max <- quantile(all.val$val, probs = top,    na.rm = T); max
  
  all.val.norm <- all.val %>%
    dplyr::mutate(
      value_norm = ifelse(
        val > max, 0, ifelse(
          val > min, (val-max)/(min-max)*100, 100))
    )

  return(all.val.norm)
}



### advanced version 
func_norm_sdg_score_auto <- function(df, trim, bottom, top, direction){
  
  # put all data together, and --------------------------------------------------------------------------------------
  # find min and max for further normalization
  all.val <- df %>% 
    # dplyr::select(year, iso3, value_rel, value_not) %>%
    gather(key = 'scenario', value = 'val', value_rel:ncol(.)) %>%
    dplyr::mutate(val = winsor(val, trim = trim, na.rm = TRUE))
  
  # quantile(all.val$value, probs = c(0.025, 0.975))
  min <- quantile(all.val$val, probs = bottom, na.rm = T); min
  # min <- ifelse(min < 0, 0, min);                          min
  max <- quantile(all.val$val, probs = top,    na.rm = T); max
  
  
  if (direction_sdg == -1) {
    all.val.norm <- all.val %>%
      dplyr::mutate(
        value_norm = ifelse(
          val > max, 0, ifelse(
            val > min, (val-max)/(min-max)*100, 100))
      )
  } else if (direction_sdg == 1) {
    all.val.norm <- all.val %>%
      dplyr::mutate(
        value_norm = ifelse(
          val < min, 0, ifelse(
            val < max, (val-min)/(max-min)*100, 100))
      )
  } else {
    print('\t\t Please specify `direction_sdg` for SDG score normalization! ...')
  }
  
  return(all.val.norm)
}
