
## *A function for normalization*
# Put all four scenarios together and normalize to SDG score 0-100

#' the winsor() function caps the tails of the distribution based on the trim parameter. 
#'    A trim value of 0.05 means that the lowest 5% and the highest 5% of the values will be 
#'    replaced with the corresponding percentiles
#'    
#'    


#' There are several *normalization methods*, including Min-Max Scaling, 
#'    Z-Score (Standard Score) Normalization, and Robust Scaling. 
#'    Let's discuss how each of these methods can affect extreme values:
#'  
#' Min-Max Scaling: This method scales the data to a specific range, often between 0 and 1. 
#'    It doesn't inherently reduce extreme values, but it can bring the values of extreme data 
#'    points closer to the range of the other values. However, if your dataset has outliers (extreme values), 
#'    they might still remain relatively extreme after Min-Max Scaling.
#'    
#' Z-Score Normalization: Z-Score normalization standardizes the data by transforming it 
#'    into a standard normal distribution with mean 0 and standard deviation 1. It helps 
#'    center the data around its mean, which might bring extreme values closer to the center. 
#'    However, extreme values that are several standard deviations away from the mean might 
#'    still remain relatively extreme.
#'    
#' Robust Scaling: Robust Scaling (also known as Min-Max Scaling with robust statistics) 
#'    scales the data by subtracting the median and then dividing by the interquartile range (IQR). 
#'    This method is less affected by outliers and can help reduce the impact of extreme values 
#'    on the scaling process.
#'    


## Custom Robust Scaling function with NA handling ---------------------------------------
robust_scale <- function(x) {
  median_val <- median(x, na.rm = TRUE)
  iqr_val <- IQR(x, na.rm = TRUE)
  scaled_val <- (x - median_val) / iqr_val
  scaled_val
}


library(stats)

func_norm_sdg_score_good <- function(df, trim = 0.025, bottom = 0.05, top = 0.95){
  
  # put all data together, and ----------------------------------------------------------#
  # find min and max for further normalization
  all.val <- df %>% 
    # dplyr::select(year, iso3, value_rel, value_not) %>%
    gather(key = 'scenario', value = 'val', value_rel:ncol(.)) %>%
    dplyr::mutate(val1 = winsor(val, trim = trim, na.rm = TRUE)) %>%
    as.data.frame()

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






func_norm_sdg_score_bad <- function(df, trim = 0.025, bottom = 0.05, top = 0.95){
  
  # put all data together, and ---------------------------------------------------------- #
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
func_norm_sdg_score_auto <- function(df, trim = 0.025, bottom = 0.05, top = 0.95, direction){
  
  ##' put all data together for further normalization ---------------------------------- #
  all.val <- df %>% 
    # dplyr::select(year, iso3, value_rel, value_not) %>%
    gather(key = 'scenario', value = 'val', value_rel:ncol(.)) %>%
    # dplyr::mutate(val = winsor(val, trim = trim, na.rm = TRUE)) %>% ## may not use now
    as.data.frame()
  
  ##' deal with extreme values --------------------------------------------------------- #
  ##'   before using *Min-Max Scaling* method to scale the data to a specific range (0-100), 
  ##'   we first use *Robust Scaling* to reduce the impact of extreme values on the scaling process.
  all.val_robust <- all.val %>%
    mutate_at(vars('val'), 
              ~ robust_scale(.))
  
  all.val_z <- all.val %>%
    mutate_at(vars('val'), 
              ~ scale(., center = TRUE, scale = TRUE))
  
  # hist(all.val$val,        breaks = 100, main = 'raw')
  # hist(all.val_robust$val, breaks = 200, main = 'robust')
  # hist(all.val_z$val,      breaks = 100, main = 'z')
  
  ##' decide which result to use ------------------------------------------------------- #
  all.val <- all.val_z ## Z-Score Normalization is better than robust scaling after testing
    
  
  ##' Scale the data to a range of 0-100 ----------------------------------------------- #
  # quantile(all.val$value, probs = c(0.025, 0.975))
  min <- quantile(all.val$val, probs = bottom, na.rm = T); min
  # min <- ifelse(min < 0, 0, min);                          min
  max <- quantile(all.val$val, probs = top,    na.rm = T); max
  
  
  ##' would be a better idea to keep the score between 1-100, because 0 can lead to Inf 
  ##'     in the further analysis steps
  if (direction_sdg == -1) {
    all.val.norm <- all.val %>%
      dplyr::mutate(
        value_norm = ifelse(
          val > max, 0+1, ifelse(
            val > min, ((val-max)/(min-max)*100+1), 100+1))
      )
  } else if (direction_sdg == 1) {
    all.val.norm <- all.val %>%
      dplyr::mutate(
        value_norm = ifelse(
          val < min, 0+1, ifelse(
            val < max, ((val-min)/(max-min)*100+1), 100+1))
      )
  } else {
    print('\t\t Please specify `direction_sdg` for SDG score normalization! ...')
  }
  
  
  ## set max = 100
  all.val.norm <- all.val.norm %>%
    dplyr::mutate(value_norm = ifelse(value_norm>100, 100, value_norm))
  
  return(all.val.norm)
}
