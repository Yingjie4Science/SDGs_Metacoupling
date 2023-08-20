
#' df:          trade matrix
#' direction_i: positive impact or negative
#' frac:        take the top `0.025` as the upper bound

func_net_import_direction <- function(df, in_or_out, direction_i, frac = 0.025){
  
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
    gather(key = 'year', value = 'total_in', 2:ncol(.))
  
  ## to calculate net-import -------------------------------------------------------------
  total_net_in <- merge(x = total_ex, y = total_in, by = c('year', 'iso3')) %>%
    ## define the year
    # dplyr::filter(year == yr) %>%
    dplyr::mutate(
      net_in      = total_in - total_ex,
      net_in_adj  = ifelse(net_in < 0, 0, net_in), ## make an adjustment (is this correct???)
      
    )   %>%
    dplyr::mutate(x = net_in) ## or net_in_adj?
  
  
  ### --> calculate transnational impact score (or spillover score) ----------------------
  ####' There are several ways to normalize data, 
  ####'     including choosing upper and lower bounds
  ####' 0/  cal upper/lower bounds
  max0  = max(total_net_in$x, na.rm = T)
  min0  = min(total_net_in$x, na.rm = T)
  ####' 1/  to use top x%; here `n` in the nth() needs to be a single integer
  max1  = total_net_in$x %>% unlist(.) %>% na.omit(.) %>% sort(decreasing = T) %>% dplyr::nth(n = round(length(.)*frac));
  min1  = total_net_in$x %>% unlist(.) %>% na.omit(.) %>% sort(decreasing = T) %>% dplyr::nth(n = round(length(.)*(1-frac)));
  ####' 2/  to use top 5 and bottom 5, as SDSN did
  # max_  = total_net_in$x %>% unlist(.) %>% na.omit(.) %>% sort(decreasing = T) %>% dplyr::nth(n = 5);
  # min_  = total_net_in$x %>% unlist(.) %>% na.omit(.) %>% sort(decreasing = T) %>% dplyr::nth(n = (length(.)-5));
  
  ####' 3/  The above bounds can scale data to [0, 100], which cannot well present the extent of negatively impacted countries
  ####'   Rather, we need to scale data to [-100, 100], in which positive values (net_imports > 0) indicates impacting others,
  ####'   while negative values indicates being impacted. 
  ####'     here, `min3` is not a min value, but the max of the values in the negative side 
  ####'   For positive values   
  x_positive <- (total_net_in$x)[total_net_in$x>0]
  x_negative <- (total_net_in$x)[total_net_in$x<0]
  max_positive <- x_positive %>% unlist(.) %>% na.omit(.) %>% sort(decreasing = T) %>% dplyr::nth(n = round(length(.)*frac));
  min_positive <- x_positive %>% unlist(.) %>% na.omit(.) %>% sort(decreasing = T) %>% dplyr::nth(n = round(length(.)*(1-frac)));
  max_negative <- x_negative %>% unlist(.) %>% na.omit(.) %>% sort(decreasing = T) %>% dplyr::nth(n = round(length(.)*(1-frac))) %>% abs();
  min_negative <- x_negative %>% unlist(.) %>% na.omit(.) %>% sort(decreasing = T) %>% dplyr::nth(n = round(length(.)*frac)) %>% abs();

  
  
  ### to decide which upper/lower bounds to use for normalization ------------------------
  # max = max1; min = min1

  total_net <- total_net_in %>%
    dplyr::mutate(
      ##' ######################################################################################### #
      ## --> 1. higher value means better performance in generating positive spillovers ------------
      ##' ######################################################################################### #
      # # score = ifelse(test = direction_i == 1, 
      # #                yes = (x-min)/(max-min)*100, 
      # #                no  = (max-x)/(max-min)*100), ## ??? why cannot work in this way?
      # score = case_when(direction_i > 0    ~ (x-min)/(max-min)*100,  ## the larger the larger
      #                   is.na(direction_i) ~ NA_real_,
      #                   TRUE               ~ (max-x)/(max-min)*100), ## the larger the smaller
      
      ##' ######################################################################################### #
      ## --> 2. higher value means worse performance in generating negative spillovers ----
      ##' ######################################################################################### #
      # score = case_when(
      #   ## the larger x, the larger spillover impact
      #   direction_i < 0    ~ (x-min)/(max-min)*100,
      #   is.na(direction_i) ~ NA_real_,
      #   ## the larger x, the smaller spillover impact
      #   TRUE               ~ (max-x)/(max-min)*100),
      # 
      # ## --> to keep score value ranging from 0-100 -----------------------------------------------
      # score = case_when(is.na(score) ~ NA_real_,
      #                   score > 100  ~ 100,
      #                   score > 0    ~ score,
      #                   TRUE         ~ 0),
      
      ##' ######################################################################################### #
      ## --> 3. higher value means worse performance in generating negative spillovers -------------
      ##' ######################################################################################### #
      score = case_when(
        is.na(direction_i) ~ NA_real_,
        ## the larger x, the larger negative impact
        in_or_out == 'in' & direction_i < 0  & x >= 0   ~      (x-min_positive)/(max_positive - min_positive)*100*(-1), # net importer, harm others via bad footprint
        in_or_out == 'in' & direction_i < 0  & x <  0   ~ (abs(x)-min_negative)/(max_negative - min_negative)*100,      # net exporter, not harm others but be harmed
        in_or_out == 'in' & direction_i > 0  & x >= 0   ~      (x-min_positive)/(max_positive - min_positive)*100,      # net importer, promote good footprint
        in_or_out == 'in' & direction_i > 0  & x <  0   ~ (abs(x)-min_negative)/(max_negative - min_negative)*100*(-1), # net exporter, not promote good footprint

        in_or_out == 'out' & direction_i < 0  & x >= 0  ~      (x-min_positive)/(max_positive - min_positive)*100,      # net importer, not harm others
        in_or_out == 'out' & direction_i < 0  & x <  0  ~ (abs(x)-min_negative)/(max_negative - min_negative)*100*(-1), # net exporter, do harm to others
        in_or_out == 'out' & direction_i > 0  & x >= 0  ~      (x-min_positive)/(max_positive - min_positive)*100*(-1), # net importer, not help others
        in_or_out == 'out' & direction_i > 0  & x <  0  ~ (abs(x)-min_negative)/(max_negative - min_negative)*100,      # net exporter, help others

        in_or_out == 'both' & direction_i > 0 & x >= 0  ~      (x-min_positive)/(max_positive - min_positive)*100,  # net exporter, help others
        in_or_out == 'both' & direction_i > 0 & x <  0  ~ (abs(x)-min_negative)/(max_negative - min_negative)*100,  # net exporter, help others

        TRUE ~ 999),
      ###' --> Need to reverse score, so that "positive larger score" indicates `overall larger negative impacts`
      score = case_when(
        is.na(score) ~ NA_real_,
        T ~ -score),
      
      
      ### to keep score value ranging from -100 to 100 -----
      score = case_when(
        is.na(score) ~ NA_real_,
        score >= 100  ~ 100,
        score <= -100 ~ -100,
        TRUE          ~ score)
      
      ) %>%
    arrange(desc(net_in))
  # 
  return(total_net)
}

