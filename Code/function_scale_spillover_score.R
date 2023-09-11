



function_scale_spillover_score <- function(df, direction_i, in_or_out, 
                                           max_positive, min_positive,
                                           max_negative = 1, min_negative = 0
    ) {

  df2 <- df %>%
    dplyr::mutate(
      score = case_when(
        is.na(direction_i)                              ~ NA_real_,
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
)
  
  return(df2)
}



