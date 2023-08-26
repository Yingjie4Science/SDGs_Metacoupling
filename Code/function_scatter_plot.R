
#'
#'
#'

function_scatter_plot <- function(data, x, y, log.xy = FALSE){
  p <- data %>%
    ggplot(aes(.data[[x]], y = .data[[y]])) +
    geom_point() +
    geom_abline(slope = 1, color = 'red')+
    theme_bw()
  if(log.xy==TRUE){
    p <- p + 
      scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x))) +
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x)))
  } else {
    p <- p
  }
  return(p)
}
