



helper_func_plot_map <- function(data, var = 'value_rel', yr = 2015, filename_postfix = '') {
  
  ## palette for fill color
  if (direction_sdg == -1) {
    pal <- '-RdYlBu'
  } else {
    pal <- 'RdYlBu'
  }
  
  rel_not_sf  <- data %>%
    dplyr::filter(year == yr) %>%
    merge(x = shp, 
          y = ., 
          by.x = "iso_a3", by.y = "iso3",  
          all.x = T)
  
  max <- max(rel_not_sf$value_rel, na.rm = T); max # 0.025
  min <- min(rel_not_sf$value_rel, na.rm = T); min # 0.000001094
  
  p <- tm_shape(rel_not_sf) +
    tm_fill(col = var, style  = "quantile", n = 10, textNA = 'NA', 
            palette = pal,
            colorNA = 'gray90') +
    tm_borders(col = "grey", lwd = 0.1, lty = "solid", alpha = 0.99) +
    tm_layout(frame = F, frame.lwd = 0.1,
              legend.position = c(0,0),
              legend.title.size = 0.9, legend.text.size  = 0.7,
              legend.width = -0.5, legend.height = -0.5, outer.margins=0, inner.margins=0,
              panel.show = F)
  fname <- paste0(dir.fig, 'Test_', filename_postfix, '_', var, '.jpg'); 
  print(fname)
  tmap_save(tm = p, filename = fname, width=7, height=2.8, units="in", dpi = 200)
  
  
}
