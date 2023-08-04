
## make sure R session defaults to English
Sys.setenv(LANG = "en")

### packages

## tidy data
library(readr)
# library(xlsx)
library(readxl)
library(writexl)
# library(XLConnect)
library(tidyverse)
library(reshape2)  ## expand long table to wide one
library(stringr)
library(scales)
library(tm)        ## clean text
library(lubridate)
library(viridis)
library(psych)


## data describe 
library(summarytools)



## shapefile data
library(rnaturalearthdata)
library(rnaturalearth)
library(sf)



### plot #####
library(cowplot)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
## Color Brewer palette
library(viridis)
# install.packages("hrbrthemes")
# library(hrbrthemes)
# library(devtools)
library(circlize)  ## for circlize plot
library(Cairo)

library(conflicted)
conflicts_prefer(
  dplyr::arrange,
  dplyr::desc,
  dplyr::rename,
  dplyr::filter, 
  dplyr::mutate, 
  dplyr::select, 
  .quiet = T
)
conflict_prefer("print", "base")
conflict_prefer("save", "base")



