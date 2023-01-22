

### packages

## data package
library(wbstats)
library(FAOSTAT)
## https://github.com/expersso/WHO
# From Github
# library(devtools)
# install_github("expersso/WHO")
library(WHO)


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
conflict_prefer("filter", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("print", "base")
conflict_prefer("save", "base")



