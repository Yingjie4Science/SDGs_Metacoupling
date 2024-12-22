![Visitor Badge](https://visitor-badge.laobi.icu/badge?page_id=yingjieli.visitor-badge)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)



## Software requirements

The R scripts were prepared using *R version 4.3.2* on Windows 11 Pro.

The following R packages were used in this analysis:

*This project does not involve any non-standard hardware.* 
```
library(readr)
library(readxl)
library(writexl)
library(tidyverse)
library(reshape2) 
library(stringr)
library(scales)
library(tm)
library(lubridate)
library(viridis)
library(summarytools)
library(rnaturalearthdata)
library(rnaturalearth)
library(sf)
library(cowplot)
library(ggplot2)
library(tmap)
library(ggpubr)
library(RColorBrewer)
```


## Data

All the data that support the findings of this study are publicly available. 

Please refer to Supplementary Table 1 and Supplementary Table 2 for detailed data source information. 


## Code

All analysis code are deposited in the folder `Code`.

*Users can refer to the detailed annotations in the main scripts below to execute the code.*

### data preprocessing
```
05_AncillaryData_ISO3Code_shp.Rmd
05a_helper_capital cities.R
10_country_profileData_prepare.Rmd
11_Comtrade.Rmd
11_Comtrade_BACI_Pesticides.Rmd
11_FAO_FishStatJ.R
21_00_unify_format_before_MRIO_data_clean.Rmd
21_MRIO_data_clean.Rmd
22_09_prep_direction_data.Rmd
```

### scenario analysis & SDG score calculation
```
22_10_TNI_rel_not.Rmd ------------------ Figures 2a, 2b, Ex4, Ex5
22_20_TNI_dst_ner.Rmd  
22_22_FootprintDistance.Rmd ------------ Figures 2d, Ex6
25_TNI_on_SDGs_Data.Rmd
```

## visualize result 
```
26_TNI_on_SDGs_Viz.Rmd ----------------- Figures 1a, 1b, 1c, 1d, 3a, 3b, Ex1, Ex2, Ex3, 
27a1_TNI_Networks_data.Rmd
27a2_TNI_Networks_plot.Rmd
27b_TNI_Networks_map.Rmd --------------- Figure 2c
```


## Contact
yingjieli DOT edu AT gmail DOT com
