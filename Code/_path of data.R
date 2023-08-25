

#' This script lists all the file path for this project
#' It helps better navigate across multiple folders while keep a nice records of path changes. 
#' 
#' Please use the following two lines to call this script
#' 


# ### data path
# source('./Code/_path of data.R')
# dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
# dir.root <- dirname(dir)
dir.root <- "D:/_papers/SDGs/SDGs_Metacouplings_private/"

## raw data ------------------------------------------------------------------------------
dir.raw          <- './Data/data_01_raw/'
dir.arm          <- './Data/data_01_raw/Arm trade_Peacekeeping/SIPRI Arms trade/'
dir.peace        <- './Data/data_01_raw/Arm trade_Peacekeeping/UN peacekeeping operations/'
dir.traffick     <- './Data/data_01_raw/Human trafficking/'
dir.student_flow <- './Data/data_01_raw/International Student flows/'
dir.oecd         <- './Data/data_01_raw/OECD/'
dir.imf          <- './Data/data_01_raw/FDI/IMF/'



#
# dir.fao  <- paste0(dir.raw, 'FAOSTAT/')  
# dir.fish <- paste0(dir.raw, 'FAOSTAT/Jessica Gephart/')
dir.fao   <- paste0('D:/data/', 'FAOSTAT/') ## move to local drive to save space
dir.fish  <- paste0('D:/data/', 'FAOSTAT/Jessica Gephart/')





## intermediate data ❗❗ ------------------------------------------------------------------
dir.process      <- paste0(dir.root, '/Data/data_02_intermediate/dt01_ctr_profile/xlsx/')
dir.cleaned      <- paste0(dir.root, '/Data/data_02_intermediate/dt01_ctr_profile/xlsx_cleaned/')

### EORA data by Mengyu, 
  
  #### 🟡 - updated on 8/5/2021; used in YSSP and dissertation 
  # postfix  <- '20210805yssp'
  # dir.eora         <- './Data/data_02_intermediate/dt02_flows/eora/2021-08-05/'
  
  #### 🟡 - updated on 8/12/2022; consumption-based calculation; 
  # postfix  <- '20220812con' 
  # dir.eora <- './Data/data_02_intermediate/dt02_flows/eora/2022-08-05con/' 
  
  #### 🟡 - updated on 8/12/2022; production-based calculation;  updated on 8/12/2022
  # postfix  <- '20220812pro'
  # dir.eora <- './Data/data_02_intermediate/dt02_flows/eora/2022-08-05pro/' ## updated on 8/12/2022
  # dir.eora <- './Data/data_02_intermediate/dt02_flows/eora/2023-08-16pro/' ## updated on 8/16/2023; only three energy-related files
  # dir.eora <- './Data/data_02_intermediate/dt02_flows/eora/2023-08-17con/' ## updated on 8/17/2023; all files + 1 materiel + 135 shdb
  # dir.shdb <- './Data/data_02_intermediate/dt02_flows/eora/2023-08-17_SHDB2019_135/'
  
  postfix  <- '20230821con'
  dir.eora <- './Data/data_02_intermediate/dt02_flows/eora/2023-08-21con/' ## updated on 8/20/2023; address the data issue of less than *10 times
  dir.shdb <- './Data/data_02_intermediate/dt02_flows/eora/2023-08-21con/' ## keep all data in the same folder



dir.eora_cleaned <- './Data/data_02_intermediate/dt02_flows/eora_cleaned/'
dir.flowScenario <- './Data/data_02_intermediate/dt02_flows/'


## figures -------------------------------------------------------------------------------
dir.fig          <- './Figure/'









