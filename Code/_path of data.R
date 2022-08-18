

#' This script lists all the file path for this project
#' It helps better navigate across multiple folders while keep a nice records of path changes. 
#' 
#' Please use the following two lines to call this script
#' 


# ### data path
# source('./Code/_path of data.R')


## raw data ------------------------------------------------------------------------------
dir.raw          <- './Data/data_01_raw/'
dir.arm          <- './Data/data_01_raw/Arm trade_Peacekeeping/SIPRI Arms trade/'
dir.traffick     <- './Data/data_01_raw/Human trafficking/'
dir.student_flow <- './Data/data_01_raw/International Student flows/'


#
# dir.fao  <- paste0(dir.raw, 'FAOSTAT/')  
# dir.fish <- paste0(dir.raw, 'FAOSTAT/Jessica Gephart/')
dir.fao   <- paste0('D:/data/', 'FAOSTAT/') ## move to local drive to save space
dir.fish  <- paste0('D:/data/', 'FAOSTAT/Jessica Gephart/')




## intermediate data ---------------------------------------------------------------------
dir.process      <- './Data/data_02_intermediate/dt01_ctr_profile/xlsx/'
dir.cleaned      <- './Data/data_02_intermediate/dt01_ctr_profile/xlsx/cleaned/'

### EORA data by Mengyu, 
  # dir.eora         <- './Data/data_02_intermediate/dt02_flows/eora/'
  
  #### - consumption-based calculation, ## updated on 8/12/2022
  # postfix  <- '20220812con' 
  # dir.eora <- './Data/data_02_intermediate/dt02_flows/eora/2022-08-05con/' 
  
  #### - production-based calculation
  postfix  <- '20220812pro'
  dir.eora <- './Data/data_02_intermediate/dt02_flows/eora/2022-08-05pro/' ## updated on 8/12/2022



dir.eora_cleaned <- './Data/data_02_intermediate/dt02_flows/eora_cleaned/'
dir.flowScenario <- './Data/data_02_intermediate/dt02_flows/'


## figures -------------------------------------------------------------------------------
dir.fig          <- './Data/Figure/'









