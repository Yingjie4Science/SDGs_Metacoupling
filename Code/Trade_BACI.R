

#' this script aims to examine each country's rank on net imports 
#' 
#' 
# t	Year
# k	Product category (HS 6-digit code)
# i	Exporter (ISO 3-digit country code)
# j	Importer (ISO 3-digit country code)
# v	Value of the trade flow (in thousands current USD)
# q	Quantity (in metric tons)

### country code -------------------------------------------------------------------------
csv      <- paste0(dir.baci, 'country_codes_V202102.csv')
baci_ctr <- read_csv(csv) %>% as.data.frame() %>%
  dplyr::select(-country_name_abbreviation, -iso_2digit_alpha) %>%
  dplyr::mutate(country_code = as.character(country_code))

### product code -------------------------------------------------------------------------
json <- 'https://comtrade.un.org/data/cache/classificationH0.json'

library(rjson)
k_code <- fromJSON(file=json)
k_code <- as.data.frame(t(sapply(k_code$results,rbind))) 
names(k_code) <- c('id', 'desc', 'parrent')
k_code_flattened <- apply(k_code,2,as.character) %>%
  as_tibble() %>%
  as.data.frame()
writexl::write_xlsx(x = k_code_flattened, path = paste0(dir.baci, 'product_codes_HS92_fromAPI.xlsx'))
str(k_code_flattened)


f <- paste0(dir.baci, 'BACI_HS92.RData')

load(file = f) ## `bacis`


head(bacis)
str(bacis)


df <- bacis %>%
  as.data.frame() %>%
  dplyr::filter(t == 2015)

# unique(df$k) %>% sort()


df2dgt <- df %>%
  dplyr::mutate(k2 = substr(k, start = 1, stop = 2)) %>%
  group_by(across(c(-k, -v, -q))) %>%
  dplyr::summarise_at(c("v", "q"), sum, na.rm = TRUE) %>%
  as.data.frame() %>%
  select(-v)

k_code2 <- k_code_flattened %>% 
  dplyr::filter(nchar(id) == 2) %>%
  select(-parrent)

df2dgt_w <- df2dgt %>%
  spread(key = 'j', value = 'q')



## Total export -----------------------------------
df <- df2dgt_w
total_ex <- df %>% 
  as.data.frame() %>%
  dplyr::mutate(total_ex = rowSums(.[4:ncol(.)], na.rm = T)) %>%
  dplyr::select(t, i, k2, total_ex)

## Total import -----------------------------------
total_in <- df %>% 
  as.data.frame() %>%
  dplyr::select(-i, -t) %>%
  group_by(k2) %>%
  dplyr::summarise_all(sum, na.rm = TRUE) %>%
  column_to_rownames("k2") %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("i") %>%
  gather(key = 'k2', value = 'total_in', 2:ncol(.))

## to calculate net-import -----------------------
total_net <- merge(x = total_ex, y = total_in, by = c('k2', 'i'), all = T) %>%
  dplyr::mutate(net_in = total_in - total_ex) %>%
  ### add country name info
  left_join(., baci_ctr, by = c('i' = 'country_code')) %>%
  left_join(., k_code2,  by = c('k2' = 'id'))



### rank by net_in 
total_net_rank <- total_net %>%
  arrange(desc(net_in)) %>%
  group_by(k2) %>%
  mutate(rank = rank(-net_in))

total_net_rank_SRB <- total_net_rank %>%
  dplyr::filter(iso_3digit_alpha == 'SRB')




df3 <- df2dgt %>%
  left_join(x = ., 
            y = k_code2, 
            by = c('k2' = 'id')) %>%
  arrange(k2)

# df3 %>% distinct(k2)




f <- paste0('./Data/trade_test/', 'tradereport_2021.txt')
dt <- read.delim(f, header = T, sep = "\t", dec = ".", stringsAsFactors = F) 
unique(dt$Record)
unique(dt$Indicator.Code)

dt1 <- dt %>%
  filter(Indicator.Code == 'I-GHG-CO2') %>%
  # filter(Record %in% c('Imports', 'Exports')) %>%
  spread(key = Record, value = Value) %>%
  mutate(Imports.net = Imports - Exports) %>%
  arrange(desc(Imports.net))


# library(R.matlab)
# # read in our data
# f <- paste0('./Data/trade_test/', 'bilateraltrade.mat') ## too large to open
# mat <- readMat(f)
