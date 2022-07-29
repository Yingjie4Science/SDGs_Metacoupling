

getwd()


df <- readr::read_csv('comtrade_SITC3.csv') %>%
  dplyr::select(Year, `Trade Flow`, Reporter, `Reporter ISO`, Partner, Commodity, `Trade Value (US$)`) %>%
  spread(key = `Trade Flow`, value = `Trade Value (US$)`) %>%
  dplyr::mutate(netin = Import - Export)


