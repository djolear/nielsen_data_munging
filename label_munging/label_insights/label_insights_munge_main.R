library(tidyverse)

li_upc <-
  read_csv("G:/My Drive/research/projects/niel/label_insights_data/label_insights_upc_raw_021721.csv")

source("G:/My Drive/research/projects/niel/label_insights_analysis/label_insights_servings_conversion.R")

write_csv(li_upc_update, "G:/My Drive/research/projects/niel/label_insights_data/label_insights_upc_sc_021721.csv")


li_upc_cal_only <-
  li_upc %>% 
  dplyr::select(
    UPC,
    Calories
  )

write_csv(li_upc_cal_only, "G:/My Drive/research/projects/niel/label_insights_data/label_insights_upc_raw_cal_only_021721.csv")


li_upc_sc_cal_only <-
  li_upc_update %>% 
  dplyr::select(
    UPC,
    calories_sc
  )

write_csv(li_upc_sc_cal_only, "G:/My Drive/research/projects/niel/label_insights_data/label_insights_upc_sc_cal_only_021721.csv")
