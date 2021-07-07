###################
## Load Packages ##
###################

library(tidyverse)

###############
## Load Data ##
###############

label_insights <-
  read_csv("G:/My Drive/research/projects/niel/label_insights_data/label_insights_031721.csv")


########################
## Run UPC Conversion ##
########################

source("G:/My Drive/research/projects/niel/nielsen_data_munging/label_munging/label_insights/label_insights_convert_upcs.R")


#############################
## Run Servings Conversion ##
#############################

source("G:/My Drive/research/projects/niel/nielsen_data_munging/label_munging/label_insights/label_insights_servings_conversion.R")


################
## Write Data ##
################

# Write data with all columns
write_csv(label_insights_servings_converted, "G:/My Drive/research/projects/niel/label_insights_data/label_insights_servings_converted_031721.csv")

# Select and write data with only calorie information
label_insights_servings_converted_calories <-
  label_insights_servings_converted %>% 
  dplyr::select(
    UPC,
    upc_new,
    Calories
  )

write_csv(label_insights_servings_converted_calories, "G:/My Drive/research/projects/niel/label_insights_data/label_insights_servings_converted_calories_only_031721.csv")


