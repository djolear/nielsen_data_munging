
###################
## Load Packages ##
###################

library("tidyverse", lib.loc = "/home/djolear/Rpackages")
library("haven", lib.loc = "/home/djolear/Rpackages")
library("lubridate", lib.loc = "/home/djolear/Rpackages")
library("doParallel", lib.loc = "/home/djolear/Rpackages")
library("foreach", lib.loc = "/home/djolear/Rpackages")

###############
## Functions ##
###############

'%!in%' <- function(x,y)!('%in%'(x,y))


###############
## Load Data ##
###############

products_master <-
  readr::read_tsv("/kilts/nielsen_extracts/HMS/Master_Files/Latest/products.tsv")


################
## Munge Data ##
################

# Select appropriate product types
products_master <-
  products_master %>% 
  filter(department_descr %!in% c("HEALTH & BEAUTY CARE", "NON-FOOD GROCERY", "GENERAL MERCHANDISE", "MAGNET DATA")) %>% 
  filter(size1_units %in% c("CT", "OZ", "PO", "LI", "QT", "ML"))

# Convert UPCs
products_master <-
  products_master %>% 
  mutate(
    first_val = as.numeric(str_sub(upc, 12, 12)) * 3 + as.numeric(str_sub(upc, 10, 10)) * 3 + 
      as.numeric(str_sub(upc, 8, 8)) * 3 + as.numeric(str_sub(upc, 6, 6)) * 3 + 
      as.numeric(str_sub(upc, 4, 4)) * 3 + as.numeric(str_sub(upc, 2, 2)) * 3 +
      as.numeric(str_sub(upc, 11, 11)) + as.numeric(str_sub(upc, 9, 9)) +
      as.numeric(str_sub(upc, 7, 7)) + as.numeric(str_sub(upc, 5, 5)) +
      as.numeric(str_sub(upc, 3, 3)) + as.numeric(str_sub(upc, 1, 1)),
    
    mod_val = first_val %% 10,
    
    check_digit = ifelse(mod_val != 10, 10 - mod_val, 0),
    
    upc_new = paste0(upc, as.character(check_digit))
  )

products_master <-
  products_master %>% 
  dplyr::select(-c(first_val, mod_val, check_digit))

# Load Label Insights data
label_insights <-
  read_csv("/project/ourminsk/nielsen/data/label_insights_servings_converted_calories_only_031721.csv") %>% 
  mutate(calories_sc = calories)

# Join Label Insights data to Nielsen product master file
products_master <-
  products_master %>% 
  left_join(
    label_insights %>%
      dplyr::select(
        upc_new,
        calories_sc
      ),
    by = "upc_new"
  )

# Convert pounds to ounces
products_master <-
  products_master %>% 
  mutate(
    size_new =
      case_when(
        size1_units == "PO" ~ size1_amount * 16,
        size1_units == "LI" ~ size1_amount * 16 * 2.2,
        size1_units == "ML" ~ (size1_amount*16*2.2)/1000,
        size1_units == "QT" ~ size1_amount *16*2.08,
        TRUE ~ size1_amount
      ),
    units_new =
      case_when(
        size1_units == "PO" ~ "OZ",
        size1_units == "LI" ~ "OZ",
        size1_units == "ML" ~ "OZ",
        size1_units == "QT" ~ "OZ",
        TRUE ~ size1_units
      )
  )

# Calculate calories_sc per unit for the products that we have calories_sc for
calories_sc_per_unit <- 
  products_master %>% 
  filter(
   !is.na(calories_sc)
  ) %>% 
  mutate(
    calories_sc_per_unit = calories_sc / size_new 
  ) %>% 
  group_by(product_module_descr, units_new) %>% 
  summarise(
    mean_cal = mean(calories_sc_per_unit, na.rm = TRUE),
    median_cal = median(calories_sc_per_unit, na.rm = TRUE),
    sd = sd(calories_sc_per_unit, na.rm = TRUE)
  ) 


# Add in data about how many products in each cell we have data for
calories_sc_per_unit <- 
  calories_sc_per_unit %>% 
  left_join(
    products_master %>% 
      filter(
        !is.na(calories_sc)
      ) %>% 
      count(product_module_descr, units_new)
  )

# Use only products that have 6 or more instances
calories_sc_per_unit <-
  calories_sc_per_unit %>% 
  filter(n > 5)

# Join imputed data
products_master <-
  products_master %>% 
  left_join(
    calories_sc_per_unit %>% 
      dplyr::select(
        product_module_descr,
        units_new,
        median_cal
      ) %>% 
      ungroup()
  )

products_master <-
  products_master %>% 
  mutate(
    calories_sc_imp = 
      case_when(
        !is.na(calories_sc) == TRUE ~ calories_sc,
        is.na(calories_sc) == TRUE ~ median_cal * size_new
      )
  ) 

products_master %>% count(is.na(calories_sc))
products_master %>% count(is.na(calories_sc_imp))

write_csv(products_master, "/project/ourminsk/nielsen/data/products_master_imputed_calories_servings_conversion.csv")


