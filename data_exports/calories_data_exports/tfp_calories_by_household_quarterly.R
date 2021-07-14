
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


######################
## Helper Functions ##
######################

calories_per_tfp_fn <- function(year, products_master, qfahpd_main, qfahpd_secondary) {
  
  purchase <-
    readr::read_tsv(paste0("/kilts/nielsen_extracts/HMS/", year, "/Annual_Files/purchases_", year, ".tsv"))
  
  trips <-
    readr::read_tsv(paste0("/kilts/nielsen_extracts/HMS/", year, "/Annual_Files/trips_", year, ".tsv"))
  
  niel_df <-
    purchase %>% 
    dplyr::select(trip_code_uc, quantity, upc, upc_ver_uc, total_price_paid) %>% 
    left_join(
      trips %>% 
        dplyr::select(
          trip_code_uc,
          household_code,
          total_spent,
          purchase_date 
        )
    ) %>% 
    mutate(
      month = month(purchase_date),
      quarter = quarter(purchase_date),
      year = year
    ) %>% 
    dplyr::select(household_code, quantity, upc, upc_ver_uc, total_spent, quarter, year)
  
  rm(purchase, trips)
  
  panelists <-
    readr::read_tsv(paste0("/kilts/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv"))
  
  niel_df <-
    niel_df %>%
    left_join(
      products_master %>%
        dplyr::select(
          calories_sc_imp,
          upc,
          upc_ver_uc
        )
    )
  
  niel_df <-
    niel_df  %>% 
    dplyr::select(
      household_code,
      quantity,
      calories_sc_imp,
      upc,
      upc_ver_uc,
      quarter,
      year
    ) %>% 
    filter(!is.na(calories_sc_imp)) 
  
  
  niel_df <-
    niel_df %>% mutate(upc = as.double(upc)) %>% 
    left_join(
      qfahpd_main,
      by = c("upc", "upc_ver_uc")
    )
  
  niel_df <-
    niel_df %>% mutate(upc = as.double(upc)) %>% 
    left_join(
      qfahpd_secondary,
      by = c("upc", "upc_ver_uc", "year")
    )
  
  niel_df <-
    niel_df %>% 
    mutate(
      tfp = coalesce(tfp.x, tfp.y)
    ) %>% 
    dplyr::select(
      household_code,
      calories_sc_imp,
      quantity,
      tfp,
      quarter
    )
  
  niel_df <-
    niel_df %>% 
    mutate(
      calories_sc_imp = calories_sc_imp * quantity
    )
  
  total_calories_by_household <-
    niel_df %>% 
    group_by(household_code, quarter) %>% 
    summarise(
      total_calories = sum(calories_sc_imp, na.rm = TRUE)
    )
  
  tfp_calories_by_household <-
    niel_df %>% 
    group_by(household_code, tfp, quarter) %>% 
    summarise(
      tfp_total_calories = sum(calories_sc_imp, na.rm = TRUE)
    )
  
  rm(niel_df)
  
  tfp_calories_by_household <- 
    tfp_calories_by_household %>% 
    left_join(
      total_calories_by_household,
      by = c("household_code", "quarter")
    ) %>% 
    mutate(
      tfp_prop_calories = tfp_total_calories / total_calories
    ) 
  
  tfp_calories_by_household <- 
    tfp_calories_by_household %>% 
    dplyr::select(
      household_code, 
      tfp, 
      quarter,
      tfp_prop_calories
    )

  tfp_calories_by_household_wide <-
    tfp_calories_by_household %>% 
    spread(tfp, tfp_prop_calories)
  
  rm(tfp_calories_by_household)
  
  tfp_calories_by_household_wide <-
    tfp_calories_by_household_wide %>% 
    left_join(
      panelists %>% 
        dplyr::select(
          household_code = Household_Cd,
          income = Household_Income,
          household_size = Household_Size,
          Household_Composition,
          Projection_Factor,
          Panel_Year ,
          Male_Head_Age:Female_Head_Occupation,
          Marital_Status,
          Race,
          Hispanic_Origin,
          Panelist_ZipCd,
          Fips_State_Cd,
          Fips_County_Cd,
          Wic_Indicator_Current
        ),
      by = "household_code"
    )
  
  rm(panelists)
  
  
  write_csv(qfahpd_health_calories_by_household_wide, paste0("/project/ourminsk/nielsen/data/data_exports/tfp_calories_sc_imputed_monthly/tfp_calories_imputed_sc_by_household_quarterly_wide_", year, ".csv"))
  print(paste0("Wide format data saved for ", year, "."))
  
  gc()
}


products_master <- 
  readr::read_csv("/project/ourminsk/nielsen/data/products_master_imputed_calories_servings_conversion.csv")

head(products_master)

qfahpd_main <- 
  read_csv("/project/ourminsk/nielsen/data/qfahpd_main_w_tfp_and_health.csv") %>% 
  dplyr::select(upc, upc_ver_uc, tfp, qfahpd_health)

qfahpd_secondary <- 
  read_csv("/project/ourminsk/nielsen/data/qfahpd_secondary_w_tfp_and_health.csv") %>% 
  dplyr::select(upc, upc_ver_uc, tfp, qfahpd_health, year)

main_tfp_calories_by_household_fn <- function(year){
  calories_per_tfp_fn(year, products_master, qfahpd_main, qfahpd_secondary)
}




years <- seq(2004, 2019, 1)


cores = 2
cl <- makeCluster(2) 
registerDoParallel(cl)

foreach(i = 1:length(years), .packages = c("tidyverse", "lubridate")) %dopar% {
  main_tfp_calories_by_household_fn(years[i])
  gc()
}

#stop cluster
stopCluster(cl)