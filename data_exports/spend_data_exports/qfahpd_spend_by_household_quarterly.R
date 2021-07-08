
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


bind_nielsen_data_fn <- function(year) {
  purchase <-
    readr::read_tsv(paste0("/kilts/nielsen_extracts/HMS/", year, "/Annual_Files/purchases_", year, ".tsv"))
  
  trips <-
    readr::read_tsv(paste0("/kilts/nielsen_extracts/HMS/", year, "/Annual_Files/trips_", year, ".tsv"))
  
   products_master <- 
    readr::read_tsv(paste0("/kilts/nielsen_extracts/HMS/Master_Files/Latest/products.tsv"))
 
  niel_df <-
    purchase %>% 
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
      quarter = quarter(purchase_date),
      year = year
    )
  
  niel_df <-
    niel_df %>% 
    left_join(
      products_master %>% 
        dplyr::select(
          product_module_descr,
          product_group_descr,
          department_descr,
          size1_amount,
          size1_units,
          upc,
          upc_ver_uc
        )
    )
  
  qfpahd_main <- 
    haven::read_dta("/home/djolear/nielsen/qfahpd_constant.dta") %>% 
    mutate(
      foodgroup_i_main = paste0("foodgroup_", foodgroup_i)
    ) %>% 
    dplyr::select(-foodgroup_i)
  
  qfpahd_secondary <- 
    haven::read_dta("/home/djolear/nielsen/qfahpd_changing.dta") %>% 
    mutate(
      foodgroup_i_secondary = paste0("foodgroup_", foodgroup_i)
    ) %>% 
    dplyr::select(-foodgroup_i)
  
  
  niel_df <-
    niel_df %>% mutate(upc = as.double(upc)) %>% 
    left_join(
      qfpahd_main,
      by = c("upc", "upc_ver_uc")
    )
  
  niel_df <-
    niel_df %>% mutate(upc = as.double(upc)) %>% 
    left_join(
      qfpahd_secondary,
      by = c("upc", "upc_ver_uc", "year")
    )
  
  niel_df <-
    niel_df %>% 
    mutate(
      foodgroup_i = coalesce(foodgroup_i_main, foodgroup_i_secondary)
    )
  
  rm(purchase, trips, products_master)
  
  return(niel_df)
}


foodgroup_spend_by_household_fn <- function(niel_df, year) {
  
  foodgroup_spending_data <-
    niel_df %>% 
    dplyr::select(
      household_code,
      total_spent,
      foodgroup_i,
      quarter
    ) %>% 
    filter(!is.na(foodgroup_i))
  
  foodgroup_spend_by_household <-
    foodgroup_spending_data %>% 
    group_by(household_code, foodgroup_i, quarter) %>% 
    summarise(
      foodgroup_total_spend = sum(total_spent)
    )
  
  total_spend_by_houshold <-
    foodgroup_spending_data %>% 
    group_by(household_code, quarter) %>% 
    summarise(
      total_spend = sum(total_spent)
    )
  
  panelists <-
    readr::read_tsv(paste0("/kilts/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv"))
  
  foodgroup_spend_by_household <- 
    foodgroup_spend_by_household %>% 
    left_join(
      total_spend_by_houshold,
      by = c("household_code", "quarter")
    ) %>% 
    mutate(
      foodgroup_prop_spend = foodgroup_total_spend / total_spend
    ) 
  
  foodgroup_spend_by_household <- 
    foodgroup_spend_by_household %>% 
    dplyr::select(
      household_code, 
      foodgroup_i,
      quarter,
      foodgroup_prop_spend
    )

  foodgroup_spend_by_household_wide <-
    foodgroup_spend_by_household %>% 
    spread(foodgroup_i, foodgroup_prop_spend)
            
  rm(foodgroup_spend_by_household)
  
  foodgroup_spend_by_household_wide <-
    foodgroup_spend_by_household_wide %>% 
    left_join(
      panelists %>% 
        dplyr::select(
          household_code = Household_Cd,
          income = Household_Income,
          household_size = Household_Size,
          Male_Head_Age:Female_Head_Occupation,
          Marital_Status,
          Race,
          zip = Panelist_ZipCd,
          Panelist_ZipCd,
          state_fips = Fips_State_Cd,
          Fips_State_Cd,
          cty_fips = Fips_County_Cd,
          Fips_County_Cd,
          Wic_Indicator_Current           
        ),
      by = "household_code"
    )
  
  write_csv(foodgroup_spend_by_household_wide, paste0("/project/ourminsk/nielsen/data/data_exports/qfahpd_spend_quarterly/foodgroup_spend_by_household_quarterly_wide_", year, ".csv"))
  print(paste0("Wide format data saved for ", year, "."))
  
  return(0)
}


main_foodgroup_spend_by_household_fn <- function(year){
  niel_df <- bind_nielsen_data_fn(year)
  foodgroup_spend_by_household_fn(niel_df, year)
}

years <- seq(2004, 2019, 1)
# years <- seq(2004, 2017)

# for(i in 1:length(years)) {
#   main_group_spend_by_household_fn(years[i])
# }

cores = 4
cl <- makeCluster(4)
registerDoParallel(cl)

foreach(i = 1:length(years), .packages = c("tidyverse", "lubridate")) %dopar% {
  main_foodgroup_spend_by_household_fn(years[i])
  gc()
}

#stop cluster
stopCluster(cl)


# map(.x = years, .f = main_foodgroup_spend_by_household_fn)

