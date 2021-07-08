
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
    dplyr::select(trip_code_uc, upc, upc_ver_uc, total_price_paid) %>% 
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
    dplyr::select(household_code,  upc, upc_ver_uc, total_spent, quarter, year)
  
  # niel_df <-
  #   niel_df %>% 
  #   left_join(
  #     products_master %>% 
  #       dplyr::select(
  #         product_module_descr,
  #         product_group_descr,
  #         department_descr,
  #         size1_amount,
  #         size1_units,
  #         upc,
  #         upc_ver_uc
  #       )
  #   )
  
  qfahpd_main <- 
    read_csv("/home/djolear/nielsen/qfahpd_main_w_tfp_and_health.csv") %>% 
    dplyr::select(upc, upc_ver_uc, qfahpd_health)
  
  qfahpd_secondary <- 
    read_csv("/home/djolear/nielsen/qfahpd_secondary_w_tfp_and_health.csv") %>% 
    dplyr::select(upc, upc_ver_uc, qfahpd_health, year)
  
  
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
      #foodgroup_i = coalesce(foodgroup_i_main, foodgroup_i_secondary),
      qfahpd_health = coalesce(qfahpd_health.x, qfahpd_health.y)
    ) %>% 
    dplyr::select(
      -c(qfahpd_health.x, qfahpd_health.y)
    )
  
  rm(purchase, trips, products_master)
  gc()
  
  return(niel_df)
}


qfahpd_health_spend_by_household_fn <- function(niel_df, year) {
  
  
  qfahpd_health_spending_data <-
    niel_df %>% 
    dplyr::select(
      household_code,
      total_spent,
      qfahpd_health,
      quarter
    ) %>% 
    filter(!is.na(qfahpd_health))
  
  rm(niel_df)
  gc()
  
  qfahpd_health_spend_by_household <-
    qfahpd_health_spending_data %>% 
    group_by(household_code, qfahpd_health, quarter) %>% 
    summarise(
      qfahpd_health_total_spend = sum(total_spent)
    )
  
  total_spend_by_houshold <-
    qfahpd_health_spending_data %>% 
    group_by(household_code, quarter) %>% 
    summarise(
      total_spend = sum(total_spent)
    )
  
  panelists <-
    readr::read_tsv(paste0("/kilts/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv"))
  
  qfahpd_health_spend_by_household <- 
    qfahpd_health_spend_by_household %>% 
    left_join(
      total_spend_by_houshold,
      by = c("household_code", "quarter")
    ) %>% 
    mutate(
      qfahpd_health_prop_spend = qfahpd_health_total_spend / total_spend
    ) 
  
  qfahpd_health_spend_by_household <- 
    qfahpd_health_spend_by_household %>% 
    dplyr::select(
      household_code, 
      qfahpd_health,
      quarter,
      qfahpd_health_prop_spend
    )

  qfahpd_health_spend_by_household_wide <-
    qfahpd_health_spend_by_household %>% 
    spread(qfahpd_health, qfahpd_health_prop_spend)
            
  rm(qfahpd_health_spend_by_household)
  
  qfahpd_health_spend_by_household_wide <-
    qfahpd_health_spend_by_household_wide %>% 
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
  
  write_csv(qfahpd_health_spend_by_household_wide, paste0("/project/ourminsk/nielsen/data/data_exports/qfahpd_health_spend_quarterly/qfahpd_health_spend_by_household_quarterly_wide_", year, ".csv"))
  print(paste0("Wide format data saved for ", year, "."))
  
  return(0)
}


main_qfahpd_health_spend_by_household_fn <- function(year){
  niel_df <- bind_nielsen_data_fn(year)
  qfahpd_health_spend_by_household_fn(niel_df, year)
}

years <- seq(2004, 2019, 1)

# for(i in 1:length(years)) {
#   main_group_spend_by_household_fn(years[i])
# }

cores = 4
cl <- makeCluster(4)
registerDoParallel(cl)

foreach(i = 1:length(years), .packages = c("tidyverse", "lubridate")) %dopar% {
  main_qfahpd_health_spend_by_household_fn(years[i])
  gc()
}

#stop cluster
stopCluster(cl)


# map(.x = years, .f = main_qfahpd_health_spend_by_household_fn)

