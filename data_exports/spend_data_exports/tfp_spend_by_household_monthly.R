
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
    dplyr::select(household_code,  upc, upc_ver_uc, total_spent, month, year)
  
  qfahpd_main <- 
    read_csv("/project/ourminsk/nielsen/data/qfahpd_main_w_tfp_and_health.csv") %>% 
    dplyr::select(upc, upc_ver_uc, qfahpd_health)
  
  qfahpd_secondary <- 
    read_csv("/project/ourminsk/nielsen/data/qfahpd_secondary_w_tfp_and_health.csv") %>% 
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
      tfp = coalesce(tfp.x, tfp.y)
    ) %>% 
    dplyr::select(
      household_code, total_spent, month, year, tfp
    )
  
  rm(purchase, trips, products_master)
  gc()
  
  return(niel_df)
}


tfp_spend_by_household_fn <- function(niel_df, year) {
  
  
  tfp_spending_data <-
    niel_df %>% 
    dplyr::select(
      household_code,
      total_spent,
      tfp,
      month
    ) %>% 
    filter(!is.na(tfp))
  
  rm(niel_df)
  gc()
  
  tfp_spend_by_household <-
    tfp_spending_data %>% 
    group_by(household_code, tfp, month) %>% 
    summarise(
      tfp_total_spend = sum(total_spent)
    )
  
  total_spend_by_houshold <-
    tfp_spending_data %>% 
    group_by(household_code, month) %>% 
    summarise(
      total_spend = sum(total_spent)
    )
  
  panelists <-
    readr::read_tsv(paste0("/kilts/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv"))
  
  tfp_spend_by_household <- 
    tfp_spend_by_household %>% 
    left_join(
      total_spend_by_houshold,
      by = c("household_code", "month")
    ) %>% 
    mutate(
      tfp_prop_spend = tfp_total_spend / total_spend
    ) 
  
  tfp_spend_by_household <- 
    tfp_spend_by_household %>% 
    dplyr::select(
      household_code, 
      tfp,
      month,
      tfp_prop_spend
    )

  tfp_spend_by_household_wide <-
    tfp_spend_by_household %>% 
    spread(tfp, tfp_prop_spend)
            
  rm(tfp_spend_by_household)
  
  tfp_spend_by_household_wide <-
    tfp_spend_by_household_wide %>% 
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
  
  write_csv(qfahpd_health_spend_by_household_wide, paste0("/project/ourminsk/nielsen/data/data_exports/tfp_spend_monthly/tfp_spend_by_household_monthly_wide_", year, ".csv"))
  print(paste0("Wide format data saved for ", year, "."))
  
  return(0)
}


main_tfp_spend_by_household_fn <- function(year){
  niel_df <- bind_nielsen_data_fn(year)
  tfp_spend_by_household_fn(niel_df, year)
}


years <- seq(2004, 2017)

cores = 4
cl <- makeCluster(4)
registerDoParallel(cl)

foreach(i = 1:length(years), .packages = c("tidyverse", "lubridate")) %dopar% {
  main_tfp_spend_by_household_fn(years[i])
  gc()
}

#stop cluster
stopCluster(cl)


# map(.x = years, .f = main_tfp_spend_by_household_fn)

