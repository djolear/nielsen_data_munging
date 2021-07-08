qfahpd_tfp_health_crosswalk <- function(df){

  df <-
    df %>% 
    mutate(
      tfp = 
        case_when(
          foodgroup_i == "foodgroup_16" ~ "tfp_1",
          foodgroup_i == "foodgroup_17" ~ "tfp_1",
          foodgroup_i == "foodgroup_18" ~ "tfp_1",
          foodgroup_i == "foodgroup_19" ~ "tfp_2",
          foodgroup_i == "foodgroup_20" ~ "tfp_2",
          foodgroup_i == "foodgroup_21" ~ "tfp_2",
          foodgroup_i == "foodgroup_50" ~ "tfp_2",
          foodgroup_i == "foodgroup_51" ~ "tfp_2",
          foodgroup_i == "foodgroup_8" ~ "tfp_3",
          foodgroup_i == "foodgroup_9" ~ "tfp_3",
          foodgroup_i == "foodgroup_4" ~ "tfp_4",
          foodgroup_i == "foodgroup_5" ~ "tfp_4",
          foodgroup_i == "foodgroup_6" ~ "tfp_5",
          foodgroup_i == "foodgroup_7" ~ "tfp_5",
          foodgroup_i == "foodgroup_14" ~ "tfp_6",
          foodgroup_i == "foodgroup_15" ~ "tfp_6",
          foodgroup_i == "foodgroup_10" ~ "tfp_7",
          foodgroup_i == "foodgroup_11" ~ "tfp_7",
          foodgroup_i == "foodgroup_12" ~ "tfp_7",
          foodgroup_i == "foodgroup_13" ~ "tfp_7",
          foodgroup_i == "foodgroup_1" ~ "tfp_8",
          foodgroup_i == "foodgroup_2" ~ "tfp_8",
          foodgroup_i == "foodgroup_3" ~ "tfp_9",
          foodgroup_i == "foodgroup_25" ~ "tfp_10",
          foodgroup_i == "foodgroup_27" ~ "tfp_10",
          foodgroup_i == "foodgroup_22" ~ "tfp_11",
          foodgroup_i == "foodgroup_24" ~ "tfp_11",
          foodgroup_i == "foodgroup_23" ~ "tfp_12",
          foodgroup_i == "foodgroup_26" ~ "tfp_12",
          foodgroup_i == "foodgroup_28" ~ "tfp_13",
          foodgroup_i == "foodgroup_29" ~ "tfp_13",
          foodgroup_i == "foodgroup_31" ~ "tfp_14",
          foodgroup_i == "foodgroup_32" ~ "tfp_14",
          foodgroup_i == "foodgroup_33" ~ "tfp_15",
          foodgroup_i == "foodgroup_34" ~ "tfp_15",
          foodgroup_i == "foodgroup_30" ~ "tfp_16",
          foodgroup_i == "foodgroup_52" ~ "tfp_16",
          foodgroup_i == "foodgroup_35" ~ "tfp_17",
          foodgroup_i == "foodgroup_36" ~ "tfp_17",
          foodgroup_i == "foodgroup_37" ~ "tfp_18",
          foodgroup_i == "foodgroup_38" ~ "tfp_19",
          foodgroup_i == "foodgroup_39" ~ "tfp_19",
          foodgroup_i == "foodgroup_54" ~ "tfp_20",
          foodgroup_i == "foodgroup_41" ~ "tfp_21",
          foodgroup_i == "foodgroup_42" ~ "tfp_21",
          foodgroup_i == "foodgroup_40" ~ "tfp_22",
          foodgroup_i == "foodgroup_44" ~ "tfp_22",
          foodgroup_i == "foodgroup_45" ~ "tfp_22",
          foodgroup_i == "foodgroup_46" ~ "tfp_22",
          foodgroup_i == "foodgroup_47" ~ "tfp_22",
          foodgroup_i == "foodgroup_49" ~ "tfp_23",
          foodgroup_i == "foodgroup_48" ~ "tfp_24"
        ),
      qfahpd_health = 
        case_when(
          foodgroup_i == "foodgroup_1" ~ "yes",
          foodgroup_i == "foodgroup_2" ~ "yes",
          foodgroup_i == "foodgroup_3" ~ "yes",
          foodgroup_i == "foodgroup_4" ~ "yes",
          foodgroup_i == "foodgroup_5" ~ "yes",
          foodgroup_i == "foodgroup_6" ~ "yes",
          foodgroup_i == "foodgroup_7" ~ "yes",
          foodgroup_i == "foodgroup_8" ~ "yes",
          foodgroup_i == "foodgroup_9" ~ "yes",
          foodgroup_i == "foodgroup_10" ~ "yes",
          foodgroup_i == "foodgroup_11" ~ "yes",
          foodgroup_i == "foodgroup_12" ~ "yes",
          foodgroup_i == "foodgroup_13" ~ "yes",
          foodgroup_i == "foodgroup_14" ~ "yes",
          foodgroup_i == "foodgroup_15" ~ "yes",
          foodgroup_i == "foodgroup_16" ~ "yes",
          foodgroup_i == "foodgroup_17" ~ "yes",
          foodgroup_i == "foodgroup_18" ~ "yes",
          foodgroup_i == "foodgroup_19" ~ "no",
          foodgroup_i == "foodgroup_20" ~ "no",
          foodgroup_i == "foodgroup_21" ~ "no",
          foodgroup_i == "foodgroup_22" ~ "yes",
          foodgroup_i == "foodgroup_23" ~ "yes",
          foodgroup_i == "foodgroup_24" ~ "yes",
          foodgroup_i == "foodgroup_25" ~ "no",
          foodgroup_i == "foodgroup_26" ~ "no",
          foodgroup_i == "foodgroup_27" ~ "no",
          foodgroup_i == "foodgroup_28" ~ "yes",
          foodgroup_i == "foodgroup_29" ~ "no",
          foodgroup_i == "foodgroup_30" ~ "no",
          foodgroup_i == "foodgroup_31" ~ "yes",
          foodgroup_i == "foodgroup_32" ~ "yes",
          foodgroup_i == "foodgroup_33" ~ "yes",
          foodgroup_i == "foodgroup_34" ~ "yes",
          foodgroup_i == "foodgroup_35" ~ "yes",
          foodgroup_i == "foodgroup_36" ~ "yes",
          foodgroup_i == "foodgroup_37" ~ "yes",
          foodgroup_i == "foodgroup_38" ~ "yes",
          foodgroup_i == "foodgroup_39" ~ "no",
          foodgroup_i == "foodgroup_40" ~ "no",
          foodgroup_i == "foodgroup_41" ~ "no",
          foodgroup_i == "foodgroup_42" ~ "no",
          foodgroup_i == "foodgroup_43" ~ "yes",
          foodgroup_i == "foodgroup_44" ~ "no",
          foodgroup_i == "foodgroup_45" ~ "no",
          foodgroup_i == "foodgroup_46" ~ "no",
          foodgroup_i == "foodgroup_47" ~ "no",
          foodgroup_i == "foodgroup_48" ~ "no",
          foodgroup_i == "foodgroup_49" ~ "no",
          foodgroup_i == "foodgroup_50" ~ "no",
          foodgroup_i == "foodgroup_51" ~ "no",
          foodgroup_i == "foodgroup_52" ~ "no",
          foodgroup_i == "foodgroup_53" ~ "no",
          foodgroup_i == "foodgroup_54" ~ "no",
          foodgroup_i == "foodgroup_1" ~ "no",
          foodgroup_i == "foodgroup_1" ~ "no",
          foodgroup_i == "foodgroup_1" ~ "no",
          foodgroup_i == "foodgroup_1" ~ "no",
          foodgroup_i == "foodgroup_1" ~ "no",
          foodgroup_i == "foodgroup_1" ~ "no",
          foodgroup_i == "foodgroup_1" ~ "no",
          foodgroup_i == "foodgroup_1" ~ "yes",
          foodgroup_i == "foodgroup_1" ~ "yes"
        )
      )
  return(df)
}


qfahpd_main <- 
  haven::read_dta("G:/My Drive/research/projects/niel/qfahpd_data/qfahpd_constant.dta") %>% 
  mutate(
    foodgroup_i = paste0("foodgroup_", foodgroup_i)
  )

qfahpd_main <- qfahpd_tfp_health_crosswalk(qfahpd_main)

qfahpd_main <-
  qfahpd_main %>% 
  mutate(
    foodgroup_i_main = foodgroup_i
  ) %>% 
  dplyr::select(-foodgroup_i)

write_csv(qfahpd_main, "G:/My Drive/research/projects/niel/qfahpd_data/qfahpd_main_w_tfp_and_health.csv")

qfahpd_secondary <- 
  haven::read_dta("G:/My Drive/research/projects/niel/qfahpd_data/qfahpd_changing.dta") %>% 
  mutate(
    foodgroup_i = paste0("foodgroup_", foodgroup_i)
  )

qfahpd_secondary <- qfahpd_tfp_health_crosswalk(qfahpd_secondary)

qfahpd_secondary <- 
  qfahpd_secondary %>% 
  mutate(
    foodgroup_i_secondary = foodgroup_i
  ) %>% 
  dplyr::select(-foodgroup_i)

write_csv(qfahpd_secondary, "G:/My Drive/research/projects/niel/qfahpd_data/qfahpd_secondary_w_tfp_and_health.csv")

