##############################
## Create 13 Character UPC's ##
##############################

# This only works for 12-character UPC's

label_insights <-
  label_insights %>% 
  mutate(
    char = nchar(UPC)
  )

label_insights <-
  label_insights %>% 
  mutate(
    upc_new = ifelse(char == 12, paste0("0", UPC), UPC)
  )

