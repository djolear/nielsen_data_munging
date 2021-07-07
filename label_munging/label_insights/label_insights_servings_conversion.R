# label insights file as 'li_upc'

servings_master <- 
  li_upc %>% 
  mutate(
    # For products that have # servings per pack, convert this variable to numeric.
    servings = gsub("[^0-9.]", "", `Serves Per Pack`),
    new_servings = ifelse(as.numeric(servings) >= 1, as.numeric(servings), NA),
    
    size = Size,
    size_quant = as.numeric(gsub("[^0-9.]", "", Size)),
    size_uom = str_to_lower(str_extract(Size, "[:alpha:]{1,}")),
    
    serving_size = `Serving Size`,
    serving_size_quant = as.numeric(gsub("[^0-9.]", "", serving_size)),
    serving_size_uom = str_to_lower(`Serving Size UOM`),
    
    size2 = `Size 2`,
    size_quant2 = as.numeric(gsub("[^0-9.]", "", `Size 2`)),
    size_uom2 = str_to_lower(str_extract(`Size 2`, "[:alpha:]{1,}")),
    
    serving_size2 = `Serving Size 2`,
    serving_size_quant2 = as.numeric(gsub("[^0-9.]", "", serving_size2)),
    serving_size_uom2 = str_to_lower(`Serving Size UOM 2`)
  ) 

servings_first_pass <- 
  servings_master %>% 
  filter(!is.na(new_servings)) %>% 
  mutate(
    pass = "first"
  )

# need to figure out double decimals

# 28.3495 = grams/oz
# 453.592 = grams/pound
# 29.5735 = ml/fl oz

servings_second_pass <-
  servings_master %>% 
  filter(is.na(new_servings) & !is.na(Calories) & !is.na(size) & !is.na(serving_size)) %>% 
  filter(size_uom %in% c("oz", "lbs", "fl", "fl oz", "g", "ml") & serving_size_uom %in% c("oz", "lbs", "fl", "fl oz", "g", "ml")) %>% 
  mutate(
    new_size = 
      case_when(
        size_uom == "oz" ~ 28.3495 * size_quant,
        size_uom == "lbs" ~ 453.592 * size_quant,
        size_uom == "fl" ~ 29.5735 * size_quant,
        size_uom == "fl oz" ~ 29.5735 * size_quant,
        size_uom == "g" ~ size_quant,
        size_uom == "ml" ~ size_quant
      ),
    new_size_uom = 
      case_when(
        size_uom == "oz" ~ "g",
        size_uom == "lbs" ~ "g",
        size_uom == "fl" ~ "g",
        size_uom == "fl oz" ~ "ml",
        size_uom == "g" ~ "g",
        size_uom == "ml" ~  "ml"
      ),
    new_serving_size = 
      case_when(
        serving_size_uom == "oz" ~ 28.3495 * serving_size_quant,
        serving_size_uom == "lbs" ~ 453.592 * serving_size_quant,
        serving_size_uom == "fl" ~ 29.5735 * serving_size_quant,
        serving_size_uom == "fl oz" ~ 29.5735 * serving_size_quant,
        serving_size_uom == "g" ~ serving_size_quant,
        serving_size_uom == "ml" ~ serving_size_quant,
      ),
    new_serving_size_uom = 
      case_when(
        serving_size_uom == "oz" ~ "g",
        serving_size_uom == "lbs" ~ "g",
        serving_size_uom == "fl" ~ "g",
        serving_size_uom == "fl oz" ~ "ml",
        serving_size_uom == "g" ~ "g",
        serving_size_uom == "ml" ~  "ml"
      ),
    pass = "second"
  ) %>% 
  mutate(
    new_servings = new_size / new_serving_size
  ) %>% 
  filter(new_size_uom == new_serving_size_uom)


servings_second_pass %>% 
  count(!is.na(new_servings), new_size_uom == new_serving_size_uom)


servings_third_pass <-
  servings_master %>% 
  filter(is.na(new_servings) & !is.na(Calories) & !is.na(size) & !is.na(serving_size)) %>% 
  filter(
    size_uom %in% c("oz", "lbs", "fl", "fl oz", "g") & 
    serving_size_uom %in% c("cup", "tsp", "tbsp") & 
    serving_size_uom2 %in% c("oz", "lbs", "fl", "fl oz", "g", "ml")
  ) %>% 
  mutate(
    new_size2 = 
      case_when(
        size_uom2 == "oz" ~ 28.3495 * size_quant2,
        size_uom2 == "lbs" ~ 453.592 * size_quant2,
        size_uom2 == "fl" ~ 29.5735 * size_quant2,
        size_uom2 == "fl oz" ~ 29.5735 * size_quant2,
        size_uom2 == "g" ~ size_quant2,
        size_uom2 == "ml" ~ size_quant2
      ),
    new_size_uom2 = 
      case_when(
        size_uom2 == "oz" ~ "g",
        size_uom2 == "lbs" ~ "g",
        size_uom2 == "fl" ~ "g",
        size_uom2 == "fl oz" ~ "ml",
        size_uom2 == "g" ~ "g",
        size_uom2 == "ml" ~  "ml"
      ),
    new_serving_size2 = 
      case_when(
        serving_size_uom2 == "oz" ~ 28.3495 * serving_size_quant2,
        serving_size_uom2 == "lbs" ~ 453.592 * serving_size_quant2,
        serving_size_uom2 == "fl" ~ 29.5735 * serving_size_quant2,
        serving_size_uom2 == "fl oz" ~ 29.5735 * serving_size_quant2,
        serving_size_uom2 == "g" ~ serving_size_quant2,
        serving_size_uom2 == "ml" ~ serving_size_quant2
      ),
    new_serving_size_uom2 = 
      case_when(
        serving_size_uom2 == "oz" ~ "g",
        serving_size_uom2 == "lbs" ~ "g",
        serving_size_uom2 == "fl" ~ "g",
        serving_size_uom2 == "fl oz" ~ "ml",
        serving_size_uom2 == "g" ~ "g",
        serving_size_uom2 == "ml" ~  "ml"
      ),
    third_pass = "third"
  ) %>% 
  mutate(
    new_servings = new_size2 / new_serving_size2
  )  %>% 
  filter(new_size_uom2 == new_serving_size_uom2)
  
servings_third_pass %>% 
  count(!is.na(new_servings), new_size_uom2 == new_serving_size_uom2)

li_upc_update <-
  bind_rows(
    servings_first_pass,
    servings_second_pass,
    servings_third_pass
  )

# li_upc_update %>% 
#   count(is.na(new_servings))

# convert nutrition

li_upc_update <-
  li_upc_update %>% 
  filter(!is.na(new_servings)) %>% 
  mutate(
    calories = Calories * new_servings,
    saturated_fat = `Saturated Fat` * new_servings,
    sodium = Sodium * new_servings,
    sugar = Sugars * new_servings,
    add_sugars = `Added Sugars` * new_servings
  )
