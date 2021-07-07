# Label Insights Label Munging
This folder contains code for munging nutrition label data that comes from Label Insights.

### Description of pre-processing

The **label_insights_munge_main.R** is the master script for this data munging.

1. First, the **label_insights_munge_main.R** script loads the Label Insights data.

2. Next, it converts Label Insights UPCs to 13-digit UPCs that we can join with the Nielsen data using the **label_insights_convert_upcs.R**.

3. Next, it converts the Label Insights nutrition information (e.g. Calories) into the amount of the nutrient in the whole item (rather than the amount of the nutrient per serving) using the **label_insights_servings_conversion.R** script.

3. Finally, it writes this new data to disk.

