# Nielsen Data Exports
This folders contains code for creating data summaries or exports of the Nielsen data. The data munging performed by this code is currently focused primarily on food purchases. Note that many of the scripts are designed to work on the Mercury Computing Cluster and take advantage of parallelization.

### Folder Structure

1. The **calories_data_exports** folder contains scripts for calculating the percentage of a households calorie budget that is allotted to different categories over different time periods.
  - **qfahpd_calories_by_household_monthly.R** calculates the percentage of households calorie budget that is allotted to Quarterly Food At Home Price Database categories that have been classified as healthy or noth healthy (see top-level repository for more on this classification).
2. The **spend_data_exports** folder contains scripts for calculating the percentage of a households food spend budget that is allotted to different categories over different time periods.