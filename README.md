# Nielsen Data Munging
This repository contains code for cleaning and working with Nielsen data. The data munging performed by this code is currently focused primarily on food purchases. Note that many of the scripts are designed to work on the Mercury Computing Cluster and take advantage of parallelization.

### Folder Structure

1. The **label_munging** folder contains scripts for cleaning nutrition label data from third party sources (Label Insights and Syndigo).
2. The **nutrition_imputation** folder contains scripts for imputing nutrition information for products that are not covered by the third party label datasets.
3. The **join_usda** folder contains scripts for joining in datasets from the USDA.
4. The **data_exports** folder contains scripts for export data summaries to analyze.

### Data Sources

In addition to using data from the Nielsen, this project uses data from several other sources.

1. [Label Insights](https://www.labelinsight.com/)
2. [Syndigo](https://www.syndigo.com/)
3. [The United States Department of Agriculture Quarterly Food at Home Price Database](https://www.ers.usda.gov/data-products/quarterly-food-at-home-price-database/)
4. [The United States Department of Agriculture Thrifty Food Plan Categories](https://www.fns.usda.gov/cnpp/usda-food-plans-cost-food-reports)

We also use a classification system from prior academic reasearch to classify foods as healthy or not.

1. [The Effect of Supercenter-format Stores on the Healthfulness of Consumers' Grocery Purchases.](https://onlinelibrary.wiley.com/doi/full/10.1093/ajae/aas132)
By Richard Volpe, Abigail Okrent, & Ephraim Leibtag. Published in 2013.
2. [The Effect of SNAP on the Composition of Purchased Foods: Evidence and Implications.](https://www.aeaweb.org/articles?id=10.1257/pol.20190350&&from=f) By Justine Hastings
Ryan Kessler, & Jesse M. Shapiro. Published in 2020.
