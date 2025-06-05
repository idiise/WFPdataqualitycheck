# Data Quality Check

## Overview

A Shiny application package for checking data quality and visualizing key food security indicators in WFP surveys. Developed by Idrissa Dabo.

## Features

- Upload SPSS datasets (.sav files) up to 200MB
- Analyze food security indicators:
  - Food Consumption Score (FCS)
  - Household Dietary Diversity Score (HDDS)
  - Reduced Coping Strategy Index (rCSI)
  - Household Hunger Scale (HHS)
  - Livelihood Coping Strategy Index (LCS)
- View results by administrative levels (Admin1, Admin2) and by enumerator
- Track survey progress with submission statistics
- Generate comprehensive HTML reports with visualizations

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("username/WFPdataqualitycheck")
```

## Usage

```r
library(WFPdataqualitycheck)

# Launch the application
run_app()
```

## Requirements

The application requires datasets with standard variable names. For more information on required variables, please see the in-app documentation or refer to [this guide](https://wfp-vam.github.io/RBD_FS_CH_guide_EN/combined-questionnaire-syntaxes-for-all-5-indicators.html).

## Contact

For questions or support, please contact:
- Idrissa Dabo: idrissa.dabo@wfp.org