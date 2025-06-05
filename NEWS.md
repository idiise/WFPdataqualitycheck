# WFPdataqualitycheck 0.2.0

* Enhanced file import capabilities - now supports multiple data formats:
  - SPSS files (.sav) - original format
  - Excel files (.xlsx, .xls) 
  - CSV files (.csv) with customizable options (separator, quotes, headers)
  - Text files (.txt) with customizable delimiters
  - Stata files (.dta)
* Added file status display showing format, size, and load status
* Improved error handling and user feedback for file uploads
* Enhanced data validation to check for required columns
* Updated user interface with conditional options for CSV/text files

# WFPdataqualitycheck 0.1.0

* Initial CRAN submission.
* Shiny application for checking data quality in World Food Programme food security surveys.
* Features for analyzing Food Consumption Score (FCS), Household Dietary Diversity Score (HDDS), Reduced Coping Strategy Index (rCSI), Household Hunger Scale (HHS), and Livelihood Coping Strategy Index (LCS).
* Support for administrative level (Admin1, Admin2) and enumerator-level visualizations.