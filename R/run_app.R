#' Launch the Data Quality Check application
#'
#' This function launches the WFP Data Quality Check Shiny application.
#' The app allows users to upload and analyze SPSS datasets (.sav files)
#' to check data quality and visualize key food security indicators including
#' Food Consumption Score (FCS), Household Dietary Diversity Score (HDDS), 
#' Reduced Coping Strategy Index (rCSI), Household Hunger Scale (HHS), 
#' and Livelihood Coping Strategy Index (LCS).
#'
#' @importFrom utils install.packages read.csv
#' @importFrom shiny shinyAppDir
#' @importFrom argonDash argonDashPage
#' @importFrom argonR argonCard
#' @importFrom DT datatable
#' @importFrom dplyr filter group_by mutate select across where
#' @importFrom formattable percent
#' @importFrom haven read_sav
#' @importFrom htmltools HTML
#' @importFrom labelled to_factor
#' @importFrom lubridate as_date
#' @importFrom plotly plot_ly ggplotly layout
#' @importFrom readxl read_xlsx read_excel
#' @importFrom rmarkdown render
#' @importFrom rstatix is_outlier
#' @importFrom shinyWidgets downloadBttn
#' @importFrom stats median
#' @importFrom stringr str_detect
#' @importFrom tidyr separate
#'
#' @return A Shiny application object
#' @export
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function() {
  # Check for required packages and install if missing
  pkgs <- c("shiny", "argonDash", "argonR", "DT", "shinyWidgets",
            "labelled", "haven", "plotly", "lubridate", "rstatix", 
            "dplyr", "tidyr", "stringr", "readxl", "formattable")
  
  invisible(lapply(pkgs, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(paste("Installing package:", pkg))
      utils::install.packages(pkg)
    }
  }))
  
  # Get the path to the application directories
  app_dir <- system.file("app", package = "WFPdataqualitycheck")
  
  if (app_dir == "") {
    stop("Could not find app directory. Try reinstalling 'WFPdataqualitycheck'.", call. = FALSE)
  }
  
  # Run the application
  shiny::shinyAppDir(app_dir)
}