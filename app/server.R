library(shiny)
library(argonDash)
library(argonR)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(labelled)
library(haven)
library(plotly)
library(lubridate)
library(rstatix)
library(expss)
library(maditr)
library(echarts4r)
# library(leaflet)
library(readxl)
library(readr)
# library(cowplot)

options(shiny.maxRequestSize = 200*1024^2) 
shinyServer(
  function(input, output){
    
    # commenter les plotlyoutput et datatableouptput du front-end
    # importer la base de donnée ----------------------------------------------
    
    
    data <- reactive({
      base1 <- input$base
      if(is.null(base1)){return()} 
      
      # Get file extension
      file_ext <- tools::file_ext(base1$name)
      
      # Read data based on file extension with error handling
      tryCatch({
        if(file_ext == "sav") {
          # SPSS files
          read_sav(base1$datapath) %>% to_factor()
        } else if(file_ext %in% c("xlsx", "xls")) {
          # Excel files
          read_excel(base1$datapath)
        } else if(file_ext == "csv") {
          # CSV files
          sep_char <- if(is.null(input$sep)) "," else input$sep
          has_header <- if(is.null(input$header)) TRUE else input$header
          quote_char <- if(is.null(input$quote) || input$quote == "") "" else input$quote
          
          read_delim(base1$datapath, 
                    delim = sep_char,
                    col_names = has_header,
                    quote = quote_char,
                    locale = locale(encoding = "UTF-8"),
                    show_col_types = FALSE)
        } else if(file_ext == "txt") {
          # Text files
          sep_char <- if(is.null(input$sep)) "\t" else input$sep
          has_header <- if(is.null(input$header)) TRUE else input$header
          quote_char <- if(is.null(input$quote) || input$quote == "") "" else input$quote
          
          read_delim(base1$datapath, 
                    delim = sep_char,
                    col_names = has_header,
                    quote = quote_char,
                    locale = locale(encoding = "UTF-8"),
                    show_col_types = FALSE)
        } else if(file_ext == "dta") {
          # Stata files
          read_dta(base1$datapath) %>% to_factor()
        } else {
          # Default fallback
          showNotification("Unsupported file format. Please use .sav, .xlsx, .xls, .csv, .txt, or .dta files.", type = "error")
          return(NULL)
        }
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error")
        return(NULL)
      })
    }) %>% bindCache(input$base) 
    
    # Variable descriptions for missing variable popup messages
    variable_descriptions <- list(
      # Core administrative variables
      "ADMIN1Name" = "Administrative Level 1 Name - This is the highest administrative division name (e.g., State, Province, Region)",
      "ADMIN2Name" = "Administrative Level 2 Name - This is the second level administrative division name (e.g., District, County)",
      "ADMIN4Name" = "Administrative Level 4 Name - This is the fourth level administrative division name (e.g., Village, Ward)",
      "EnuName" = "Enumerator Name - The name or ID of the data collector/enumerator",
      "EnuSupervisorName" = "Enumerator Supervisor Name - The name or ID of the data collection supervisor",
      "HHHName" = "Household Head Name - The name of the household head",
      
      # Survey metadata
      "@_submission_time" = "Submission Time - Timestamp when the survey was submitted (format: YYYY-MM-DD HH:MM:SS)",
      "@_location_latitude" = "Location Latitude - GPS latitude coordinate of the survey location",
      "@_location_longitude" = "Location Longitude - GPS longitude coordinate of the survey location",
      
      # Food Consumption Score variables
      "FCSStap" = "FCS Staples - Number of days staple foods were consumed in the past 7 days (0-7)",
      "FCSPulse" = "FCS Pulses - Number of days pulses/legumes were consumed in the past 7 days (0-7)",
      "FCSPr" = "FCS Protein - Number of days protein foods (meat/fish) were consumed in the past 7 days (0-7)",
      "FCSVeg" = "FCS Vegetables - Number of days vegetables were consumed in the past 7 days (0-7)",
      "FCSFruit" = "FCS Fruits - Number of days fruits were consumed in the past 7 days (0-7)",
      "FCSDairy" = "FCS Dairy - Number of days dairy products were consumed in the past 7 days (0-7)",
      "FCSFat" = "FCS Fats - Number of days fats/oils were consumed in the past 7 days (0-7)",
      "FCSSugar" = "FCS Sugar - Number of days sugar/sweets were consumed in the past 7 days (0-7)",
      
      # Household Dietary Diversity Score variables
      "HDDSStapCer" = "HDDS Cereals - Whether cereals/grains were consumed yesterday (Yes/No or 1/0)",
      "HDDSStapRoot" = "HDDS Roots/Tubers - Whether roots/tubers were consumed yesterday (Yes/No or 1/0)",
      "HDDSVeg" = "HDDS Vegetables - Whether vegetables were consumed yesterday (Yes/No or 1/0)",
      "HDDSFruit" = "HDDS Fruits - Whether fruits were consumed yesterday (Yes/No or 1/0)",
      "HDDSPrMeat" = "HDDS Meat - Whether meat was consumed yesterday (Yes/No or 1/0)",
      "HDDSPrEggs" = "HDDS Eggs - Whether eggs were consumed yesterday (Yes/No or 1/0)",
      "HDDSPrFish" = "HDDS Fish - Whether fish/seafood was consumed yesterday (Yes/No or 1/0)",
      "HDDSPulse" = "HDDS Pulses - Whether pulses/legumes were consumed yesterday (Yes/No or 1/0)",
      "HDDSDairy" = "HDDS Dairy - Whether dairy products were consumed yesterday (Yes/No or 1/0)",
      "HDDSFat" = "HDDS Fats - Whether fats/oils were consumed yesterday (Yes/No or 1/0)",
      "HDDSSugar" = "HDDS Sugar - Whether sugar/sweets were consumed yesterday (Yes/No or 1/0)",
      "HDDSCond" = "HDDS Condiments - Whether condiments/spices were consumed yesterday (Yes/No or 1/0)",
      
      # Reduced Coping Strategy Index variables
      "rCSILessQlty" = "rCSI Less Quality - Number of days relied on less preferred foods in the past 7 days (0-7)",
      "rCSIBorrow" = "rCSI Borrow Food - Number of days borrowed food or relied on help in the past 7 days (0-7)",
      "rCSIMealSize" = "rCSI Meal Size - Number of days limited portion size in the past 7 days (0-7)",
      "rCSIMealAdult" = "rCSI Adults Eat Less - Number of days adults ate less so children could eat in the past 7 days (0-7)",
      "rCSIMealNb" = "rCSI Meal Number - Number of days reduced number of meals in the past 7 days (0-7)",
      
      # Household Hunger Scale variables
      "HHSNoFood_FR" = "HHS No Food Frequency - How often no food to eat in the past 4 weeks (0=Never, 1=Rarely, 2=Sometimes, 3=Often)",
      "HHSBedHung_FR" = "HHS Bed Hungry Frequency - How often went to bed hungry in the past 4 weeks (0=Never, 1=Rarely, 2=Sometimes, 3=Often)",
      "HHSNotEat_FR" = "HHS Not Eat Frequency - How often went whole day without eating in the past 4 weeks (0=Never, 1=Rarely, 2=Sometimes, 3=Often)",
      
      # Livelihood Coping Strategy variables - Stress
      "Lcs_stress_DomAsset" = "LCS Stress Domestic Assets - Sold household assets/goods (Yes/No or 1/0)",
      "Lcs_stress_Saving" = "LCS Stress Savings - Spent savings (Yes/No or 1/0)",
      "Lcs_stress_EatOut" = "LCS Stress Eat Out - Reduced expenditures on food (Yes/No or 1/0)",
      "Lcs_stress_BorrowCash" = "LCS Stress Borrow Cash - Purchased food on credit or borrowed money (Yes/No or 1/0)",
      
      # Livelihood Coping Strategy variables - Crisis
      "Lcs_crisis_ProdAssets" = "LCS Crisis Productive Assets - Sold productive assets (Yes/No or 1/0)",
      "Lcs_crisis_Health" = "LCS Crisis Health - Reduced expenditures on health (Yes/No or 1/0)",
      "Lcs_crisis_Edu" = "LCS Crisis Education - Reduced expenditures on education (Yes/No or 1/0)",
      
      # Livelihood Coping Strategy variables - Emergency
      "Lcs_em_ResAsset" = "LCS Emergency Residential Assets - Sold house or land (Yes/No or 1/0)",
      "Lcs_em_Begged" = "LCS Emergency Begged - Begged for money/food (Yes/No or 1/0)",
      "Lcs_em_IllegalAct" = "LCS Emergency Illegal Activities - Engaged in illegal income activities (Yes/No or 1/0)"
    )
    
    # Function to check missing variables without showing notifications
    check_missing_variables <- function(data_df) {
      if(is.null(data_df)) return(NULL)
      
      # Define all required variables by category
      essential_vars <- c("ADMIN1Name", "ADMIN2Name", "EnuName")
      metadata_vars <- c("@_submission_time", "@_location_latitude", "@_location_longitude")
      fcs_vars <- c("FCSStap", "FCSPulse", "FCSPr", "FCSVeg", "FCSFruit", "FCSDairy", "FCSFat", "FCSSugar")
      hdds_vars <- c("HDDSStapCer", "HDDSStapRoot", "HDDSVeg", "HDDSFruit", "HDDSPrMeat", "HDDSPrEggs", "HDDSPrFish", "HDDSPulse", "HDDSDairy", "HDDSFat", "HDDSSugar", "HDDSCond")
      rcsi_vars <- c("rCSILessQlty", "rCSIBorrow", "rCSIMealSize", "rCSIMealAdult", "rCSIMealNb")
      hhs_vars <- c("HHSNoFood_FR", "HHSBedHung_FR", "HHSNotEat_FR")
      lcs_vars <- c("Lcs_stress_DomAsset", "Lcs_stress_Saving", "Lcs_stress_EatOut", "Lcs_stress_BorrowCash",
                    "Lcs_crisis_ProdAssets", "Lcs_crisis_Health", "Lcs_crisis_Edu",
                    "Lcs_em_ResAsset", "Lcs_em_Begged", "Lcs_em_IllegalAct")
      other_vars <- c("ADMIN4Name", "EnuSupervisorName", "HHHName")
      
      # Check for missing variables in each category
      data_cols <- names(data_df)
      
      missing_essential <- essential_vars[!essential_vars %in% data_cols]
      missing_metadata <- metadata_vars[!metadata_vars %in% data_cols]
      missing_fcs <- fcs_vars[!fcs_vars %in% data_cols]
      missing_hdds <- hdds_vars[!hdds_vars %in% data_cols]
      missing_rcsi <- rcsi_vars[!rcsi_vars %in% data_cols]
      missing_hhs <- hhs_vars[!hhs_vars %in% data_cols]
      missing_lcs <- lcs_vars[!lcs_vars %in% data_cols]
      missing_other <- other_vars[!other_vars %in% data_cols]
      
      all_missing_indicators <- c(missing_metadata, missing_fcs, missing_hdds, missing_rcsi, missing_hhs, missing_lcs, missing_other)
      
      return(list(
        essential_missing = missing_essential,
        metadata_missing = missing_metadata,
        fcs_missing = missing_fcs,
        hdds_missing = missing_hdds,
        rcsi_missing = missing_rcsi,
        hhs_missing = missing_hhs,
        lcs_missing = missing_lcs,
        other_missing = missing_other,
        total_missing = length(all_missing_indicators) + length(missing_essential),
        has_critical_missing = length(missing_essential) > 0
      ))
    }
    
    # Function to show section-specific notifications with close button
    show_section_notification <- function(section_name, missing_vars, var_type = "warning") {
      if(length(missing_vars) == 0) return()
      
      message_text <- switch(section_name,
        "submission" = paste0("Cannot display submission timeline.\n\nMissing variable: @_submission_time\n\n",
                             variable_descriptions[["@_submission_time"]]),
        "admin1" = paste0("Cannot display ADMIN1 analysis.\n\nMissing variable: ADMIN1Name\n\n",
                         variable_descriptions[["ADMIN1Name"]]),
        "admin2" = paste0("Cannot display ADMIN2 analysis.\n\nMissing variables: ",
                         paste(missing_vars, collapse = ", "), "\n\n",
                         "Required for geographical breakdown by administrative level 2."),
        "enumerator" = paste0("Cannot display Enumerator analysis.\n\nMissing variables: ",
                             paste(missing_vars, collapse = ", "), "\n\n",
                             "Required for data quality monitoring by enumerator."),
        "fcs" = paste0("Cannot display Food Consumption Score (FCS) analysis.\n\nMissing variables:\n",
                      paste(missing_vars, collapse = ", "), "\n\n",
                      "FCS measures household food access based on dietary diversity and food frequency."),
        "hdds" = paste0("Cannot display Household Dietary Diversity Score (HDDS) analysis.\n\nMissing variables:\n",
                       paste(missing_vars, collapse = ", "), "\n\n",
                       "HDDS measures dietary diversity based on food groups consumed."),
        "rcsi" = paste0("Cannot display Reduced Coping Strategy Index (rCSI) analysis.\n\nMissing variables:\n",
                       paste(missing_vars, collapse = ", "), "\n\n",
                       "rCSI measures food-related coping behaviors when households face food shortages."),
        "hhs" = paste0("Cannot display Household Hunger Scale (HHS) analysis.\n\nMissing variables:\n",
                      paste(missing_vars, collapse = ", "), "\n\n",
                      "HHS measures household experience of food insecurity-related hunger."),
        "lcs" = paste0("Cannot display Livelihood Coping Strategies (LCS) analysis.\n\nMissing variables:\n",
                      paste(missing_vars, collapse = ", "), "\n\n",
                      "LCS measures household responses to economic shocks and stresses."),
        paste0("Missing variables for ", section_name, ": ", paste(missing_vars, collapse = ", "))
      )
      
      showNotification(
        HTML(paste0(message_text, "<br><br><em>Click to dismiss this message.</em>")),
        type = var_type,
        duration = NULL,  # User must click to dismiss
        closeButton = TRUE
      )
    }
    
    # Function to show error notifications for technical issues
    show_error_notification <- function(section_name, error_message) {
      clean_error <- if(grepl("object.*not found", error_message, ignore.case = TRUE)) {
        "Required data is not available for analysis."
      } else if(grepl("subscript.*bounds", error_message, ignore.case = TRUE)) {
        "Data structure does not match expected format."
      } else if(grepl("non-numeric", error_message, ignore.case = TRUE)) {
        "Data contains non-numeric values where numbers are expected."
      } else if(grepl("zero.*length", error_message, ignore.case = TRUE)) {
        "No data available for analysis."
      } else {
        "An error occurred while processing the data."
      }
      
      showNotification(
        HTML(paste0("Error in ", section_name, " analysis:<br><br>",
                   clean_error, "<br><br>",
                   "Please check that your data contains the required variables and is properly formatted.",
                   "<br><br><em>Click to dismiss this message.</em>")),
        type = "error",
        duration = NULL,
        closeButton = TRUE
      )
    }
    
    # File status output
    output$file_status <- renderText({
      if(is.null(input$base)) {
        return("No file uploaded")
      }
      
      file_ext <- tools::file_ext(input$base$name)
      file_size <- round(input$base$size / 1024^2, 2)
      
      status_text <- paste0("File: ", input$base$name, "\n",
                           "Format: ", toupper(file_ext), "\n", 
                           "Size: ", file_size, " MB\n")
      
      if(!is.null(data())) {
        # Check variables without showing notifications
        validation_result <- check_missing_variables(data())
        
        if(!is.null(validation_result)) {
          if(validation_result$has_critical_missing) {
            status_text <- paste0(status_text, 
                                 "Status: ❌ Critical variables missing\n",
                                 "Missing essential columns: ", 
                                 paste(validation_result$essential_missing, collapse = ", "))
          } else if(validation_result$total_missing > 0) {
            status_text <- paste0(status_text, 
                                 "Status: ⚠ File loaded with warnings\n",
                                 "Some analysis variables are missing")
          } else {
            status_text <- paste0(status_text, "Status: ✓ Successfully loaded")
          }
        } else {
          status_text <- paste0(status_text, "Status: ✓ Successfully loaded")
        }
      } else {
        status_text <- paste0(status_text, "Status: ✗ Error loading file")
      }
      
      return(status_text)
    })
    
    # Helper function to safely check if variables exist
    safe_check_vars <- function(data_df, required_vars) {
      if(is.null(data_df)) return(FALSE)
      all(required_vars %in% names(data_df))
    }
    
    # Helper function to safely access data with fallback
    safe_data_access <- function(data_df, var_name, fallback_value = NA) {
      if(is.null(data_df) || !var_name %in% names(data_df)) {
        return(fallback_value)
      }
      return(data_df[[var_name]])
    }
    
    header <- reactive(head(data()))
    output$apercu <- renderDataTable(datatable({
      header()
    }, rownames = FALSE,options = list(dom = 'Blfrtip',scrollX = 300,scroller = TRUE,scrollY = 300, FixeHeader = TRUE))
    
    )
    
    
    
    # Accueil -----------------------------------------------------------------
    
    indispensable <- reactive({
      standard_var <- readxl::read_xlsx("required_variables.xlsx")
      
    })
    
    output$varStandard <- renderDataTable(datatable({
      indispensable()
    }, rownames = FALSE)
    )
    
    
    
    # Avancemet de l'enquête --------------------------------------------------
    

# Submissions -------------------------------------------------------------

      
    Total_Question <- reactive({
      data_df <- req(data())
      
      # Check if submission time variable exists
      if(!safe_check_vars(data_df, "@_submission_time")) {
        return(data.frame(Survey_date = as.Date(character(0)), n = integer(0)))
      }
      
      tryCatch({
        data_df %>% 
          separate(`@_submission_time`, c("Survey_date", "survey_hour"), sep = " ") %>% 
          group_by(Survey_date) %>% 
          count()
      }, error = function(e) {
        return(data.frame(Survey_date = as.Date(character(0)), n = integer(0)))
      })
    }) 
    
    Total_Question2 <- reactive({
      Total_Question <- Total_Question() |> mutate(
        Survey_date = as_date(Survey_date)
      )
      Total_Question <- as.data.frame(Total_Question)
    })
    
    output$submission <- renderEcharts4r({
      tryCatch({
        data_df <- data()
        if(is.null(data_df)) return(NULL)
        
        # Check if variables are available for this section
        if(!safe_check_vars(data_df, "@_submission_time")) {
          show_section_notification("submission", c("@_submission_time"))
          return(NULL)
        }
        
        submission_data <- Total_Question2()
        if(is.null(submission_data) || nrow(submission_data) == 0) {
          show_error_notification("Submission Timeline", "No submission data available")
          return(NULL)
        }
        
        submission_data %>%
          e_charts(Survey_date) %>%  # Initialiser avec l'axe X = dates
          e_line(n, smooth = TRUE, symbol = "none") %>%  # Ligne lissée sans marqueurs
          e_x_axis(name = "Date", 
                   type = "time",
                   axisLabel = list(
                     rotate = 45,
                     formatter = "{yyyy}-{MM}-{dd}"
                   )) %>%  # Personnaliser l'axe X
          e_y_axis(name = "Total questionnaire") %>%  # Personnaliser l'axe Y
          e_title("Data collected") %>%  # Titre
          e_tooltip(trigger = "axis") %>%  # Tooltip interactif
          e_datazoom() |>   # Zoom sur les données
          e_legend(show = FALSE) |> 
          e_toolbox_feature("saveAsImage")
          
      }, error = function(e) {
        show_error_notification("Submission Timeline", e$message)
        return(NULL)
      })
    })
    

# Admin1 ------------------------------------------------------------------

      
    countsadm1table <- reactive({
      data_df <- req(data())
      
      # Check if ADMIN1Name variable exists
      if(!safe_check_vars(data_df, "ADMIN1Name")) {
        return(data.frame(ADMIN1Name = character(0), n = integer(0)))
      }
      
      tryCatch({
        data_df %>% group_by(ADMIN1Name) %>% count()
      }, error = function(e) {
        return(data.frame(ADMIN1Name = character(0), n = integer(0)))
      })
    }) %>% bindCache(data())
    
    output$adm1_graph <- renderPlotly({
      tryCatch({
        data_df <- data()
        if(is.null(data_df)) return(NULL)
        
        # Check if variables are available for this section
        if(!safe_check_vars(data_df, "ADMIN1Name")) {
          show_section_notification("admin1", c("ADMIN1Name"))
          return(NULL)
        }
        
        admin1_data <- countsadm1table()
        if(is.null(admin1_data) || nrow(admin1_data) == 0) {
          show_error_notification("ADMIN1 Analysis", "No ADMIN1 data available for analysis")
          return(NULL)
        }
        
        admin1_data %>% 
          plot_ly(x = ~reorder(ADMIN1Name, -n), y = ~n) %>% 
          add_bars() %>% 
          layout(xaxis = list(title = ""), 
                 yaxis = list(title = "total Questionnaire"))
                 
      }, error = function(e) {
        show_error_notification("ADMIN1 Analysis", e$message)
        return(NULL)
      })
    })
    
    # report admin1
    pavaadmihtml <- reactive({
      countsadm1table()  %>% plot_ly(x =~reorder(ADMIN1Name,-n), y=~n ) %>% add_bars() %>% layout(xaxis=list(title= ""), 
                                                                                                  yaxis= list(title = "Total Questionnaire"))
    })
    

# Admin2 ------------------------------------------------------------------

    
    output$avancementAdmin1 <- renderUI({
      data_df <- data()
      if(is.null(data_df) || !safe_check_vars(data_df, "ADMIN1Name")) {
        return(selectInput(inputId = "admin1avancement", label = "ADMIN1Name", 
                          choices = "No ADMIN1Name variable found"))
      }
      
      selectInput(
        inputId = "admin1avancement", label = "ADMIN1Name", 
        choices = unique(data_df$ADMIN1Name)
      )
    })
    
    countsadm1adm2table <- reactive({
      data_df <- req(data())
      
      # Check if required variables exist
      if(!safe_check_vars(data_df, c("ADMIN1Name", "ADMIN2Name"))) {
        return(data.frame(ADMIN1Name = character(0), ADMIN2Name = character(0), n = integer(0)))
      }
      
      if(is.null(input$admin1avancement)) {
        return(data.frame(ADMIN1Name = character(0), ADMIN2Name = character(0), n = integer(0)))
      }
      
      tryCatch({
        data_df %>% 
          group_by(ADMIN1Name, ADMIN2Name) %>% 
          count() %>%
          filter(ADMIN1Name == req(input$admin1avancement))
      }, error = function(e) {
        return(data.frame(ADMIN1Name = character(0), ADMIN2Name = character(0), n = integer(0)))
      })
    }) 
    # %>% bindCache(data())
    
    
    output$adm2_graph <- renderPlotly({
      tryCatch({
        data_df <- data()
        if(is.null(data_df)) return(NULL)
        
        # Check if variables are available for this section
        required_vars <- c("ADMIN1Name", "ADMIN2Name")
        missing_vars <- required_vars[!required_vars %in% names(data_df)]
        if(length(missing_vars) > 0) {
          show_section_notification("admin2", missing_vars)
          return(NULL)
        }
        
        admin2_data <- countsadm1adm2table()
        if(is.null(admin2_data) || nrow(admin2_data) == 0) {
          show_error_notification("ADMIN2 Analysis", "No ADMIN2 data available for the selected ADMIN1")
          return(NULL)
        }
        
        admin2_data %>% 
          plot_ly(x = ~reorder(ADMIN2Name, -n), y = ~n, type = "bar") %>%
          layout(xaxis = list(title = "", zerolinecolor = '#ffff',
                             zerolinewidth = 2,
                             gridcolor = 'ffff'),
                 yaxis = list(title = "total Questionnaire"))
                 
      }, error = function(e) {
        show_error_notification("ADMIN2 Analysis", e$message)
        return(NULL)
      })
    })
    
    # report admin2
    pavaadmi2html <- reactive({
      data_df <- req(data())
      
      # Check if required variables exist
      if(!safe_check_vars(data_df, c("ADMIN1Name", "ADMIN2Name"))) {
        return(NULL)
      }
      
      # Use the currently selected admin1 filter, fallback to first available if none selected
      selected_admin1 <- input$admin1avancement
      if(is.null(selected_admin1) || selected_admin1 == "") {
        selected_admin1 <- unique(data_df$ADMIN1Name)[1]
      }
      
      if(is.null(selected_admin1)) {
        return(NULL)
      }
      
      tryCatch({
        admin2_data <- data_df %>% 
          group_by(ADMIN1Name, ADMIN2Name) %>% 
          count() %>%
          filter(ADMIN1Name == selected_admin1)
          
        if(nrow(admin2_data) == 0) {
          return(NULL)
        }
        
        admin2_data %>% 
          plot_ly(x = ~reorder(ADMIN2Name,-n), y=~n, type = "bar",hovertext = ~paste(ADMIN1Name,"<br>", ADMIN2Name  , n), hoverinfo = "text")  %>%
          layout(xaxis=list(title= "",zerolinecolor = '#ffff',
                            zerolinewidth = 2,
                            gridcolor = 'ffff'),
                 yaxis= list(title = "Total Questionnaire"))
      }, error = function(e) {
        return(NULL)
      })
    })
    

# Enqueteurs --------------------------------------------------------------

    
    
    output$avancementenqueteur <- renderUI({
      data_df <- data()
      if(is.null(data_df) || !safe_check_vars(data_df, "ADMIN1Name")) {
        return(selectInput(inputId = "avancementenqueteur", label = "ADMIN1Name", 
                          choices = "No ADMIN1Name variable found"))
      }
      
      selectInput(
        inputId = "avancementenqueteur", label = "ADMIN1Name", 
        choices = unique(data_df$ADMIN1Name)
      )
    })
    
    countsenumtable <- reactive({
      data_df <- req(data())
      
      # Check if required variables exist
      if(!safe_check_vars(data_df, c("ADMIN1Name", "EnuName"))) {
        return(data.frame(ADMIN1Name = character(0), EnuName = character(0), n = integer(0)))
      }
      
      if(is.null(input$avancementenqueteur)) {
        return(data.frame(ADMIN1Name = character(0), EnuName = character(0), n = integer(0)))
      }
      
      tryCatch({
        data_df %>% 
          group_by(ADMIN1Name, EnuName) %>% 
          count() %>% 
          arrange(desc(n)) %>% 
          filter(ADMIN1Name == req(input$avancementenqueteur))
      }, error = function(e) {
        return(data.frame(ADMIN1Name = character(0), EnuName = character(0), n = integer(0)))
      })
    }) 
    
    # cartographie enumerateur
    enum_mapping <- reactive({
      data_df <- req(data())
      
      # Check if required variables exist for mapping
      required_vars <- c("ADMIN1Name", "EnuName", "@_location_latitude", "@_location_longitude")
      if(!safe_check_vars(data_df, required_vars)) {
        return(data.frame(ADMIN1Name = character(0), EnuName = character(0), 
                         latitude = numeric(0), longitude = numeric(0)))
      }
      
      tryCatch({
        data_df %>% 
          select(ADMIN1Name, EnuName, `@_location_latitude`, `@_location_longitude`) %>% 
          rename(latitude = `@_location_latitude`, longitude = `@_location_longitude`)
      }, error = function(e) {
        return(data.frame(ADMIN1Name = character(0), EnuName = character(0), 
                         latitude = numeric(0), longitude = numeric(0)))
      })
    })
    
    # output$map_enum <- renderLeaflet({
    #   enum_mapping() |> filter(ADMIN1Name == req(input$avancementenqueteur)) |> 
    #   leaflet() |>
    #     addTiles() |>
    #     addCircleMarkers(
    #       ~longitude, ~latitude,
    #       color = "blue",
    #       radius = 5,
    #       stroke = FALSE,
    #       fillOpacity = 0.7,
    #       popup = ~paste("EnuName:", EnuName)
    #     )
    # })
    
    
    # %>% bindCache(data())

# enqueteurs au niveau admin2 ---------------------------------------------

    
    output$adm1enum_graph <- renderPlotly({
      data_df <- data()
      if(is.null(data_df)) return(NULL)
      
      # Check if variables are available for this section
      required_vars <- c("ADMIN1Name", "EnuName")
      missing_vars <- required_vars[!required_vars %in% names(data_df)]
      if(length(missing_vars) > 0) {
        show_section_notification("enumerator", missing_vars)
        return(NULL)
      }
      
      enum_data <- countsenumtable()
      if(is.null(enum_data) || nrow(enum_data) == 0) {
        return(NULL)
      }
      
      enum_data %>% 
        plot_ly(x = ~reorder(EnuName, -n), y = ~n, type = "bar") %>%
        layout(xaxis = list(title = "", zerolinecolor = '#ffff',
                           zerolinewidth = 2,
                           gridcolor = 'ffff'),
               yaxis = list(title = "Total Questionnaire"))
    })
    
    # report enquêteur
    pavaadmienqhtml <- reactive({
      data_df <- req(data())
      
      # Check if required variables exist
      if(!safe_check_vars(data_df, c("ADMIN1Name", "EnuName"))) {
        return(NULL)
      }
      
      # Use the currently selected admin1 filter, fallback to first available if none selected
      selected_admin1 <- input$avancementenqueteur
      if(is.null(selected_admin1) || selected_admin1 == "") {
        selected_admin1 <- unique(data_df$ADMIN1Name)[1]
      }
      
      if(is.null(selected_admin1)) {
        return(NULL)
      }
      
      tryCatch({
        enum_data <- data_df %>% 
          group_by(ADMIN1Name, EnuName) %>% 
          count() %>% 
          arrange(desc(n)) %>% 
          filter(ADMIN1Name == selected_admin1)
          
        if(nrow(enum_data) == 0) {
          return(NULL)
        }
        
        enum_data %>% 
          plot_ly(x = ~ reorder(EnuName,-n), y = ~n,type = "bar",hovertext = ~paste(ADMIN1Name,"<br>", EnuName  , n), hoverinfo = "text") %>%
          layout(xaxis=list(title= "",zerolinecolor = '#ffff',
                            zerolinewidth = 2,
                            gridcolor = 'ffff'),
                   yaxis= list(title = "total Questionnaire"))
      }, error = function(e) {
        return(NULL)
      })
    })
    
    # enquêteurs by admin2
    output$avancementenqueteuradm2 <- renderUI({
      selectInput(
        inputId = "avancementenqueteuradm2",label = "ADMIN1Name", choices = unique(data()$ADMIN1Name)
      )
    })



    countsenumtableadm2 <- reactive({  # if(is.null(data())){return()}
      req(data()) %>% group_by(ADMIN1Name,ADMIN2Name,EnuName) %>%  count() %>% arrange(desc(`n`)) |>
        filter(ADMIN1Name == req(input$avancementenqueteuradm2))
      # filter(EnuName == req(input$enqueteuradm2))

    } #
    )
# par admin4
    output$enqueteuradm2 <- renderUI({
      selectInput(
        inputId = "enqueteuradm2",label = "Enuname", choices = unique(countsenumtableadm2()$EnuName)
      )
    })

    countsenumtableadm3 <- reactive({
      countsenumtableadm2() |> filter(EnuName == req(input$enqueteuradm2))
    })

    output$adm2enum_graph1 <- renderPlotly({
      plot_ly(
        type = "treemap", labels = countsenumtableadm3()$ADMIN2Name,parents=countsenumtableadm3()$EnuName,
        values = countsenumtableadm3()$n
      )
    })
    
    # cartographie
    
    # output$map_enum_adm2 <- renderLeaflet({
    #   enum_mapping() |> filter(ADMIN1Name == req(input$avancementenqueteuradm2)) |> 
    #     filter(EnuName == req(input$enqueteuradm2)) |> 
    #     leaflet() |>
    #     addTiles() |>
    #     addCircleMarkers(
    #       ~longitude, ~latitude,
    #       color = "blue",
    #       radius = 5,
    #       stroke = FALSE,
    #       fillOpacity = 0.7,
    #       popup = ~paste("EnuName:", EnuName)
    #     )
    # })
    
    # report enqueteur adm2
    pavaadmienqhtmladm2 <- reactive({
      data_df <- req(data())
      
      # Check if required variables exist
      if(!safe_check_vars(data_df, c("ADMIN1Name", "ADMIN2Name", "EnuName"))) {
        return(NULL)
      }
      
      # Use the currently selected admin1 filter, fallback to first available if none selected
      selected_admin1 <- input$avancementenqueteuradm2
      if(is.null(selected_admin1) || selected_admin1 == "") {
        selected_admin1 <- unique(data_df$ADMIN1Name)[1]
      }
      
      if(is.null(selected_admin1)) {
        return(NULL)
      }
      
      tryCatch({
        # Get data for the selected admin1
        enum_adm2_data <- data_df %>% 
          group_by(ADMIN1Name, ADMIN2Name, EnuName) %>%  
          count() %>% 
          arrange(desc(n)) %>%
          filter(ADMIN1Name == selected_admin1)
          
        if(nrow(enum_adm2_data) == 0) {
          return(NULL)
        }
        
        # For treemap, we'll show all enumerators within the selected admin1
        # but group them by admin2
        plot_ly(
          type = "treemap", 
          labels = enum_adm2_data$ADMIN2Name,
          parents = enum_adm2_data$EnuName,
          values = enum_adm2_data$n
        )
      }, error = function(e) {
        return(NULL)
      })
    })
    
    
    # Score de consommation alimentaire ---------------------------------------
    
    fcg_colors = c("Acceptable" = "#ECE1B1","Borderline" = "#E67536","Poor" = "#E3002B")
    
    
    dataset <- reactive({
      tryCatch({
        base_data <- req(data())
        result_data <- base_data
        
        # Helper function to safely check and process variables
        safe_mutate_if_exists <- function(df, vars_list, mutate_expr) {
          if(all(vars_list %in% names(df))) {
            tryCatch({
              df <- df %>% mutate_expr
            }, error = function(e) {
              message("Warning in processing ", paste(vars_list, collapse = ", "), ": ", e$message)
              df
            })
          }
          return(df)
        }
        
        # 1. Process FCS variables if they exist
        fcs_vars <- c("FCSStap", "FCSPulse", "FCSPr", "FCSVeg", "FCSFruit", "FCSDairy", "FCSFat", "FCSSugar")
        if(all(fcs_vars %in% names(result_data))) {
          result_data <- result_data %>% 
            mutate(
              FCSStap = ifelse(is.na(FCSStap), 0, FCSStap),
              FCSPulse = ifelse(is.na(FCSPulse), 0, FCSPulse),
              FCSPr = ifelse(is.na(FCSPr), 0, FCSPr),
              FCSVeg = ifelse(is.na(FCSVeg), 0, FCSVeg),
              FCSFruit = ifelse(is.na(FCSFruit), 0, FCSFruit),
              FCSDairy = ifelse(is.na(FCSDairy), 0, FCSDairy),
              FCSFat = ifelse(is.na(FCSFat), 0, FCSFat),
              FCSSugar = ifelse(is.na(FCSSugar), 0, FCSSugar)
            ) %>% 
            mutate(FCS = (2 * FCSStap) + (3 * FCSPulse) + (4 * FCSPr) + FCSVeg + FCSFruit + (4 * FCSDairy) + (0.5 * FCSFat) + (0.5 * FCSSugar)) %>% 
            mutate(
              FCSCat21 = case_when(
                FCS <= 21 ~ "Poor", 
                between(FCS, 21.5, 35) ~ "Borderline", 
                FCS > 35 ~ "Acceptable"
              ),
              FCSCat28 = case_when(
                FCS <= 28 ~ "Poor", 
                between(FCS, 28, 42) ~ "Borderline", 
                FCS > 42 ~ "Acceptable"
              )
            )
        }
        
        # 2. Process HDDS variables if they exist
        hdds_vars <- c("HDDSStapCer", "HDDSStapRoot", "HDDSVeg", "HDDSFruit", "HDDSPrMeat", "HDDSPrEggs", "HDDSPrFish", "HDDSPulse", "HDDSDairy", "HDDSFat", "HDDSSugar", "HDDSCond")
        if(all(hdds_vars %in% names(result_data))) {
          result_data <- result_data %>% 
            mutate(
              HDDSStapCer = case_when(HDDSStapCer == "Yes" ~ 1, TRUE ~ 0),
              HDDSStapRoot = case_when(HDDSStapRoot == "Yes" ~ 1, TRUE ~ 0),
              HDDSVeg = case_when(HDDSVeg == "Yes" ~ 1, TRUE ~ 0),
              HDDSFruit = case_when(HDDSFruit == "Yes" ~ 1, TRUE ~ 0),
              HDDSPrMeat = case_when(HDDSPrMeat == "Yes" ~ 1, TRUE ~ 0),
              HDDSPrEggs = case_when(HDDSPrEggs == "Yes" ~ 1, TRUE ~ 0),
              HDDSPrFish = case_when(HDDSPrFish == "Yes" ~ 1, TRUE ~ 0),
              HDDSPulse = case_when(HDDSPulse == "Yes" ~ 1, TRUE ~ 0),
              HDDSDairy = case_when(HDDSDairy == "Yes" ~ 1, TRUE ~ 0),
              HDDSFat = case_when(HDDSFat == "Yes" ~ 1, TRUE ~ 0),
              HDDSSugar = case_when(HDDSSugar == "Yes" ~ 1, TRUE ~ 0),
              HDDSCond = case_when(HDDSCond == "Yes" ~ 1, TRUE ~ 0)
            ) %>% 
            mutate(HDDS = HDDSStapCer + HDDSStapRoot + HDDSVeg + HDDSFruit + HDDSPrMeat + HDDSPrEggs + HDDSPrFish + HDDSPulse + HDDSDairy + HDDSFat + HDDSSugar + HDDSCond) %>% 
            mutate(HDDS_CH = case_when(
              HDDS >= 5 ~ "Phase1",
              HDDS == 4 ~ "Phase2",
              HDDS == 3 ~ "Phase3",
              HDDS == 2 ~ "Phase4",
              HDDS < 2 ~ "Phase5"
            ))
        }
        
        # 3. Process rCSI variables if they exist
        rcsi_vars <- c("rCSILessQlty", "rCSIBorrow", "rCSIMealSize", "rCSIMealAdult", "rCSIMealNb")
        if(all(rcsi_vars %in% names(result_data))) {
          result_data <- result_data %>% 
            mutate(
              rCSILessQlty = ifelse(is.na(rCSILessQlty), 0, rCSILessQlty),
              rCSIBorrow = ifelse(is.na(rCSIBorrow), 0, rCSIBorrow),
              rCSIMealSize = ifelse(is.na(rCSIMealSize), 0, rCSIMealSize),
              rCSIMealAdult = ifelse(is.na(rCSIMealAdult), 0, rCSIMealAdult),
              rCSIMealNb = ifelse(is.na(rCSIMealNb), 0, rCSIMealNb)
            ) %>% 
            mutate(rCSI = rCSILessQlty + (2 * rCSIBorrow) + rCSIMealSize + (3 * rCSIMealAdult) + rCSIMealNb) %>% 
            mutate(rCSI_CH = case_when(
              rCSI <= 3 ~ "Phase1",
              between(rCSI, 4, 18) ~ "Phase2",
              rCSI >= 19 ~ "Phase3"
            ))
        }
        
        # 4. Process HHS variables if they exist
        hhs_vars <- c("HHSNoFood_FR", "HHSBedHung_FR", "HHSNotEat_FR")
        if(all(hhs_vars %in% names(result_data))) {
          result_data <- result_data %>% 
            mutate(
              HHhSNoFood_FR_r = case_when(
                HHSNoFood_FR == "Rarely (1–2 times)" ~ 1,
                HHSNoFood_FR == "Sometimes (3–10 times)" ~ 1,
                HHSNoFood_FR == "Often (more than 10 times)" ~ 2,
                TRUE ~ 0
              ),
              HHhSBedHung_FR_r = case_when(
                HHSBedHung_FR == "Rarely (1–2 times)" ~ 1,
                HHSBedHung_FR == "Sometimes (3–10 times)" ~ 1,
                HHSBedHung_FR == "Often (more than 10 times)" ~ 2,
                TRUE ~ 0
              ),
              HHhSNotEat_FR_r = case_when(
                HHSNotEat_FR == "Rarely (1–2 times)" ~ 1,
                HHSNotEat_FR == "Sometimes (3–10 times)" ~ 1,
                HHSNotEat_FR == "Often (more than 10 times)" ~ 2,
                TRUE ~ 0
              )
            ) %>% 
            mutate(HHhS = HHhSNoFood_FR_r + HHhSBedHung_FR_r + HHhSNotEat_FR_r) %>% 
            mutate(HHhS_CH = case_when(
              HHhS == 0 ~ "Phase1",
              HHhS == 1 ~ "Phase2",
              HHhS %in% c(2, 3) ~ "Phase3",
              HHhS == 4 ~ "Phase4",
              HHhS >= 5 ~ "Phase5"
            ))
        }
        
        # 5. Process LCS variables if they exist
        lcs_stress_vars <- c("Lcs_stress_DomAsset", "Lcs_stress_Saving", "Lcs_stress_EatOut", "Lcs_stress_BorrowCash")
        lcs_crisis_vars <- c("Lcs_crisis_ProdAssets", "Lcs_crisis_Health", "Lcs_crisis_Edu")
        lcs_emergency_vars <- c("Lcs_em_ResAsset", "Lcs_em_Begged", "Lcs_em_IllegalAct")
        
        if(all(c(lcs_stress_vars, lcs_crisis_vars, lcs_emergency_vars) %in% names(result_data))) {
          result_data <- result_data %>% 
            mutate(
              stress_coping = case_when(
                Lcs_stress_DomAsset == "Yes" | Lcs_stress_DomAsset == "No, because we already sold those assets or have engaged in this activity within the last 12 months and cannot continue to do it" ~ "Yes",
                Lcs_stress_Saving == "Yes" | Lcs_stress_Saving == "No, because we already sold those assets or have engaged in this activity within the last 12 months and cannot continue to do it" ~ "Yes",
                Lcs_stress_EatOut == "Yes" | Lcs_stress_EatOut == "No, because we already sold those assets or have engaged in this activity within the last 12 months and cannot continue to do it" ~ "Yes",
                Lcs_stress_BorrowCash == "Yes" | Lcs_stress_BorrowCash == "No, because we already sold those assets or have engaged in this activity within the last 12 months and cannot continue to do it" ~ "Yes",
                TRUE ~ "No"
              ),
              crisis_coping = case_when(
                Lcs_crisis_ProdAssets == "Yes" | Lcs_crisis_ProdAssets == "No, because we already sold those assets or have engaged in this activity within the last 12 months and cannot continue to do it" ~ "Yes",
                Lcs_crisis_Health == "Yes" | Lcs_crisis_Health == "No, because we already sold those assets or have engaged in this activity within the last 12 months and cannot continue to do it" ~ "Yes",
                Lcs_crisis_Edu == "Yes" | Lcs_crisis_Edu == "No, because we already sold those assets or have engaged in this activity within the last 12 months and cannot continue to do it" ~ "Yes",
                TRUE ~ "No"
              ),
              emergency_coping = case_when(
                Lcs_em_ResAsset == "Yes" | Lcs_em_ResAsset == "No, because we already sold those assets or have engaged in this activity within the last 12 months and cannot continue to do it" ~ "Yes",
                Lcs_em_Begged == "Yes" | Lcs_em_Begged == "No, because we already sold those assets or have engaged in this activity within the last 12 months and cannot continue to do it" ~ "Yes",
                Lcs_em_IllegalAct == "Yes" | Lcs_em_IllegalAct == "No, because we already sold those assets or have engaged in this activity within the last 12 months and cannot continue to do it" ~ "Yes",
                TRUE ~ "No"
              )
            ) %>% 
            mutate(LhCSICat = case_when(
              emergency_coping == "Yes" ~ "EmergencyStrategies",
              crisis_coping == "Yes" ~ "CrisisStrategies",
              stress_coping == "Yes" ~ "StressStrategies",
              TRUE ~ "NoStrategies"
            )) %>% 
            mutate(LhCSICat = fct_relevel(LhCSICat, c("NoStrategies", "StressStrategies", "CrisisStrategies", "EmergencyStrategies")))
        }
        
        return(result_data)
        
      }, error = function(e) {
        # If all processing fails, return the base data
        message("Error in dataset processing: ", e$message)
        return(req(data()))
      })
    }) %>% bindCache(data()) 
    
    # FCGadm1table <- reactive(req(dataset())%>% 
    #                            group_by(ADMIN1Name) %>%  count(FCSCat28) %>% mutate(perc = 100 * n / sum(n)) %>% ungroup() %>% select(-n) %>% 
    #                            mutate_if(is.numeric, round, 1) %>% 
    #                            left_join(countsadm1table(), by = "ADMIN1Name")
    # ) %>% bindCache(dataset())
    
    # output$FCGadm1_barplot <- renderPlotly({
    #   p4 <- FCGadm1table() %>% ggplot(aes(x=ADMIN1Name, y = perc, fill = FCSCat28,text= paste("Total questionnaire :",n) )) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=fcg_colors) + facet_grid(. ~ ADMIN1Name, scales = "free_x")  +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p4)
    # })

# Admin1 ------------------------------------------------------------------
    output$fcsseuiladmn1 <- renderUI({
      selectInput(
        inputId = "varfcsfilteradm1",label = "Selectionner Seuil FCS", choices = names(dataset()[c("FCSCat28","FCSCat21")]),selected = 1
      )
    })
    
    # FCGadm1table <- reactive({
    #   req(dataset()) |> expss::cross_rpct(ADMIN1Name,list(dataset()[input$varfcsfilteradm1]),total_row_position = "none") |> as_tibble() |> dplyr::rename(ADMIN1Name = 1) |> mutate(
    #     ADMIN1Name = stringr::str_replace_all(string = ADMIN1Name,pattern = ".*\\|", replacement = "")
    #   ) |> pivot_longer(
    #     cols = 2:4,names_to = "FCSCat",values_to = "Percentage" )|> mutate(
    #       across( where(is.numeric), ~round(.,1)))
    # })
    
    # output$FCGadm1_barplot <- renderPlotly({
    #   p4 <- FCGadm1table() %>% ggplot(aes(x=ADMIN1Name,  y = Percentage, fill = FCSCat,text = paste(ADMIN1Name, paste0(Percentage,"%"),sep = "  ") )) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=fcg_colors) + facet_grid(. ~ ADMIN1Name, scales = "free_x")  +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent_format())
    #   ggplotly(p4,tooltip = "text")
    # })
    
    ## ajouter par moi
    
    FCGadm1table <- reactive({
      req(dataset()) |> expss::cross_rpct(ADMIN1Name,list(dataset()[input$varfcsfilteradm1]),total_row_position = "none") |> as_tibble() |> dplyr::rename(ADMIN1Name = 1) |> mutate(
        ADMIN1Name = stringr::str_replace_all(string = ADMIN1Name,pattern = ".*\\|", replacement = "")
      ) |> mutate(
        across( where(is.numeric), ~round(.,1)))
    })
    
    output$FCGadm1_barplot <- renderEcharts4r({
      tryCatch({
        data_df <- data()
        if(is.null(data_df)) return(NULL)
        
        # Check if essential variables are available for this section
        fcs_vars <- c("FCSStap", "FCSPulse", "FCSPr", "FCSVeg", "FCSFruit", "FCSDairy", "FCSFat", "FCSSugar")
        missing_vars <- fcs_vars[!fcs_vars %in% names(data_df)]
        
        # If more than half of FCS variables are missing, show notification
        if(length(missing_vars) > length(fcs_vars) / 2) {
          show_section_notification("fcs", missing_vars)
          return(NULL)
        }
        
        # Check if processed FCS data exists in dataset
        dataset_data <- dataset()
        if(is.null(dataset_data) || !"FCSCat28" %in% names(dataset_data)) {
          show_section_notification("fcs", missing_vars)
          return(NULL)
        }
        
        # If some variables are missing, show a warning but continue
        if(length(missing_vars) > 0) {
          showNotification(
            paste0("Warning: FCS analysis is incomplete. Missing variables: ", 
                  paste(missing_vars, collapse = ", "), 
                  ". Results may not be fully accurate."),
            type = "warning", 
            duration = 8
          )
        }
        
        fcs_data <- FCGadm1table()
        if(is.null(fcs_data) || nrow(fcs_data) == 0) {
          show_error_notification("FCS Analysis", "No FCS data available for analysis")
          return(NULL)
        }
        
        fcs_data |>  e_charts(ADMIN1Name) %>%
          e_bar(Poor, name = "Poor", stack = "stack", 
                color = "#E3002B") %>%
          e_bar(Borderline, name = "Borderline", stack = "stack", 
                color = "#E67536") %>%
          e_bar(Acceptable, name = "Acceptable", stack = "stack", 
                color = "#ECE1B1") %>%
          e_labels(position = "inside", 
                   formatter = htmlwidgets::JS("function(params) {return params.value[1] + '%';}")) %>%
          e_y_axis(max = 100, name = "") %>%
          e_x_axis(name = "") %>%
          e_tooltip(trigger = "item") %>%
          e_legend(left = "center") %>%
          e_grid(containLabel = TRUE) |> 
          e_toolbox_feature("saveAsImage")
          
      }, error = function(e) {
        show_error_notification("FCS Analysis", e$message)
        return(NULL)
      })
    })
    
    # rapport HTML
    # p4html <- reactive({
    #   p4 <- FCGadm1table() %>% ggplot(aes(x=ADMIN1Name,  y = Percentage, fill = FCSCat,text = paste(ADMIN1Name, paste0(Percentage,"%"),sep = "  ") )) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=fcg_colors) + facet_grid(. ~ ADMIN1Name, scales = "free_x")  +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p4,tooltip = "text")
    # })
    
    p4html <- reactive({
      FCGadm1table() |>  e_charts(ADMIN1Name) %>%
        e_bar(Poor, name = "Poor", stack = "stack", 
              color = "#E3002B") %>%
        e_bar(Borderline, name = "Borderline", stack = "stack", 
              color = "#E67536") %>%
        e_bar(Acceptable, name = "Acceptable", stack = "stack", 
              color = "#ECE1B1")%>%
        e_labels(position = "inside", 
                 formatter = htmlwidgets::JS("function(params) {return params.value[1] + '%';}")) %>%
        e_y_axis(max = 100, name = "") %>%
        e_x_axis(name = "") %>%
        # e_title("Répartition des catégories FCS par région") %>%
        e_tooltip(trigger = "item") %>%
        e_legend(left = "center") %>%
        e_grid(containLabel = TRUE) |> 
        e_toolbox_feature("saveAsImage")
      
    })
    
    
    #######################  Admin2 ########################### 
    
    output$fcsadmin2name <- renderUI({
      selectInput(
        inputId = "fcsadmin2nom",label = "ADMIN1Name", choices = sort(unique(dataset()$ADMIN1Name)),selected = 1
      )
    })
    
    countsadm1adm2table2 <- reactive(
      countsadm1adm2table()[,c("ADMIN2Name", "n")]
    )
    
    output$fcsseuil <- renderUI({
      selectInput(
        inputId = "varfcsfilter",label = "Selectionner Seuil FCS", choices = names(dataset()[c("FCSCat28","FCSCat21")]),selected = 1
      )
    })
    
    # FCGadm2table <- reactive({req(dataset())%>% 
    #     group_by(ADMIN1Name, ADMIN2Name) %>%  
    #     count(FCSCat28) %>% mutate(perc = 100 * n / sum(n)) %>% ungroup() %>% 
    #     select(-n) %>% mutate_if(is.numeric, round, 1)
    #   # %>% 
    #   #                          filter(ADMIN1Name == req(input$fcsadmin2nom)) %>% 
    #   #                          select(ADMIN1Name, everything())
    #   # %>%  
    #   # left_join(countsadm1adm2table2(), by = "ADMIN2Name") %>%
    #   # filter(ADMIN1Name == req(input$fcsadmin2nom)) 
    #   
    # }) 
    # %>% bindCache(dataset())
    
    # FCGadm2table <- reactive({
    #   req(dataset()) |> expss::cross_rpct(ADMIN2Name,list(dataset()[input$varfcsfilter]),total_row_position = "none") |>   as_data_frame()|> dplyr::rename(ADMIN2Name = 1) |> mutate(
    #     ADMIN2Name = stringr::str_replace_all(string = ADMIN2Name,pattern = ".*\\|", replacement = "")
    #   ) |> mutate(
    #     ADMIN1Name = maditr::vlookup(lookup_value = ADMIN2Name,dict = dataset(),lookup_column = "ADMIN2Name",result_column ="ADMIN1Name")
    #   ) |> relocate(c(ADMIN1Name), .before = ADMIN2Name) |>
    #     tidyr::pivot_longer(cols = 3:5,names_to = "FCSCat",values_to = "Percentage") |> mutate(
    #       across( where(is.numeric), ~round(.,1)  )
    #     )  
    # })
    # 
    # 
    # # output$FCGadm2_barplot <- renderPlotly({
    # #   p5 <- FCGadm2table() %>% filter(ADMIN1Name == req(input$fcsadmin2nom )) %>% ggplot(aes(x=ADMIN2Name, y = perc, fill = FCSCat28)) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=fcg_colors)  +labs(x = "", y = "")  +scale_y_continuous(labels = scales::percent)
    # #   ggplotly(p5)
    # #   
    # # })
    # 
    # output$FCGadm2_barplot <- renderPlotly({
    #   p5 <- FCGadm2table() %>% filter(ADMIN1Name == req(input$fcsadmin2nom )) %>% ggplot(aes(x=ADMIN2Name,  y = Percentage, fill = FCSCat,text = paste(ADMIN2Name, paste0(Percentage,"%"),sep = "  ") )) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=fcg_colors)  +labs(x = "", y = "") + scale_y_continuous(labels = scales::percent)
    #   ggplotly(p5,tooltip = "text")
    #   
    # })
    
    # Ajouter par moi 
    FCGadm2table <- reactive({
      tryCatch({
        # Vérifier si la variable FCS sélectionnée existe
        if (is.null(input$varfcsfilter) || !input$varfcsfilter %in% names(dataset())) {
          showNotification("Food Consumption Score variables are missing. Please ensure all FCS variables (FCSStap, FCSPulse, FCSPr, FCSVeg, FCSFruit, FCSDairy, FCSFat, FCSSugar) are present in your dataset.", 
                         type = "warning", duration = 10)
          return(NULL)
        }
        
        req(dataset()) |> expss::cross_rpct(ADMIN2Name,list(dataset()[input$varfcsfilter]),total_row_position = "none") |>   as_data_frame()|> dplyr::rename(ADMIN2Name = 1) |> mutate(
          ADMIN2Name = stringr::str_replace_all(string = ADMIN2Name,pattern = ".*\\|", replacement = "")
        ) |> mutate(
          ADMIN1Name = maditr::vlookup(lookup_value = ADMIN2Name,dict = dataset(),lookup_column = "ADMIN2Name",result_column ="ADMIN1Name")
        ) |> relocate(c(ADMIN1Name), .before = ADMIN2Name) |> mutate(
            across( where(is.numeric), ~round(.,1)  )
          )
      }, error = function(e) {
        showNotification(paste("Error processing Food Consumption Score data for Admin2:", e$message), 
                       type = "error", duration = 10)
        return(NULL)
      })
    })
    
    output$FCGadm2_barplot <- renderEcharts4r({
      
      data <- FCGadm2table()
      if (is.null(data)) return(NULL)
      
      data %>% filter(ADMIN1Name == req(input$fcsadmin2nom )) |> 
        e_charts(ADMIN2Name) %>%
        e_bar(Poor, name = "Poor", stack = "stack", 
              color = "#E3002B") %>%
        e_bar(Borderline, name = "Borderline", stack = "stack", 
              color = "#E67536") %>%
        e_bar(Acceptable, name = "Acceptable", stack = "stack", 
              color = "#ECE1B1")%>%
        e_labels(position = "inside", 
                 formatter = htmlwidgets::JS("function(params) {return params.value[1] + '%';}")) %>%
        e_y_axis(max = 100, name = "") %>%
        e_x_axis(name = "") %>%
        # e_title("Répartition des catégories FCS par région") %>%
        e_tooltip(trigger = "item") %>%
        e_legend(left = "center") %>%
        e_grid(containLabel = TRUE) |> 
        e_toolbox_feature("saveAsImage")
      
    })
    
    
    
    # rapport html
    # p5html <- reactive({
    #   p5 <- FCGadm2table() %>% filter(ADMIN1Name == req(input$fcsadmin2nom )) %>% ggplot(aes(x=ADMIN2Name,  y = Percentage, fill = FCSCat,text = paste(ADMIN1Name, paste( ADMIN2Name, paste0(Percentage,"%"),sep = " " ),sep = "\n") )) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=fcg_colors)  +labs(x = "", y = "") + scale_y_continuous(labels = scales::percent)
    #   ggplotly(p5,tooltip = "text")
    # })
    
    p5html <- reactive({
      tryCatch({
        data_df <- req(data())
        
        # Check if required variables exist
        if(!safe_check_vars(data_df, c("ADMIN1Name", "ADMIN2Name"))) {
          return(NULL)
        }
        
        # Use the currently selected admin1 filter, fallback to first available if none selected
        selected_admin1 <- input$fcsadmin2nom
        if(is.null(selected_admin1) || selected_admin1 == "") {
          selected_admin1 <- unique(data_df$ADMIN1Name)[1]
        }
        
        if(is.null(selected_admin1)) {
          return(NULL)
        }
        
        # Get FCS data for admin2
        data <- FCGadm2table()
        if(is.null(data)) return(NULL)
        
        # Filter for selected admin1
        filtered_data <- data %>% filter(ADMIN1Name == selected_admin1)
        if(nrow(filtered_data) == 0) {
          return(NULL)
        }
        
        filtered_data |> 
          e_charts(ADMIN2Name) %>%
          e_bar(Poor, name = "Poor", stack = "stack", 
              color = "#E3002B") %>%
        e_bar(Borderline, name = "Borderline", stack = "stack", 
              color = "#E67536") %>%
        e_bar(Acceptable, name = "Acceptable", stack = "stack", 
              color = "#ECE1B1")%>%
        e_labels(position = "inside", 
                 formatter = htmlwidgets::JS("function(params) {return params.value[1] + '%';}")) %>%
        e_y_axis(max = 100, name = "") %>%
        e_x_axis(name = "") %>%
        # e_title("Répartition des catégories FCS par région") %>%
        e_tooltip(trigger = "item",formatter = htmlwidgets::JS("
      function(params) {
        return `${params.data.ADMIN1Name} - ${params.name}<br>
                ${params.seriesName}: ${params.value[1]}%`;
      }
    ")) %>%
        e_legend(left = "center") %>%
        e_grid(containLabel = TRUE) |> 
        e_toolbox_feature("saveAsImage")
      }, error = function(e) {
        cat("Error in p5html:", e$message, "\n")
        return(NULL)
      })
    })
    
    #######################  Enquêteur ########################### 
    
    output$enqueteurfcs <- renderUI({
      selectInput(
        inputId = "fcsenqueteur",label = "ADMIN1Name", choices = unique(data()$ADMIN1Name)
      )
    })
    
    
    countsenumtable2 <- reactive(
      countsenumtable()[,c("EnuName", "n")]
    ) 
    
    # FCGEnumNametable <- reactive({req(dataset())%>% 
    #     group_by( ADMIN1Name, EnuName) %>%  
    #     count(FCSCat28) %>% mutate(perc = 100 * n / sum(n)) %>% ungroup() %>% 
    #     select(-n) %>% mutate_if(is.numeric, round, 1)
    #   # %>% 
    #   # left_join(countsenumtable2(), by = "EnuName") 
    #   # %>% 
    #   
    # }) 
    # %>% bindCache(dataset())
    
    output$fcsseuilenq <- renderUI({
      selectInput(
        inputId = "varfcsfilterenq",label = "Selectionner Seuil FCS", choices = names(dataset()[c("FCSCat28","FCSCat21")]),selected = 1
      )
    })
    
    FCGEnumNametable <- reactive({
      tryCatch({
        # Vérifier si la variable FCS sélectionnée existe
        if (is.null(input$varfcsfilterenq) || !input$varfcsfilterenq %in% names(dataset())) {
          showNotification("Food Consumption Score variables are missing. Please ensure all FCS variables are present in your dataset.", 
                         type = "warning", duration = 10)
          return(NULL)
        }
        
        req(dataset()) |> expss::cross_rpct(EnuName,list(dataset()[input$varfcsfilterenq]),total_row_position = "none") |>   as_data_frame()|> dplyr::rename(EnuName = 1) |> mutate(
          EnuName = stringr::str_replace_all(string = EnuName,pattern = ".*\\|", replacement = "")
        ) |> mutate(
          ADMIN1Name = maditr::vlookup(lookup_value = EnuName,dict = dataset(),lookup_column = "EnuName",result_column ="ADMIN1Name")
        ) |> relocate(c(ADMIN1Name), .before = EnuName) |>
          pivot_longer(cols = 3:5,names_to = "FCSCat",values_to = "Percentage") |> mutate(
            across( where(is.numeric), ~round(.,1)  )
          )
      }, error = function(e) {
        showNotification(paste("Error processing Food Consumption Score data for Enumerators:", e$message), 
                       type = "error", duration = 10)
        return(NULL)
      })
    })
    
    # output$FCGadm1EnumName_barplot <- renderPlotly({
    #   p6 <- req(FCGEnumNametable()) %>% filter(ADMIN1Name == req(input$fcsenqueteur )) %>% ggplot(aes(x=EnuName, y = perc, fill = FCSCat28)) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90))   +labs(x = "", y = "")+scale_fill_manual(values=fcg_colors)+scale_y_continuous(labels = scales::percent)
    #   ggplotly(p6)
      
      
      # + facet_wrap(ADMIN1Name ~ ., scales = "free")
      # req(input$fcsenqueteur))
      # Partie idy
      
      # FCGEnumNametable()  %>% dplyr::filter(ADMIN1Name == req(input$fcsenqueteur )) %>%  plot_ly(x= ~EnuName, y = ~perc, color = ~factor(FCSCat28, levels = c("Poor", "Borderline", "Acceptable")), colors = fcg_colors) %>% 
      #   layout(xaxis=list(title= ""), yaxis= list(title = "")) %>% 
      #   layout(barmode ="stack")
    # })
    
    # ajouter par moi
    
    FCGEnumNametable <- reactive({
      req(dataset()) |> expss::cross_rpct(EnuName,list(dataset()[input$varfcsfilterenq]),total_row_position = "none") |>   as_data_frame()|> dplyr::rename(EnuName = 1) |> mutate(
        EnuName = stringr::str_replace_all(string = EnuName,pattern = ".*\\|", replacement = "")
      ) |> mutate(
        ADMIN1Name = maditr::vlookup(lookup_value = EnuName,dict = dataset(),lookup_column = "EnuName",result_column ="ADMIN1Name")
      ) |> relocate(c(ADMIN1Name), .before = EnuName) |> mutate(
          across( where(is.numeric), ~round(.,1)  )
        )
    })
    
    output$FCGadm1EnumName_barplot <- renderEcharts4r({
      data <- FCGEnumNametable()
      if (is.null(data)) return(NULL)
      
      data %>% filter(ADMIN1Name == req(input$fcsenqueteur )) %>% 
        e_charts(EnuName) %>%
        e_bar(Poor, name = "Poor", stack = "stack", 
              color = "#E3002B") %>%
        e_bar(Borderline, name = "Borderline", stack = "stack", 
              color = "#E67536") %>%
        e_bar(Acceptable, name = "Acceptable", stack = "stack", 
              color = "#ECE1B1")%>%
        e_labels(position = "inside", 
                 formatter = htmlwidgets::JS("function(params) {return params.value[1] + '%';}")) %>%
        e_y_axis(max = 100, name = "") %>%
        e_x_axis(name = "") %>%
        # e_title("Répartition des catégories FCS par région") %>%
        e_tooltip(trigger = "item") %>%
        e_legend(left = "center") %>%
        e_grid(containLabel = TRUE) |> 
        e_toolbox_feature("saveAsImage")
        
    })
    
    
    # partie Day by day FCS
    fcs_dataset <- reactive({
      req(dataset()) |>
        select(ADMIN1Name,EnuName, FCSStap, FCSPulse, FCSDairy, FCSPr, FCSVeg, FCSFruit, FCSFat, FCSSugar) |>
        filter(ADMIN1Name == req(input$fcsenqueteur )) |> 
        pivot_longer(
          cols = c(FCSStap, FCSPulse, FCSDairy, FCSPr, FCSVeg, FCSFruit, FCSFat, FCSSugar),
          names_to = 'FoodGroup',
          values_to = 'Days'
        ) |>
        mutate(
          FoodGroup = factor(FoodGroup, levels = c(
            "FCSStap", "FCSPulse", "FCSDairy", "FCSPr",
            "FCSVeg", "FCSFruit", "FCSFat", "FCSSugar"
          )),
          Days = to_factor(Days)
        ) |>
        group_by(EnuName, FoodGroup) |>
        count(Days, .drop = FALSE) |>
        mutate(perc = 100 * n / sum(n)) |>
        ungroup() |>
        select(-n) |>
        mutate(perc = replace_na(perc, 0)) |>
        mutate(perc = round(perc, 1))
    })
    
    output$fcs_day <- renderPlotly({
      req(fcs_dataset())
      
      p <- ggplot(
        data = fcs_dataset(),
        aes(text = paste("Enumerator:", EnuName,
                         "<br>FoodGroup:", FoodGroup,
                         "<br>Days:", Days,
                         "<br>Percentage:", perc, "%"))
      ) +
        geom_col(aes(fill = Days, y = perc, x = EnuName)) +
        facet_wrap(~FoodGroup) +
        scale_fill_manual(values = c("red", "pink1", "pink3", "palegreen", 
                                     "palegreen2",  "palegreen3", "limegreen",  
                                     "springgreen3")) +
        labs(
          title = "FCS group by Days",
          subtitle = "Per enumerator"
        ) +
        theme(
          strip.text.x = element_text(face = "bold", hjust = 0.5),
          strip.placement = "outside",
          panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = 6)
        ) +
        coord_flip()
      
      ggplotly(p, tooltip = "text") |>
        layout(
          margin = list(l = 150),
          legend = list(orientation = "h", y = -0.1),
          hoverlabel = list(bgcolor = "white")
        ) |>
        config(displayModeBar = TRUE)
    })
    
    # Rapport HTML
    # p6html <- reactive({
    #   p6 <- req(FCGEnumNametable()) %>% filter(ADMIN1Name == req(input$fcsenqueteur )) %>% ggplot(aes(x=EnuName, y = Percentage, fill = FCSCat, text = paste(ADMIN1Name,  paste(EnuName, paste0(Percentage,"%"),sep = "  "), sep = "\n"))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90))   +labs(x = "", y = "")+scale_fill_manual(values=fcg_colors)+scale_y_continuous(labels = scales::percent)
    #   ggplotly(p6, tooltip = "text")
    # })
    
    p6html <- reactive({
      tryCatch({
        data_df <- req(data())
        
        # Check if required variables exist
        if(!safe_check_vars(data_df, c("ADMIN1Name", "EnuName"))) {
          return(NULL)
        }
        
        # Use the currently selected admin1 filter, fallback to first available if none selected
        selected_admin1 <- input$fcsenqueteur
        if(is.null(selected_admin1) || selected_admin1 == "") {
          selected_admin1 <- unique(data_df$ADMIN1Name)[1]
        }
        
        if(is.null(selected_admin1)) {
          return(NULL)
        }
        
        # Get FCS data for enumerators
        data <- FCGEnumNametable()
        if(is.null(data)) return(NULL)
        
        # Filter for selected admin1
        filtered_data <- data %>% filter(ADMIN1Name == selected_admin1)
        if(nrow(filtered_data) == 0) {
          return(NULL)
        }
        
        filtered_data %>% 
        e_charts(EnuName) %>%
        e_bar(Poor, name = "Poor", stack = "stack", 
              color = "#E3002B") %>%
        e_bar(Borderline, name = "Borderline", stack = "stack", 
              color = "#E67536") %>%
        e_bar(Acceptable, name = "Acceptable", stack = "stack", 
              color = "#ECE1B1")%>%
        e_labels(position = "inside", 
                 formatter = htmlwidgets::JS("function(params) {return params.value[1] + '%';}")) %>%
        e_y_axis(max = 100, name = "") %>%
        e_x_axis(name = "") %>%
        # e_title("Répartition des catégories FCS par région") %>%
        e_tooltip(trigger = "item") %>%
        e_legend(left = "center") %>%
        e_grid(containLabel = TRUE) |> 
        e_toolbox_feature("saveAsImage")
      }, error = function(e) {
        cat("Error in p6html:", e$message, "\n")
        return(NULL)
      })
    })
    
    # Boxplot enquêteur
    FCSEnumNametable <- reactive(req(dataset())%>% 
                                   group_by(EnuName) %>% 
                                   summarise(FCS_median = median(FCS)) %>% 
                                   mutate(FCS_outlier = is_outlier(FCS_median))
    ) %>% bindCache(dataset())
    
    # output$FCSEnumNamebox <- renderPlotly({
    #   p7 <- FCSEnumNametable() %>% ggplot(aes(x = "", y =FCS_median)) +geom_boxplot() +geom_point(aes(color = FCS_outlier))+
    #     theme_minimal() +
    #     scale_color_manual(values = c("FALSE" = "#27AE60", "TRUE" = "red"))  
    #   
    #   ggplotly(p7)
    # })
    # ajouter par moi même
    output$FCSEnumNamebox <- renderEcharts4r({
      FCSEnumNametable() |> e_charts() |> 
        e_boxplot(FCS_median) |> 
        e_scatter(FCS_median, FCS_outlier, symbol_size = 8) |>
        e_color(c("#27AE60", "red")) |>
        e_tooltip() |>
        e_x_axis(name = "", show = TRUE) |> 
        e_toolbox_feature("saveAsImage")
      
    })
    
    #  Tableau valeur abberante
    output$FCSOutliertable <- renderDataTable(datatable({
      req(FCSEnumNametable()) %>% filter(FCS_outlier == TRUE)
    }, rownames = FALSE )
    
    )
    
    # FCS Day-by-Day plot for report
    pfcsdayhtml <- reactive({
      req(fcs_dataset())
      
      p <- ggplot(
        data = fcs_dataset(),
        aes(text = paste("Enumerator:", EnuName,
                         "<br>FoodGroup:", FoodGroup,
                         "<br>Days:", Days,
                         "<br>Percentage:", perc, "%"))
      ) +
        geom_col(aes(fill = Days, y = perc, x = EnuName)) +
        facet_wrap(~FoodGroup) +
        scale_fill_manual(values = c("red", "pink1", "pink3", "palegreen", 
                                     "palegreen2",  "palegreen3", "limegreen",  
                                     "springgreen3")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(title = "FCS Day-by-Day Analysis by Enumerator",
             x = "Enumerator", y = "Percentage", fill = "Days")
      
      ggplotly(p, tooltip = "text")
    })
    
    # Céréales
    # couleur <- brewer.pal(4,"RdYIBu")
    tabcereale <- reactive(
      req(dataset()) %>% separate(`@_submission_time`, c("Survey_date", "survey_hour"), sep = " ") %>% 
        filter(FCSStap <= 4) %>% 
        select(ADMIN2Name, ADMIN4Name,HHHName,EnuSupervisorName, EnuName,FCSStap,FCSVeg, Survey_date)
      
    ) %>% bindCache(dataset())
    
    tabcereale2 <- reactive({
      tabcereale() |> mutate(
        Survey_date = as_date(Survey_date)
      )
    })
    
    output$cereale <- renderDataTable(datatable({
      tabcereale2()
    },rownames = FALSE,filter = "top",extensions = c("Buttons"),options=list(dom = 'Blfrtip',
                                                                             buttons=c("csv","excel"),
                                                                             lengthMenu = list(c(12,24,60,-1),c(12,24,60,"All")),
                                                                             scrollX = 300,scroller = TRUE,scrollY = 500, FixeHeader = TRUE 
    )) #%>% formatStyle(6, backgroundColor = "green") pageLength = nrow(tabcereale()
    )
    
    # Céréales légumineuses et lait
    tabcerealelegumelait <- reactive(req(dataset()) %>% separate(`@_submission_time`, c("Survey_date", "survey_hour"), sep = " ")   %>%   filter(FCSStap <= 4)  %>% select(
      ADMIN2Name, ADMIN4Name,HHHName,EnuSupervisorName, EnuName,FCSStap,FCSVeg, FCSDairy, Survey_date
    ) )%>% bindCache(dataset())
    
    tabcerealelegumelait2 <- reactive({
      tabcerealelegumelait() |> mutate(
        Survey_date = as_date(Survey_date)
      )
    })
    
    output$cerealelegumelait <- renderDataTable(datatable({
      tabcerealelegumelait2()
    },rownames = FALSE,filter = "top", extensions = c("Buttons"),options=list(dom = 'Blfrtip',
                                                                              buttons=c("csv","excel"),
                                                                              lengthMenu = list(c(12,24,60,-1),c(12,24,60,"All")
                                                                              ),
                                                                              scrollX = 300,scroller = TRUE,scrollY = 500, FixeHeader = TRUE                                                             
    ))
    )
    # pageLength = nrow(tabcerealelegumelait()
    # Légumes et feuilles
    tablegumefeuille <- reactive(
      req(dataset()) %>% separate(`@_submission_time`, c("Survey_date", "survey_hour"), sep = " ") %>% filter(
        FCSVeg <= 3
      ) %>% select(ADMIN2Name, ADMIN4Name, HHHName,EnuSupervisorName, EnuName, FCSVeg , Survey_date)
    ) %>% bindCache(dataset())
    
    tablegumefeuille2 <- reactive({
      tablegumefeuille() |> mutate(
        Survey_date = as_date(Survey_date)
      )
    })
    
    output$legumefeuille <- renderDataTable(datatable({
      
      tablegumefeuille2()
    },rownames = FALSE,filter = "top", extensions = c("Buttons"),options=list(dom = 'Blfrtip',
                                                                              buttons=c("csv","excel"),
                                                                              lengthMenu = list(c(12,24,60,-1),c(12,24,60,"All")
                                                                              ),
                                                                              scrollX = 300,scroller = TRUE,scrollY = 500, FixeHeader = TRUE 
                                                                              
    ))
    )
    # pageLength = nrow(tablegumefeuille())
    
    # HDSS --------------------------------------------------------------------
    #######################  Admin1 ###########################
    CH_colors <- c("Phase1" = "#ECE1B1","Phase2" = "#E6B068","Phase3" = "#E67536","Phase4" = "#E3002B","Phase5" = "#820000")
    # CH_colors2 <- c("Phase1" = "#ECE1B1","Phase2" = "#E6B068","Phase3" = "#E67536","Phase4" = "#E3002B","Phase5" = "#820000")
    hdds_order <- c("Phase5","Phase4","Phase3","Phase2","Phase1")
    # HDDSphaseadm1table <- reactive(req(dataset())%>%
    #                                  group_by(ADMIN1Name) %>%  count(HDDS_CH) %>%
    #                                  mutate(perc = 100 * n / sum(n)) %>% ungroup() %>%
    #                                  select(-n) %>% mutate_if(is.numeric, round, 1) %>%
    #                                  left_join(countsadm1table(), by = "ADMIN1Name")
    # ) %>% bindCache(dataset())
    # 
    # # output$HDDSadm1_barplot <- renderPlotly({
    # #   p8 <- HDDSphaseadm1table() %>% ggplot(aes(x=ADMIN1Name, y = perc, fill = HDDS_CH,text= paste("Total questionnaire :",n))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=CH_colors) + facet_wrap(. ~ ADMIN1Name, scales = "free_x") +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    # #   ggplotly(p8)
    # # })
    # 
    # output$HDDSadm1_barplot <- renderPlotly({
    #   p8 <- HDDSphaseadm1table() %>% ggplot(aes(x=ADMIN1Name, y = perc, fill = HDDS_CH,text= paste(paste(ADMIN1Name), paste(formattable::percent(perc)/100),sep = " " )))  +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=CH_colors) + facet_wrap(. ~ ADMIN1Name, scales = "free_x") +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p8, tooltip = "text")
    # })
    
    # Ajouter par moi même
    
    HDDSphaseadm1table <- reactive({
      tryCatch({
        # Vérifier si HDDS_CH existe dans le dataset
        data_vars <- names(dataset())
        if (!"HDDS_CH" %in% data_vars) {
          showNotification("HDDS variables are missing. Please ensure all HDDS variables (HDDSStap, HDDSPuls, HDDSDairy, HDDSPr, HDDSVeg, HDDSFruit, HDDSOil, HDDSSugar) are present in your dataset.", 
                         type = "warning", duration = 10)
          return(NULL)
        }
        
        df <- req(dataset())%>%
          expss::cross_rpct(ADMIN1Name,list(HDDS_CH),total_row_position = "none") |> as_tibble() |> dplyr::rename(ADMIN1Name = 1) |> mutate(
            ADMIN1Name = stringr::str_replace_all(string = ADMIN1Name,pattern = ".*\\|", replacement = "")
          ) |> mutate(
            across( where(is.numeric), ~round(.,1)))
        
        df_long <- df %>%
          pivot_longer(
            cols = starts_with("Phase"),
            names_to = "Phase",
            values_to = "Percentage") |>
            mutate(Phase = factor(Phase, levels = hdds_order)) |> 
            arrange(Phase)
        
        return(df_long)
      }, error = function(e) {
        showNotification(paste("Error processing HDDS data:", e$message, "Please check that all required HDDS variables are present in your dataset."), 
                       type = "error", duration = 10)
        return(NULL)
      })
    }) %>% bindCache(dataset())
    
    
    
    output$HDDSadm1_barplot <- renderEcharts4r({
      
      data <- HDDSphaseadm1table()
      if (is.null(data)) return(NULL)
      
      data |> group_by(Phase) %>%
        e_charts(ADMIN1Name) %>%
        e_bar(Percentage, stack = "total", bind = Phase) %>%
        e_color(c("#820000","#E3002B","#E67536","#E6B068","#ECE1B1")) |> 
        # e_color(unname(CH_colors)) %>%
        e_tooltip(trigger = "axis") %>%
        e_legend(left = "center") %>%
        e_y_axis(max = 100, name = "") %>%
        e_labels(
          position = "inside",
          formatter = htmlwidgets::JS("
      function(params) {
        return (params.value[1] || 0) + '%';
      }
    ")
        ) %>%
        e_tooltip(
          formatter = htmlwidgets::JS("
      function(params) {
        return params.seriesName + '<br>' +
               params.value[0] + ': ' + (params.value[1] || 0) + '%';
      }
    ")
        ) |> 
        e_toolbox_feature("saveAsImage")
    })
    
    # text= paste(paste( "Total questionnaire :",n),paste(ADMIN1Name), paste(round(100*perc,1)),sep = "\n" ))) 
    # report admin1
    # phdds1html <- reactive({
    #   p8 <- HDDSphaseadm1table() %>% ggplot(aes(x=ADMIN1Name, y = perc, fill = HDDS_CH,text= paste(paste(ADMIN1Name), paste(formattable::percent(perc)/100),sep = " " ))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=CH_colors) + facet_wrap(. ~ ADMIN1Name, scales = "free_x") +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p8, tooltip = "text")
    # })
    
    phdds1html <- reactive({
      HDDSphaseadm1table() |> group_by(Phase) %>%
        e_charts(ADMIN1Name) %>%
        e_bar(Percentage, stack = "total", bind = Phase) %>%
        e_color(c("#820000","#E3002B","#E67536","#E6B068","#ECE1B1")) |> 
        # e_color(unname(CH_colors)) %>%
        e_tooltip(trigger = "axis") %>%
        e_legend(left = "center") %>%
        e_y_axis(max = 100, name = "") %>%
        e_labels(
          position = "inside",
          formatter = htmlwidgets::JS("
      function(params) {
        return (params.value[1] || 0) + '%';
      }
    ")
        ) %>%
        e_tooltip(
          formatter = htmlwidgets::JS("
      function(params) {
        return params.seriesName + '<br>' +
               params.value[0] + ': ' + (params.value[1] || 0) + '%';
      }
    ")
        ) |> 
        e_toolbox_feature("saveAsImage")
    })
    
    ####################### Admin2 ###########################
    output$hddsAdmin2 <- renderUI({
      selectInput(
        inputId = "admin2hdds",label = "ADMIN1Name", choices = unique(data()$ADMIN1Name)
      )
    })
    
    # HDDSphaseadm2table <- reactive(req(dataset())%>% 
    #                                  group_by(ADMIN1Name, ADMIN2Name) %>%  
    #                                  count(HDDS_CH) %>% mutate(perc = 100 * n / sum(n)) %>% ungroup() %>% 
    #                                  select(-n) %>% mutate_if(is.numeric, round, 1) %>% 
    #                                  left_join(countsadm1adm2table2(), by = "ADMIN2Name")
    # ) %>% bindCache(dataset())
    # 
    # # output$HDDSadm2_barplot <- renderPlotly({
    # #   p9 <- HDDSphaseadm2table() %>% filter(ADMIN1Name == req(input$admin2hdds)) %>%  ggplot(aes(x=ADMIN2Name, y = perc, fill = HDDS_CH)) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=CH_colors)  +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    # #   ggplotly(p9)
    # #   # + facet_wrap(. ~ ADMIN1Name, scales = "free_x")
    # # })
    # 
    # output$HDDSadm2_barplot <- renderPlotly({
    #   p9 <- HDDSphaseadm2table() %>% filter(ADMIN1Name == req(input$admin2hdds)) %>%  ggplot(aes(x=ADMIN2Name, y = perc, fill = HDDS_CH, text = paste(paste(ADMIN1Name), paste(ADMIN2Name,formattable::percent(perc)/100, sep = " "), sep="\n"))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=CH_colors)  +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p9, tooltip = "text")
    # })
    
    # Ajouter par moi même
    
    HDDSphaseadm2table <- reactive({
      tryCatch({
        # Vérifier si HDDS_CH existe dans le dataset
        data_vars <- names(dataset())
        if (!"HDDS_CH" %in% data_vars) {
          showNotification("HDDS variables are missing. Please ensure all HDDS variables are present in your dataset.", 
                         type = "warning", duration = 10)
          return(NULL)
        }
        
        df_hddsadm2 <-  req(dataset())%>% expss::cross_rpct(ADMIN2Name,list(HDDS_CH),total_row_position = "none") |>   as_data_frame()|> dplyr::rename(ADMIN2Name = 1) |> mutate(
          ADMIN2Name = stringr::str_replace_all(string = ADMIN2Name,pattern = ".*\\|", replacement = "")
        ) |> mutate(
          ADMIN1Name = maditr::vlookup(lookup_value = ADMIN2Name,dict = dataset(),lookup_column = "ADMIN2Name",result_column ="ADMIN1Name")
        ) |> relocate(c(ADMIN1Name), .before = ADMIN2Name) |> mutate(
          across( where(is.numeric), ~round(.,1)  )
        )
        
        df_long_hdds_adm2 <- df_hddsadm2 %>%
          pivot_longer(
            cols = starts_with("Phase"),
            names_to = "Phase",
            values_to = "Percentage") |> 
            mutate(Phase = factor(Phase, levels = hdds_order)) |> 
            arrange(Phase)
        
        return(df_long_hdds_adm2)
      }, error = function(e) {
        showNotification(paste("Error processing HDDS data for Admin2:", e$message), 
                       type = "error", duration = 10)
        return(NULL)
      })
    }) %>% bindCache(dataset())
    
    output$HDDSadm2_barplot <- renderEcharts4r({
      data <- HDDSphaseadm2table()
      if (is.null(data)) return(NULL)
      
      data %>% filter(ADMIN1Name == req(input$admin2hdds)) |> group_by(Phase) %>%
        e_charts(ADMIN2Name) %>%
        e_bar(Percentage, stack = "total", bind = Phase) %>%
        e_color(c("#820000","#E3002B","#E67536","#E6B068","#ECE1B1")) |> 
        # e_color(unname(CH_colors)) %>%
        e_tooltip(trigger = "axis") %>%
        e_legend(left = "center") %>%
        e_y_axis(max = 100, name = "") %>%
        e_labels(
          position = "inside",
          formatter = htmlwidgets::JS("
      function(params) {
        return (params.value[1] || 0) + '%';
      }
    ")
        ) %>%
        e_tooltip(
          formatter = htmlwidgets::JS("
      function(params) {
        return params.seriesName + '<br>' +
               params.value[0] + ': ' + (params.value[1] || 0) + '%';
      }
    ")
        ) |> 
        e_toolbox_feature("saveAsImage")
    })
    
    
    # report admin2
    # phdds2html <- reactive({
    #   p9 <- HDDSphaseadm2table() %>% filter(ADMIN1Name == req(input$admin2hdds)) %>%  ggplot(aes(x=ADMIN2Name, y = perc, fill = HDDS_CH, text = paste(paste(ADMIN1Name), paste(ADMIN2Name,formattable::percent(perc)/100, sep = " "), sep="\n"))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=CH_colors)  +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p9, tooltip = "text")
    # })
    
    phdds2html <- reactive({
      tryCatch({
        data_df <- req(data())
        
        # Check if required variables exist
        if(!safe_check_vars(data_df, c("ADMIN1Name", "ADMIN2Name"))) {
          return(NULL)
        }
        
        # Use the currently selected admin1 filter, fallback to first available if none selected
        selected_admin1 <- input$admin2hdds
        if(is.null(selected_admin1) || selected_admin1 == "") {
          selected_admin1 <- unique(data_df$ADMIN1Name)[1]
        }
        
        if(is.null(selected_admin1)) {
          return(NULL)
        }
        
        # Get HDDS data for admin2
        data <- HDDSphaseadm2table()
        if(is.null(data)) return(NULL)
        
        # Filter for selected admin1
        filtered_data <- data %>% filter(ADMIN1Name == selected_admin1)
        if(nrow(filtered_data) == 0) {
          return(NULL)
        }
        
        filtered_data |> group_by(Phase) %>%
        e_charts(ADMIN2Name) %>%
        e_bar(Percentage, stack = "total", bind = Phase) %>%
        e_color(c("#820000","#E3002B","#E67536","#E6B068","#ECE1B1")) |> 
        # e_color(unname(CH_colors)) %>%
        e_tooltip(trigger = "axis") %>%
        e_legend(left = "center") %>%
        e_y_axis(max = 100, name = "") %>%
        e_labels(
          position = "inside",
          formatter = htmlwidgets::JS("
      function(params) {
        return (params.value[1] || 0) + '%';
      }
    ")
        ) %>%
        e_tooltip(
          formatter = htmlwidgets::JS("
      function(params) {
        return params.seriesName + '<br>' +
               params.value[0] + ': ' + (params.value[1] || 0) + '%';
      }
    ")
        ) |> 
        e_toolbox_feature("saveAsImage")
    }) 
      # ajou parenthese
    })
    
    #######################  Enquêteurs ########################### 
    output$hddsenqueteur <- renderUI({
      selectInput(
        inputId = "enqueteurhdds",label = "ADMIN1Name", choices = unique(data()$ADMIN1Name)
      )
    })
    
    
    # HDDSadm1EnumNametable <- reactive(req(dataset())%>% 
    #                                     group_by(EnuName,  ADMIN1Name) %>%  count(HDDS_CH) %>% 
    #                                     mutate(perc = 100 * n / sum(n)) %>% ungroup() %>% select(-n) %>% 
    #                                     mutate_if(is.numeric, round, 1)%>% 
    #                                     left_join(countsenumtable2(), by = "EnuName")
    # ) %>% bindCache(dataset())
    
    # output$HDDSadm1EnumName_barplot <- renderPlotly({
    #   p10 <- HDDSadm1EnumNametable() %>% filter(ADMIN1Name == req(input$enqueteurhdds)) %>% ggplot(aes(x=EnuName, y = perc, fill = HDDS_CH)) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=CH_colors)  +labs(x = "", y = "") +theme(axis.text.x=element_blank()) +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p10)
    #   # + facet_wrap(ADMIN1Name ~ ., scales = "free")
    #   # ,text= paste("Total questionnaire :",n)
    # })
    
    # output$HDDSadm1EnumName_barplot <- renderPlotly({
    #   p10 <- HDDSadm1EnumNametable() %>% filter(ADMIN1Name == req(input$enqueteurhdds)) %>% ggplot(aes(x=EnuName, y = perc, fill = HDDS_CH, text = paste(paste(ADMIN1Name), paste(EnuName,formattable::percent(perc)/100, sep = " "), sep="\n"))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=CH_colors)  +labs(x = "", y = "")  +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p10, tooltip = "text")
    # })
    
    # Ajouter par moi même
    HDDSadm1EnumNametable <- reactive({
      tryCatch({
        # Vérifier si HDDS_CH existe dans le dataset
        data_vars <- names(dataset())
        if (!"HDDS_CH" %in% data_vars) {
          showNotification("HDDS variables are missing. Please ensure all HDDS variables are present in your dataset.", 
                         type = "warning", duration = 10)
          return(NULL)
        }
        
        df_hddsenum <-   req(dataset()) |> expss::cross_rpct(EnuName,list(HDDS_CH),total_row_position = "none") |>   as_data_frame()|> dplyr::rename(EnuName = 1) |> mutate(
          EnuName = stringr::str_replace_all(string = EnuName,pattern = ".*\\|", replacement = "")
        ) |> mutate(
          ADMIN1Name = maditr::vlookup(lookup_value = EnuName,dict = dataset(),lookup_column = "EnuName",result_column ="ADMIN1Name")
        ) |> relocate(c(ADMIN1Name), .before = EnuName) |> mutate(
          across( where(is.numeric), ~round(.,1)  )
        )
        
        df_long_hdds_enum <- df_hddsenum %>%
          pivot_longer(
            cols = starts_with("Phase"),
            names_to = "Phase",
            values_to = "Percentage") |> 
            mutate(Phase = factor(Phase, levels = hdds_order)) |> 
            arrange(Phase)
        
        return(df_long_hdds_enum)
      }, error = function(e) {
        showNotification(paste("Error processing HDDS data for Enumerators:", e$message), 
                       type = "error", duration = 10)
        return(NULL)
      })
    })
    
    output$HDDSadm1EnumName_barplot <- renderEcharts4r({
      
      data <- HDDSadm1EnumNametable()
      if (is.null(data)) return(NULL)
      
      data %>% filter(ADMIN1Name == req(input$enqueteurhdds)) |> 
        group_by(Phase) %>%
        e_charts(EnuName) %>%
        e_bar(Percentage, stack = "total", bind = Phase) %>%
        e_color(c("#820000","#E3002B","#E67536","#E6B068","#ECE1B1")) |> 
        # e_color(unname(CH_colors)) %>%
        e_tooltip(trigger = "axis") %>%
        e_legend(left = "center") %>%
        e_y_axis(max = 100, name = "") %>%
        e_labels(
          position = "inside",
          formatter = htmlwidgets::JS("
      function(params) {
        return (params.value[1] || 0) + '%';
      }
    ")
        ) %>%
        e_tooltip(
          formatter = htmlwidgets::JS("
      function(params) {
        return params.seriesName + '<br>' +
               params.value[0] + ': ' + (params.value[1] || 0) + '%';
      }
    ")
        ) |> 
        e_toolbox_feature("saveAsImage")
      
    })
    
    # report enquêteur
    # phdds3html <- reactive({
    #   p10 <- HDDSadm1EnumNametable() %>% filter(ADMIN1Name == req(input$enqueteurhdds)) %>% ggplot(aes(x=EnuName, y = perc, fill = HDDS_CH, text = paste(paste(ADMIN1Name), paste(EnuName,formattable::percent(perc)/100, sep = " "), sep="\n"))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=CH_colors)  +labs(x = "", y = "")  +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p10, tooltip = "text")
    # })
    
    phdds3html <- reactive({
      tryCatch({
        data_df <- req(data())
        
        # Check if required variables exist
        if(!safe_check_vars(data_df, c("ADMIN1Name", "EnuName"))) {
          return(NULL)
        }
        
        # Use the currently selected admin1 filter, fallback to first available if none selected
        selected_admin1 <- input$enqueteurhdds
        if(is.null(selected_admin1) || selected_admin1 == "") {
          selected_admin1 <- unique(data_df$ADMIN1Name)[1]
        }
        
        if(is.null(selected_admin1)) {
          return(NULL)
        }
        
        # Get HDDS data for enumerators and filter
        enum_data <- HDDSadm1EnumNametable()
        if(is.null(enum_data)) return(NULL)
        
        filtered_data <- enum_data %>% filter(ADMIN1Name == selected_admin1)
        if(nrow(filtered_data) == 0) {
          return(NULL)
        }
        
        filtered_data |> 
          group_by(Phase) %>%
          e_charts(EnuName) %>%
          e_bar(Percentage, stack = "total", bind = Phase) %>%
          e_color(c("#820000","#E3002B","#E67536","#E6B068","#ECE1B1")) |> 
          # e_color(unname(CH_colors)) %>%
          e_tooltip(trigger = "axis") %>%
          e_legend(left = "center") %>%
          e_y_axis(max = 100, name = "") %>%
          e_labels(
            position = "inside",
            formatter = htmlwidgets::JS("
        function(params) {
          return (params.value[1] || 0) + '%';
        }
      ")
          ) %>%
          e_tooltip(
            formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + '<br>' +
                 params.value[0] + ': ' + (params.value[1] || 0) + '%';
        }
      ")
          ) |> 
          e_toolbox_feature("saveAsImage")
      }, error = function(e) {
        cat("Error in phdds3html:", e$message, "\n")
        return(NULL)
      })
    })
    
    # rCSI --------------------------------------------------------------------
    #######################  Admin1 ###########################
    rCSI_colors = c("Phase1" = "#ECE1B1","Phase2" = "#E6B068","Phase3" = "#E67536")
    rcsi_order <- c("Phase3","Phase2","Phase1")
    # rCSIphaseadm1table <- reactive(req(dataset())%>%
    #                                  group_by(ADMIN1Name) %>%  count(rCSI_CH) %>% 
    #                                  mutate(perc = 100 * n / sum(n)) %>% ungroup() %>% 
    #                                  select(-n) %>% mutate_if(is.numeric, round, 1) %>%
    #                                  left_join(countsadm1table(), by = "ADMIN1Name")
    # ) %>% bindCache(dataset())
    # 
    # # output$rCSIadm1_barplot <- renderPlotly({
    # #   p11 <- rCSIphaseadm1table() %>% ggplot(aes(x=ADMIN1Name, y = perc, fill = rCSI_CH,text= paste("Total questionnaire :",n))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=rCSI_colors) + facet_grid(. ~ ADMIN1Name, scales = "free_x") +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    # #   ggplotly(p11)
    # # })
    # output$rCSIadm1_barplot <- renderPlotly({
    #   p11 <- rCSIphaseadm1table() %>% ggplot(aes(x=ADMIN1Name, y = perc, fill = rCSI_CH,text= paste(paste(ADMIN1Name), paste(formattable::percent(perc)/100),sep = " " ))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=rCSI_colors) + facet_grid(. ~ ADMIN1Name, scales = "free_x") +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p11, tooltip = "text")
    # })
    
    # Ajouter par moi
    rCSIphaseadm1table <- reactive( { df_rcsi_adm1 <- req(dataset())%>%
        expss::cross_rpct(ADMIN1Name,list(rCSI_CH),total_row_position = "none") |> as_tibble() |> dplyr::rename(ADMIN1Name = 1) |> mutate(
          ADMIN1Name = stringr::str_replace_all(string = ADMIN1Name,pattern = ".*\\|", replacement = "")
        ) |> mutate(
          across( where(is.numeric), ~round(.,1)))
      
      df_long_rcsi_adm1 <- df_rcsi_adm1 %>%
        pivot_longer(
          cols = starts_with("Phase"),
          names_to = "Phase",
          values_to = "Percentage") |> 
          mutate(Phase = factor(Phase, levels = rcsi_order)) |> 
          arrange(Phase)
      
    }) %>% bindCache(dataset())
    
    output$rCSIadm1_barplot <- renderEcharts4r({
      tryCatch({
        data_df <- data()
        if(is.null(data_df)) return(NULL)
        
        # Check if variables are available for this section
        rcsi_vars <- c("rCSILessQlty", "rCSIBorrow", "rCSIMealSize", "rCSIMealAdult", "rCSIMealNb")
        missing_vars <- rcsi_vars[!rcsi_vars %in% names(data_df)]
        
        # If more than half of rCSI variables are missing, show notification
        if(length(missing_vars) > length(rcsi_vars) / 2) {
          show_section_notification("rcsi", missing_vars)
          return(NULL)
        }
        
        # Check if processed rCSI data exists in dataset
        dataset_data <- dataset()
        if(is.null(dataset_data) || !"rCSI_CH" %in% names(dataset_data)) {
          show_section_notification("rcsi", missing_vars)
          return(NULL)
        }
        
        # If some variables are missing, show a warning but continue
        if(length(missing_vars) > 0) {
          showNotification(
            paste0("Warning: rCSI analysis is incomplete. Missing variables: ", 
                  paste(missing_vars, collapse = ", "), 
                  ". Results may not be fully accurate."),
            type = "warning", 
            duration = 8
          )
        }
        
        rcsi_data <- rCSIphaseadm1table()
        if(is.null(rcsi_data) || nrow(rcsi_data) == 0) {
          show_error_notification("rCSI Analysis", "No rCSI data available for analysis")
          return(NULL)
        }
        
        rcsi_data |> 
          group_by(Phase) %>%
          e_charts(ADMIN1Name) %>%
          e_bar(Percentage, stack = "total", bind = Phase) %>%
          e_color(c("#E67536", "#E6B068", "#ECE1B1")) |> 
          e_tooltip(trigger = "axis") %>%
          e_legend(left = "center") %>%
          e_y_axis(max = 100, name = "") %>%
          e_labels(
            position = "inside",
            formatter = htmlwidgets::JS("
        function(params) {
          return (params.value[1] || 0) + '%';
        }
      ")
          ) %>%
          e_tooltip(
            formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + '<br>' +
                 params.value[0] + ': ' + (params.value[1] || 0) + '%';
        }
      ")
          ) |> 
          e_toolbox_feature("saveAsImage")
          
      }, error = function(e) {
        show_error_notification("rCSI Analysis", e$message)
        return(NULL)
      })
    })
    
    
    # report admin1
    # prcsihtml <- reactive({
    #   p11 <- rCSIphaseadm1table() %>% ggplot(aes(x=ADMIN1Name, y = perc, fill = rCSI_CH,text= paste(paste(ADMIN1Name), paste(formattable::percent(perc)/100),sep = " " ))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=rCSI_colors) + facet_grid(. ~ ADMIN1Name, scales = "free_x") +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p11, tooltip = "text")
    # })
    
    prcsihtml <- reactive({
      rCSIphaseadm1table() |> group_by(Phase) %>%
        e_charts(ADMIN1Name) %>%
        e_bar(Percentage, stack = "total", bind = Phase) %>%
        e_color(c("#E67536", "#E6B068", "#ECE1B1")) |> 
        # e_color(unname(rCSI_colors)) %>%
        e_tooltip(trigger = "axis") %>%
        e_legend(left = "center") %>%
        e_y_axis(max = 100, name = "") %>%
        e_labels(
          position = "inside",
          formatter = htmlwidgets::JS("
      function(params) {
        return (params.value[1] || 0) + '%';
      }
    ")
        ) %>%
        e_tooltip(
          formatter = htmlwidgets::JS("
      function(params) {
        return params.seriesName + '<br>' +
               params.value[0] + ': ' + (params.value[1] || 0) + '%';
      }
    ")
        ) |> 
        e_toolbox_feature("saveAsImage")
    })
    
    #######################  Admin2 ###########################
    
    output$rcsiAdmin2 <- renderUI({
      selectInput(
        inputId = "admin2rcsi",label = "ADMIN1Name", choices = unique(data()$ADMIN1Name)
      )
    })
    
    rCSIphaseadm2table <- reactive(req(dataset())%>% 
                                     group_by(ADMIN1Name, ADMIN2Name) %>%  count(rCSI_CH) %>% 
                                     mutate(perc = 100 * n / sum(n)) %>% ungroup() %>% 
                                     select(-n) %>% mutate_if(is.numeric, round, 1) %>% 
                                     left_join(countsadm1adm2table2(), by = "ADMIN2Name")
    ) %>% bindCache(dataset())
    
    # output$rCSIadm2_barplot <- renderPlotly({
    #   p12 <- rCSIphaseadm2table() %>% filter(ADMIN1Name == req(input$admin2rcsi))  %>% ggplot(aes(x=ADMIN2Name, y = perc, fill = rCSI_CH)) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=rCSI_colors) +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p12)
    #   # + facet_grid(. ~ ADMIN1Name, scales = "free_x") 
    # })
    output$rCSIadm2_barplot <- renderPlotly({
      p12 <- rCSIphaseadm2table() %>% filter(ADMIN1Name == req(input$admin2rcsi))  %>% ggplot(aes(x=ADMIN2Name, y = perc, fill = rCSI_CH,text = paste(paste(ADMIN1Name), paste(ADMIN2Name,formattable::percent(perc)/100, sep = " "), sep="\n"))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=rCSI_colors) +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
      ggplotly(p12,tooltip = "text")
    })
    # Ajouter par moi 
    
    rCSIphaseadm2table <- reactive({ df_rcsiadm2 <- req(dataset()) |> expss::cross_rpct(ADMIN2Name,list(rCSI_CH),total_row_position = "none") |>   as_data_frame()|> dplyr::rename(ADMIN2Name = 1) |> mutate(
      ADMIN2Name = stringr::str_replace_all(string = ADMIN2Name,pattern = ".*\\|", replacement = "")
    ) |> mutate(
      ADMIN1Name = maditr::vlookup(lookup_value = ADMIN2Name,dict = dataset(),lookup_column = "ADMIN2Name",result_column ="ADMIN1Name")
    ) |> relocate(c(ADMIN1Name), .before = ADMIN2Name) |> mutate(
      across( where(is.numeric), ~round(.,1)  )
    )
      
      df_long_rcsiadm2 <- df_rcsiadm2 %>%
        pivot_longer(
          cols = starts_with("Phase"),
          names_to = "Phase",
          values_to = "Percentage") |> 
        mutate(Phase = factor(Phase, levels = rcsi_order)) |> 
        arrange(Phase)  
      })
    
    output$rCSIadm2_barplot <- renderEcharts4r({
      rCSIphaseadm2table() %>% filter(ADMIN1Name == req(input$admin2rcsi)) |> group_by(Phase) %>%
        e_charts(ADMIN2Name) %>%
        e_bar(Percentage, stack = "total", bind = Phase) %>%
        e_color(c("#E67536", "#E6B068", "#ECE1B1")) |> 
        # e_color(unname(rCSI_colors)) %>%
        e_tooltip(trigger = "axis") %>%
        e_legend(left = "center") %>%
        e_y_axis(max = 100, name = "") %>%
        e_labels(
          position = "inside",
          formatter = htmlwidgets::JS("
      function(params) {
        return (params.value[1] || 0) + '%';
      }
    ")
        ) %>%
        e_tooltip(
          formatter = htmlwidgets::JS("
      function(params) {
        return params.seriesName + '<br>' +
               params.value[0] + ': ' + (params.value[1] || 0) + '%';
      }
    ")
        ) |> 
        e_toolbox_feature("saveAsImage")
      
    })
    
    # export report
    # prcsi2html <- reactive({
    #   p12 <- rCSIphaseadm2table() %>% filter(ADMIN1Name == req(input$admin2rcsi))  %>% ggplot(aes(x=ADMIN2Name, y = perc, fill = rCSI_CH,text = paste(paste(ADMIN1Name), paste(ADMIN2Name,formattable::percent(perc)/100, sep = " "), sep="\n"))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=rCSI_colors) +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p12,tooltip = "text")
    # })
    
    prcsi2html <- reactive({
      rCSIphaseadm2table() %>% filter(ADMIN1Name == req(input$admin2rcsi)) |> group_by(Phase) %>%
        e_charts(ADMIN2Name) %>%
        e_bar(Percentage, stack = "total", bind = Phase) %>%
        e_color(c("#E67536", "#E6B068", "#ECE1B1")) |> 
        # e_color(unname(rCSI_colors)) %>%
        e_tooltip(trigger = "axis") %>%
        e_legend(left = "center") %>%
        e_y_axis(max = 100, name = "") %>%
        e_labels(
          position = "inside",
          formatter = htmlwidgets::JS("
      function(params) {
        return (params.value[1] || 0) + '%';
      }
    ")
        ) %>%
        e_tooltip(
          formatter = htmlwidgets::JS("
      function(params) {
        return params.seriesName + '<br>' +
               params.value[0] + ': ' + (params.value[1] || 0) + '%';
      }
    ")
        ) |> 
        e_toolbox_feature("saveAsImage")
    })
    
    #######################  Enquêteurs ########################### 
    
    output$rcsienqueteur <- renderUI({
      selectInput(
        inputId = "enqueteurrcsi",label = "ADMIN1Name", choices = unique(data()$ADMIN1Name)
      )
    })
    
    # rCSIEnumNametable <- reactive(req(dataset())%>% 
    #                                 group_by(EnuName, ADMIN1Name) %>%  
    #                                 count(rCSI_CH) %>% mutate(perc = 100 * n / sum(n)) %>% ungroup() %>% 
    #                                 select(-n) %>% mutate_if(is.numeric, round, 1)%>% 
    #                                 left_join(countsenumtable2(), by = "EnuName")
    # ) %>% bindCache(dataset())
    # 
    # # output$rCSIadm1EnumName_barplot <- renderPlotly({
    # #   p13 <- rCSIEnumNametable() %>% filter(ADMIN1Name == req(input$enqueteurrcsi)) %>% ggplot(aes(x=EnuName, y = perc, fill = rCSI_CH)) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=rCSI_colors)  +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    # #   ggplotly(p13)
    # #   # + facet_wrap(. ~ ADMIN1Name, scales = "free")
    # # })
    # 
    # output$rCSIadm1EnumName_barplot <- renderPlotly({
    #   p13 <- rCSIEnumNametable() %>% filter(ADMIN1Name == req(input$enqueteurrcsi)) %>% ggplot(aes(x=EnuName, y = perc, fill = rCSI_CH,text = paste(paste(ADMIN1Name), paste(EnuName,formattable::percent(perc)/100, sep = " "), sep="\n"))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=rCSI_colors)  +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p13,tooltip = "text")
    # })
    
    # Ajouter par moi
    rCSIEnumNametable <- reactive({ df_rcsienum <-  req(dataset()) |> 
      expss::cross_rpct(EnuName,list(rCSI_CH),total_row_position = "none") |>   as_data_frame()|> dplyr::rename(EnuName = 1) |> mutate(
        EnuName = stringr::str_replace_all(string = EnuName,pattern = ".*\\|", replacement = "")
      ) |> mutate(
        ADMIN1Name = maditr::vlookup(lookup_value = EnuName,dict = dataset(),lookup_column = "EnuName",result_column ="ADMIN1Name")
      ) |> relocate(c(ADMIN1Name), .before = EnuName) |> mutate(
        across( where(is.numeric), ~round(.,1)  )
      )
      
      df_long_rcsienum <- df_rcsienum %>%
        pivot_longer(
          cols = starts_with("Phase"),
          names_to = "Phase",
          values_to = "Percentage") |> 
          mutate(Phase = factor(Phase, levels = rcsi_order)) |> 
          arrange(Phase)
      })
    
    output$rCSIadm1EnumName_barplot <- renderEcharts4r({
      rCSIEnumNametable() %>% filter(ADMIN1Name == req(input$enqueteurrcsi)) |> 
        group_by(Phase) %>%
        e_charts(EnuName) %>%
        e_bar(Percentage, stack = "total", bind = Phase) %>%
        e_color(c("#E67536", "#E6B068", "#ECE1B1")) |> 
        # e_color(unname(rCSI_colors)) %>%
        e_tooltip(trigger = "axis") %>%
        e_legend(left = "center") %>%
        e_y_axis(max = 100, name = "") %>%
        e_labels(
          position = "inside",
          formatter = htmlwidgets::JS("
      function(params) {
        return (params.value[1] || 0) + '%';
      }
    ")
        ) %>%
        e_tooltip(
          formatter = htmlwidgets::JS("
      function(params) {
        return params.seriesName + '<br>' +
               params.value[0] + ': ' + (params.value[1] || 0) + '%';
      }
    ")
        ) |> 
        e_toolbox_feature("saveAsImage")
    })
    
    
    # partie rCSI by day
    rcsi_data <- reactive({
      req(dataset()) |>
        select(ADMIN1Name,EnuName, rCSILessQlty, rCSIBorrow, rCSIMealNb, rCSIMealAdult, rCSIMealSize) |>
        filter(ADMIN1Name == req(input$enqueteurrcsi)) |> 
        pivot_longer(
          cols = c(rCSILessQlty, rCSIBorrow, rCSIMealNb, rCSIMealAdult, rCSIMealSize),
          names_to = 'Strategy',
          values_to = 'Days'
        ) |>
        mutate(
          Strategy = factor(Strategy, levels = c(
            "rCSILessQlty", "rCSIBorrow", "rCSIMealNb", "rCSIMealAdult", "rCSIMealSize")),
          Days = as.factor(Days)
        ) |>
        group_by(EnuName, Strategy) |>
        count(Days, .drop = FALSE) |>
        mutate(perc = 100 * n / sum(n)) |>
        ungroup() |>
        select(-n) |>
        mutate(perc = replace_na(perc, 0)) |>
        mutate(perc = round(perc, 1))
    })

    output$rcsi_day <- renderPlotly({
      req(rcsi_data())

      p <- ggplot(
        data = rcsi_data(),
        aes(text = paste("Enumerator:", EnuName,
                         "<br>Strategy:", Strategy,
                         "<br>Days:", Days,
                         "<br>Percentage:", perc, "%"))
      ) +
        geom_col(aes(fill = Days, y = perc, x = EnuName)) +
        facet_wrap(~Strategy) +
        scale_fill_manual(values = c("springgreen3", "limegreen", "palegreen3",
                                     "orange", "pink1", "indianred1", "red",
                                     "pink1")) +
        labs(
          title = "RCSI by Days",
          subtitle = "Per enumerator"
        ) +
        theme(
          strip.text.x = element_text(face = "bold", hjust = 0.5),
          strip.placement = "outside",
          panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = 6)
        ) +
        coord_flip()

      ggplotly(p, tooltip = "text") |>
        layout(
          margin = list(l = 150),
          legend = list(orientation = "h", y = -0.1),
          hoverlabel = list(bgcolor = "white")
        ) |>
        config(displayModeBar = TRUE)
    })


    # export report
    # prcsi3html <- reactive({
    #   p13 <- rCSIEnumNametable() %>% filter(ADMIN1Name == req(input$enqueteurrcsi)) %>% ggplot(aes(x=EnuName, y = perc, fill = rCSI_CH, text = paste(paste(ADMIN1Name), paste(EnuName,formattable::percent(perc)/100, sep = " "), sep="\n"))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=rCSI_colors)  +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p13,tooltip = "text")
    # })
    
    prcsi3html <- reactive({
      rCSIEnumNametable() %>% filter(ADMIN1Name == req(input$enqueteurrcsi)) |> 
        group_by(Phase) %>%
        e_charts(EnuName) %>%
        e_bar(Percentage, stack = "total", bind = Phase) %>%
        e_color(c("#E67536", "#E6B068", "#ECE1B1")) |> 
        # e_color(unname(rCSI_colors)) %>%
        e_tooltip(trigger = "axis") %>%
        e_legend(left = "center") %>%
        e_y_axis(max = 100, name = "") %>%
        e_labels(
          position = "inside",
          formatter = htmlwidgets::JS("
      function(params) {
        return (params.value[1] || 0) + '%';
      }
    ")
        ) %>%
        e_tooltip(
          formatter = htmlwidgets::JS("
      function(params) {
        return params.seriesName + '<br>' +
               params.value[0] + ': ' + (params.value[1] || 0) + '%';
      }
    ")
        ) |> 
        e_toolbox_feature("saveAsImage")
    })
    
    # Boxplot enquêteur
    rcsiEnumNametable <- reactive(req(dataset())%>% 
                                    group_by(EnuName) %>% 
                                    summarise(rcsi_median = median(rCSI)) %>% 
                                    mutate(rcsi_outlier = is_outlier(rcsi_median))
    ) %>% bindCache(dataset())
    
    # output$rcsiEnumNamebox <- renderPlotly({
    #   p14 <- rcsiEnumNametable() %>% ggplot(aes(x = "", y =rcsi_median)) +geom_boxplot() +geom_point(aes(color = rcsi_outlier))
    #   ggplotly(p14)
    # })
    
    output$rcsiEnumNamebox <- renderEcharts4r({
        rcsiEnumNametable() %>% 
        e_charts() |> 
        e_boxplot(rcsi_median) |> 
        e_scatter(rcsi_median, rcsi_outlier, symbol_size = 8) |>
        e_color(c("#27AE60", "red")) |>
        e_tooltip() |>
        e_x_axis(name = "", show = TRUE) |> 
        e_toolbox_feature("saveAsImage")
    })
    
    #  Tableau valeur abberante
    output$rcsiOutliertable <- renderDataTable(datatable({
      rcsiEnumNametable() %>% filter(rcsi_outlier == TRUE)
    }, rownames = FALSE )
    
    )
    
    # rCSI Day-by-Day plot for report
    prcsidayhtml <- reactive({
      req(rcsi_data())
      
      p <- ggplot(
        data = rcsi_data(),
        aes(text = paste("Enumerator:", EnuName,
                         "<br>Strategy:", Strategy,
                         "<br>Days:", Days,
                         "<br>Percentage:", perc, "%"))
      ) +
        geom_col(aes(fill = Days, y = perc, x = EnuName)) +
        facet_wrap(~Strategy) +
        scale_fill_manual(values = c("springgreen3", "limegreen", "palegreen3",
                                     "orange", "pink1", "indianred1", "red",
                                     "pink1")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(title = "rCSI Day-by-Day Analysis by Enumerator",
             x = "Enumerator", y = "Percentage", fill = "Days")
      
      ggplotly(p, tooltip = "text")
    })
    
    # indice de la faim HHS ---------------------------------------------------
    #######################  Admin1 ###########################
    CH_colors = c("Phase1" = "#ECE1B1","Phase2" = "#E6B068","Phase3" = "#E67536","Phase4" = "#E3002B", "Phase5" = "#820000")
    hhs_order <- c("Phase5","Phase4","Phase3","Phase2","Phase1")
    
    # HHSphaseadm1table <- reactive(req(dataset())%>%
    #                                 group_by(ADMIN1Name) %>%  count(HHhS_CH) %>% 
    #                                 mutate(perc = 100 * n / sum(n)) %>% ungroup() %>% 
    #                                 select(-n) %>% mutate_if(is.numeric, round, 1)%>%
    #                                 left_join(countsadm1table(), by = "ADMIN1Name")
    # ) %>% bindCache(dataset())
    # 
    # # output$HHSadm1_barplot <- renderPlotly({
    # #   p15 <- HHSphaseadm1table() %>% ggplot(aes(x=ADMIN1Name, y = perc, fill = HHhS_CH,text= paste("Total questionnaire :",n))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=CH_colors) + facet_grid(. ~ ADMIN1Name, scales = "free_x") +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    # #   ggplotly(p15)
    # # })
    # 
    # output$HHSadm1_barplot <- renderPlotly({
    #   p15 <- HHSphaseadm1table() %>% ggplot(aes(x=ADMIN1Name, y = perc, fill = HHhS_CH,text= paste(paste(ADMIN1Name), paste(formattable::percent(perc)/100),sep = " " ))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=CH_colors) + facet_grid(. ~ ADMIN1Name, scales = "free_x") +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p15, tooltip = "text")
    # })
    
    # Ajouter par moi
    HHSphaseadm1table <- reactive({
      tryCatch({
        # Vérifier si HHhS_CH existe dans le dataset
        data_vars <- names(dataset())
        if (!"HHhS_CH" %in% data_vars) {
          showNotification("Household Hunger Scale variables are missing. Please ensure all HHS variables (HHSNoFood_FR, HHSBedHung_FR, HHSNotEat_FR) are present in your dataset.", 
                         type = "warning", duration = 10)
          return(NULL)
        }
        
        df_hhs_adm1 <- req(dataset()) |> 
          expss::cross_rpct(ADMIN1Name,list(HHhS_CH),total_row_position = "none") |> as_tibble() |> dplyr::rename(ADMIN1Name = 1) |> mutate(
            ADMIN1Name = stringr::str_replace_all(string = ADMIN1Name,pattern = ".*\\|", replacement = "")
          ) |> mutate(
            across( where(is.numeric), ~round(.,1)))
        
        df_long_hhs_adm1 <- df_hhs_adm1 %>%
          pivot_longer(
            cols = starts_with("Phase"),
            names_to = "Phase",
            values_to = "Percentage") |> 
            mutate(Phase = factor(Phase, levels = hhs_order)) |> 
            arrange(Phase)
        
        return(df_long_hhs_adm1)
      }, error = function(e) {
        showNotification(paste("Error processing Household Hunger Scale data:", e$message, "Please check that all required HHS variables are present."), 
                       type = "error", duration = 10)
        return(NULL)
      })
    }) %>% bindCache(dataset())
    
    
    output$HHSadm1_barplot <- renderEcharts4r({
      tryCatch({
        data_df <- data()
        if(is.null(data_df)) return(NULL)
        
        # Check if variables are available for this section
        hhs_vars <- c("HHSNoFood_FR", "HHSBedHung_FR", "HHSNotEat_FR")
        missing_vars <- hhs_vars[!hhs_vars %in% names(data_df)]
        
        # HHS requires all 3 variables for accurate analysis
        if(length(missing_vars) > 1) {
          show_section_notification("hhs", missing_vars)
          return(NULL)
        }
        
        # Check if processed HHS data exists in dataset
        dataset_data <- dataset()
        if(is.null(dataset_data) || !"HHhS_CH" %in% names(dataset_data)) {
          show_section_notification("hhs", missing_vars)
          return(NULL)
        }
        
        # If one variable is missing, show a warning but continue
        if(length(missing_vars) > 0) {
          showNotification(
            paste0("Warning: HHS analysis is incomplete. Missing variable: ", 
                  paste(missing_vars, collapse = ", "), 
                  ". Results may not be fully accurate."),
            type = "warning", 
            duration = 8
          )
        }
        
        hhs_data <- HHSphaseadm1table()
        if(is.null(hhs_data) || nrow(hhs_data) == 0) {
          show_error_notification("HHS Analysis", "No HHS data available for analysis")
          return(NULL)
        }
        
        hhs_data |> group_by(Phase) %>%
          e_charts(ADMIN1Name) %>%
          e_bar(Percentage, stack = "total", bind = Phase) %>%
          e_color(c("#820000","#E3002B","#E67536","#E6B068","#ECE1B1")) |> 
          e_tooltip(trigger = "axis") %>%
          e_legend(left = "center") %>%
          e_y_axis(max = 100, name = "") %>%
          e_labels(
            position = "inside",
            formatter = htmlwidgets::JS("
        function(params) {
          return (params.value[1] || 0) + '%';
        }
      ")
          ) %>%
          e_tooltip(
            formatter = htmlwidgets::JS("
        function(params) {
          return params.seriesName + '<br>' +
                 params.value[0] + ': ' + (params.value[1] || 0) + '%';
        }
      ")
          ) |> 
          e_toolbox_feature("saveAsImage")
          
      }, error = function(e) {
        show_error_notification("HHS Analysis", e$message)
        return(NULL)
      })
    })
    
    # export report
    # phhs1html <- reactive({
    #   p15 <- HHSphaseadm1table() %>% ggplot(aes(x=ADMIN1Name, y = perc, fill = HHhS_CH,text= paste(paste(ADMIN1Name), paste(formattable::percent(perc)/100),sep = " " ))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=CH_colors) + facet_grid(. ~ ADMIN1Name, scales = "free_x") +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p15, tooltip = "text")
    # })
    
    phhs1html <- reactive({
      HHSphaseadm1table() |> group_by(Phase) %>%
        e_charts(ADMIN1Name) %>%
        e_bar(Percentage, stack = "total", bind = Phase) %>%
        e_color(c("#820000","#E3002B","#E67536","#E6B068","#ECE1B1")) |> 
        # e_color(unname(CH_colors)) %>%
        e_tooltip(trigger = "axis") %>%
        e_legend(left = "center") %>%
        e_y_axis(max = 100, name = "") %>%
        e_labels(
          position = "inside",
          formatter = htmlwidgets::JS("
      function(params) {
        return (params.value[1] || 0) + '%';
      }
    ")
        ) %>%
        e_tooltip(
          formatter = htmlwidgets::JS("
      function(params) {
        return params.seriesName + '<br>' +
               params.value[0] + ': ' + (params.value[1] || 0) + '%';
      }
    ")
        ) |> 
        e_toolbox_feature("saveAsImage")
    })
    
    #######################  Admin2 ###########################
    
    output$hhsAdmin2 <- renderUI({
      selectInput(
        inputId = "admin2hhs",label = "ADMIN1Name", choices = unique(data()$ADMIN1Name)
      )
    })
    
    # HHSphaseadm2table <- reactive(req(dataset())%>% 
    #                                 group_by(ADMIN1Name, ADMIN2Name) %>%  count(HHhS_CH) %>%
    #                                 mutate(perc = 100 * n / sum(n)) %>% ungroup() %>% 
    #                                 select(-n) %>% mutate_if(is.numeric, round, 1) %>% 
    #                                 left_join(countsadm1adm2table2(), by = "ADMIN2Name")
    # ) %>% bindCache(dataset())
    # 
    # # output$HHSadm2_barplot <- renderPlotly({
    # #   p16 <- HHSphaseadm2table() %>% filter(ADMIN1Name == req(input$admin2hhs)) %>% ggplot(aes(x=ADMIN2Name, y = perc, fill = HHhS_CH)) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=CH_colors)  +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    # #   ggplotly(p16)
    # #   # + facet_grid(. ~ ADMIN1Name, scales = "free_x")
    # # })
    # 
    # output$HHSadm2_barplot <- renderPlotly({
    #   p16 <- HHSphaseadm2table() %>% filter(ADMIN1Name == req(input$admin2hhs)) %>% ggplot(aes(x=ADMIN2Name, y = perc, fill = HHhS_CH, text = paste(paste(ADMIN1Name), paste(ADMIN2Name,formattable::percent(perc)/100, sep = " "), sep="\n"))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=CH_colors)  +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p16,tooltip = "text")
    # })
    
    # Ajouter par moi
    HHSphaseadm2table <- reactive({
      tryCatch({
        # Vérifier si HHhS_CH existe dans le dataset
        data_vars <- names(dataset())
        if (!"HHhS_CH" %in% data_vars) {
          showNotification("Household Hunger Scale variables are missing. Please ensure all HHS variables are present in your dataset.", 
                         type = "warning", duration = 10)
          return(NULL)
        }
        
        df_hhsadm2 <- req(dataset()) |> 
          expss::cross_rpct(ADMIN2Name,list(HHhS_CH),total_row_position = "none") |>   as_data_frame()|> dplyr::rename(ADMIN2Name = 1) |> mutate(
            ADMIN2Name = stringr::str_replace_all(string = ADMIN2Name,pattern = ".*\\|", replacement = "")
          ) |> mutate(
            ADMIN1Name = maditr::vlookup(lookup_value = ADMIN2Name,dict = dataset(),lookup_column = "ADMIN2Name",result_column ="ADMIN1Name")
          ) |> relocate(c(ADMIN1Name), .before = ADMIN2Name) |> mutate(
            across( where(is.numeric), ~round(.,1)  )
          )
        
        df_long_hhsadm2 <- df_hhsadm2 %>%
          pivot_longer(
            cols = starts_with("Phase"),
            names_to = "Phase",
            values_to = "Percentage") |> 
          mutate(Phase = factor(Phase, levels = hhs_order)) |> 
          arrange(Phase)
        
        return(df_long_hhsadm2)
      }, error = function(e) {
        showNotification(paste("Error processing HHS data for Admin2:", e$message), 
                       type = "error", duration = 10)
        return(NULL)
      })
    })
    
    output$HHSadm2_barplot <- renderEcharts4r({
      data <- HHSphaseadm2table()
      if (is.null(data)) return(NULL)
      
      data %>% filter(ADMIN1Name == req(input$admin2hhs)) |> 
        group_by(Phase) %>%
        e_charts(ADMIN2Name) %>%
        e_bar(Percentage, stack = "total", bind = Phase) %>%
        e_color(c("#820000","#E3002B","#E67536","#E6B068","#ECE1B1")) |> 
        # e_color(unname(CH_colors)) %>%
        e_tooltip(trigger = "axis") %>%
        e_legend(left = "center") %>%
        e_y_axis(max = 100, name = "") %>%
        e_labels(
          position = "inside",
          formatter = htmlwidgets::JS("
      function(params) {
        return (params.value[1] || 0) + '%';
      }
    ")
        ) %>%
        e_tooltip(
          formatter = htmlwidgets::JS("
      function(params) {
        return params.seriesName + '<br>' +
               params.value[0] + ': ' + (params.value[1] || 0) + '%';
      }
    ")
        ) |> 
        e_toolbox_feature("saveAsImage")
    })
    
    # export report
    phhs1html <- reactive({
      HHSphaseadm1table() |> group_by(Phase) %>%
        e_charts(ADMIN1Name) %>%
        e_bar(Percentage, stack = "total", bind = Phase) %>%
        e_color(c("#820000","#E3002B","#E67536","#E6B068","#ECE1B1")) |> 
        e_tooltip(trigger = "axis") %>%
        e_legend(left = "center") %>%
        e_y_axis(max = 100, name = "") %>%
        e_labels(
          position = "inside",
          formatter = htmlwidgets::JS("
      function(params) {
        return (params.value[1] || 0) + '%';
      }
    ")
        ) %>%
        e_tooltip(
          formatter = htmlwidgets::JS("
      function(params) {
        return params.seriesName + '<br>' +
               params.value[0] + ': ' + (params.value[1] || 0) + '%';
      }
    ")
        ) |> 
        e_toolbox_feature("saveAsImage")
    })
    
    # phhs2html <- reactive({
    #   p16 <- HHSphaseadm2table() %>% filter(ADMIN1Name == req(input$admin2hhs)) %>% ggplot(aes(x=ADMIN2Name, y = perc, fill = HHhS_CH, text = paste(paste(ADMIN1Name), paste(ADMIN2Name,formattable::percent(perc)/100, sep = " "), sep="\n"))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=CH_colors)  +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p16,tooltip = "text")
    # })
    
    phhs2html <- reactive({
      HHSphaseadm2table() %>% filter(ADMIN1Name == req(input$admin2hhs)) |> 
        group_by(Phase) %>%
        e_charts(ADMIN2Name) %>%
        e_bar(Percentage, stack = "total", bind = Phase) %>%
        e_color(c("#820000","#E3002B","#E67536","#E6B068","#ECE1B1")) |> 
        # e_color(unname(CH_colors)) %>%
        e_tooltip(trigger = "axis") %>%
        e_legend(left = "center") %>%
        e_y_axis(max = 100, name = "") %>%
        e_labels(
          position = "inside",
          formatter = htmlwidgets::JS("
      function(params) {
        return (params.value[1] || 0) + '%';
      }
    ")
        ) %>%
        e_tooltip(
          formatter = htmlwidgets::JS("
      function(params) {
        return params.seriesName + '<br>' +
               params.value[0] + ': ' + (params.value[1] || 0) + '%';
      }
    ")
        ) |> 
        e_toolbox_feature("saveAsImage")
    })
    
    #######################  Enquêteurs ########################### 
    
    output$hhsenqueteur <- renderUI({
      selectInput(
        inputId = "enqueteurhhs",label = "ADMIN1Name", choices = unique(data()$ADMIN1Name)
      )
    })
    
    HHSadm1EnumNametable <- reactive(req(dataset())%>% 
                                       group_by(EnuName,  ADMIN1Name) %>%  count(HHhS_CH) %>% 
                                       mutate(perc = 100 * n / sum(n)) %>% ungroup() %>% 
                                       select(-n) %>% mutate_if(is.numeric, round, 1)%>% 
                                       left_join(countsenumtable2(), by = "EnuName")
    ) %>% bindCache(dataset())
    
    # output$HHSadm1EnumName_barplot <- renderPlotly({
    #   p17 <- HHSadm1EnumNametable() %>% filter(ADMIN1Name == req(input$enqueteurhhs)) %>% ggplot(aes(x=EnuName, y = perc, fill = HHhS_CH)) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=CH_colors)  +labs(x = "", y = "")  +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p17)
    #   # + facet_wrap(ADMIN1Name ~ ., scales = "free")
    # })
    
    output$HHSadm1EnumName_barplot <- renderPlotly({
      p17 <- HHSadm1EnumNametable() %>% filter(ADMIN1Name == req(input$enqueteurhhs)) %>% ggplot(aes(x=EnuName, y = perc, fill = HHhS_CH, text = paste(paste(ADMIN1Name), paste(EnuName,formattable::percent(perc)/100, sep = " "), sep="\n"))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=CH_colors)  +labs(x = "", y = "")  +scale_y_continuous(labels = scales::percent)
      ggplotly(p17,tooltip = "text")
      # + facet_wrap(ADMIN1Name ~ ., scales = "free")
    })
    
    # Ajouter par moi
    HHSadm1EnumNametable <- reactive({
      tryCatch({
        # Vérifier si HHhS_CH existe dans le dataset
        data_vars <- names(dataset())
        if (!"HHhS_CH" %in% data_vars) {
          showNotification("Household Hunger Scale variables are missing. Please ensure all HHS variables are present in your dataset.", 
                         type = "warning", duration = 10)
          return(NULL)
        }
        
        df_hhsenum <-  req(dataset()) |> 
          expss::cross_rpct(EnuName,list(HHhS_CH),total_row_position = "none") |>   as_data_frame()|> dplyr::rename(EnuName = 1) |> mutate(
            EnuName = stringr::str_replace_all(string = EnuName,pattern = ".*\\|", replacement = "")
          ) |> mutate(
            ADMIN1Name = maditr::vlookup(lookup_value = EnuName,dict = dataset(),lookup_column = "EnuName",result_column ="ADMIN1Name")
          ) |> relocate(c(ADMIN1Name), .before = EnuName) |> mutate(
            across( where(is.numeric), ~round(.,1)  )
          )
        
        df_long_hhsenum <- df_hhsenum %>%
          pivot_longer(
            cols = starts_with("Phase"),
            names_to = "Phase",
            values_to = "Percentage") |> 
          mutate(Phase = factor(Phase, levels = hhs_order)) |> 
          arrange(Phase)
        
        return(df_long_hhsenum)
      }, error = function(e) {
        showNotification(paste("Error processing HHS data for Enumerators:", e$message), 
                       type = "error", duration = 10)
        return(NULL)
      })
    })
    
    output$HHSadm1EnumName_barplot <- renderEcharts4r({
      data <- HHSadm1EnumNametable()
      if (is.null(data)) return(NULL)
      
      data %>% filter(ADMIN1Name == req(input$enqueteurhhs)) |> 
        group_by(Phase) %>%
        e_charts(EnuName) %>%
        e_bar(Percentage, stack = "total", bind = Phase) %>%
        e_color(c("#820000","#E3002B","#E67536","#E6B068","#ECE1B1")) |> 
        # e_color(unname(CH_colors)) %>%
        e_tooltip(trigger = "axis") %>%
        e_legend(left = "center") %>%
        e_y_axis(max = 100, name = "") %>%
        e_labels(
          position = "inside",
          formatter = htmlwidgets::JS("
      function(params) {
        return (params.value[1] || 0) + '%';
      }
    ")
        ) %>%
        e_tooltip(
          formatter = htmlwidgets::JS("
      function(params) {
        return params.seriesName + '<br>' +
               params.value[0] + ': ' + (params.value[1] || 0) + '%';
      }
    ")
        ) |> 
        e_toolbox_feature("saveAsImage")
    })
    
    # export report
    # phhs3html <- reactive({
    #   p17 <- HHSadm1EnumNametable() %>% filter(ADMIN1Name == req(input$enqueteurhhs)) %>% ggplot(aes(x=EnuName, y = perc, fill = HHhS_CH, text = paste(paste(ADMIN1Name), paste(EnuName,formattable::percent(perc)/100, sep = " "), sep="\n"))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=CH_colors)  +labs(x = "", y = "")  +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p17,tooltip = "text")
    # })
    
    phhs3html <- reactive({
      HHSadm1EnumNametable() %>% filter(ADMIN1Name == req(input$enqueteurhhs)) |> 
        group_by(Phase) %>%
        e_charts(EnuName) %>%
        e_bar(Percentage, stack = "total", bind = Phase) %>%
        e_color(c("#820000","#E3002B","#E67536","#E6B068","#ECE1B1")) |> 
        # e_color(unname(CH_colors)) %>%
        e_tooltip(trigger = "axis") %>%
        e_legend(left = "center") %>%
        e_y_axis(max = 100, name = "") %>%
        e_labels(
          position = "inside",
          formatter = htmlwidgets::JS("
      function(params) {
        return (params.value[1] || 0) + '%';
      }
    ")
        ) %>%
        e_tooltip(
          formatter = htmlwidgets::JS("
      function(params) {
        return params.seriesName + '<br>' +
               params.value[0] + ': ' + (params.value[1] || 0) + '%';
      }
    ")
        ) |> 
        e_toolbox_feature("saveAsImage")
    })
    
    # Stratégie d'apdaptation lhcs --------------------------------------------
    
    #######################  Admin1 ###########################
    LHCS_colors <-  c("EmergencyStrategies" = "#E3002B","CrisisStrategies" = "#E67536","StressStrategies" = "#E6B068","NoStrategies" = "#ECE1B1")
    strategy_order <- c("EmergencyStrategies","CrisisStrategies","StressStrategies","NoStrategies")
    
    LHCSadm1table <- reactive(req(dataset())%>%
                                group_by(ADMIN1Name) %>%  count(LhCSICat) %>% 
                                mutate(perc = 100 * n / sum(n)) %>% ungroup() %>% 
                                select(-n) %>% mutate_if(is.numeric, round, 1)%>%
                                left_join(countsadm1table(), by = "ADMIN1Name")
    ) %>% bindCache(dataset())
    
    # output$LHCSadm1_barplot <- renderPlotly({
    #   p17 <- LHCSadm1table() %>% ggplot(aes(x=ADMIN1Name, y = perc, fill = LhCSICat,text= paste("Total questionnaire :",n))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=LHCS_colors) + facet_wrap(. ~ ADMIN1Name, scales = "free_x") +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p17)
    # })
    
    output$LHCSadm1_barplot <- renderPlotly({
      p17 <- LHCSadm1table() %>% ggplot(aes(x=ADMIN1Name, y = perc, fill = LhCSICat,text= paste(paste(ADMIN1Name), paste(formattable::percent(perc)/100),sep = " " ))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=LHCS_colors) + facet_wrap(. ~ ADMIN1Name, scales = "free_x") +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
      ggplotly(p17, tooltip = "text")
    })
    
    # Ajouter par moi
    LHCSadm1table <- reactive({
      tryCatch({
        # Vérifier si LhCSICat existe dans le dataset
        data_vars <- names(dataset())
        if (!"LhCSICat" %in% data_vars) {
          showNotification("Livelihood Coping Strategy variables are missing. Please ensure all LCS variables (LCSIStress1-4, LCSICrisis1-3, LCSIEmergency1-4) are present in your dataset.", 
                         type = "warning", duration = 10)
          return(NULL)
        }
        
        df_lcs_adm1 <- req(dataset()) |> 
          expss::cross_rpct(ADMIN1Name,list(LhCSICat),total_row_position = "none") |> as_tibble() |> dplyr::rename(ADMIN1Name = 1) |> mutate(
            ADMIN1Name = stringr::str_replace_all(string = ADMIN1Name,pattern = ".*\\|", replacement = "")
          ) |> mutate(
            across( where(is.numeric), ~round(.,1)))
        
        df_long_lcs_adm1 <- df_lcs_adm1 %>%
          pivot_longer(
            cols = -ADMIN1Name,
            names_to = "Strategy",
            values_to = "Percentage") |> 
          mutate(Strategy = factor(Strategy, levels = strategy_order)) %>%
          arrange(Strategy)
        
        return(df_long_lcs_adm1)
      }, error = function(e) {
        showNotification(paste("Error processing Livelihood Coping Strategy data:", e$message), 
                       type = "error", duration = 10)
        return(NULL)
      })
    })
    
    output$LHCSadm1_barplot <- renderEcharts4r({
      data <- LHCSadm1table()
      if (is.null(data)) return(NULL)
      
      data |> group_by(Strategy) %>%
        e_charts(ADMIN1Name) %>%
        e_bar(Percentage, stack = "total", bind = Strategy) %>%
        e_color(unname(LHCS_colors)) %>%
        e_legend(left = "center") %>%
        e_y_axis(max = 100, name = "") %>%
        e_labels(
          position = "inside",
          formatter = htmlwidgets::JS("
      function(params) {
        return (params.value[1] || 0) + '%';
      }
    ")
        ) %>%
        e_tooltip(
          formatter = htmlwidgets::JS("
      function(params) {
        return params.seriesName + '<br>' +
               params.value[0] + ': ' + (params.value[1] || 0) + '%';
      }
    ")
        ) |> 
        e_toolbox_feature("saveAsImage")
    })
    
    # export report
    # plcs1html <- reactive({
    #   p17 <- LHCSadm1table() %>% ggplot(aes(x=ADMIN1Name, y = perc, fill = LhCSICat,text= paste(paste(ADMIN1Name), paste(formattable::percent(perc)/100),sep = " " ))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=LHCS_colors) + facet_wrap(. ~ ADMIN1Name, scales = "free_x") +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p17, tooltip = "text")
    # })
    
    plcs1html <- reactive({
      LHCSadm1table() |> group_by(Strategy) %>%
        e_charts(ADMIN1Name) %>%
        e_bar(Percentage, stack = "total", bind = Strategy) %>%
        e_color(unname(LHCS_colors)) %>%
        e_legend(left = "center") %>%
        e_y_axis(max = 100, name = "") %>%
        e_labels(
          position = "inside",
          formatter = htmlwidgets::JS("
      function(params) {
        return (params.value[1] || 0) + '%';
      }
    ")
        ) %>%
        e_tooltip(
          formatter = htmlwidgets::JS("
      function(params) {
        return params.seriesName + '<br>' +
               params.value[0] + ': ' + (params.value[1] || 0) + '%';
      }
    ")
        ) |> 
        e_toolbox_feature("saveAsImage")
    })
    
    #######################  Admin2 ###########################
    
    output$lhcsAdmin2 <- renderUI({
      selectInput(
        inputId = "admin2lhcs",label = "ADMIN1Name", choices = unique(data()$ADMIN1Name)
      )
    })
    
    LHCSadm2table <- reactive(req(dataset())%>% 
                                group_by(ADMIN1Name, ADMIN2Name) %>%  count(LhCSICat) %>% 
                                mutate(perc = 100 * n / sum(n)) %>% ungroup() %>% 
                                select(-n) %>% mutate_if(is.numeric, round, 1) %>% 
                                left_join(countsadm1adm2table2(), by = "ADMIN2Name")
    ) %>% bindCache(dataset())
    
    # output$LHCSadm2_barplot <- renderPlotly({
    #   p18 <- LHCSadm2table() %>% filter(ADMIN1Name == req(input$admin2lhcs))  %>% ggplot(aes(x=ADMIN2Name, y = perc, fill = LhCSICat)) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=LHCS_colors)  +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p18)
    #   # + facet_wrap(. ~ ADMIN1Name, scales = "free_x")
    # })
    
    output$LHCSadm2_barplot <- renderPlotly({
      p18 <- LHCSadm2table() %>% filter(ADMIN1Name == req(input$admin2lhcs))  %>% ggplot(aes(x=ADMIN2Name, y = perc, fill = LhCSICat, text = paste(paste(ADMIN1Name), paste(ADMIN2Name,formattable::percent(perc)/100, sep = " "), sep="\n"))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=LHCS_colors)  +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
      ggplotly(p18,tooltip = "text")
      # + facet_wrap(. ~ ADMIN1Name, scales = "free_x")
    })
    
    # Ajouter par moi 
    LHCSadm2table <- reactive({
      tryCatch({
        # Vérifier si LhCSICat existe dans le dataset
        data_vars <- names(dataset())
        if (!"LhCSICat" %in% data_vars) {
          showNotification("Livelihood Coping Strategy variables are missing. Please ensure all LCS variables are present in your dataset.", 
                         type = "warning", duration = 10)
          return(NULL)
        }
        
        df_lcsadm2 <- req(dataset()) |> 
          expss::cross_rpct(ADMIN2Name,list(LhCSICat),total_row_position = "none") |>   as_data_frame()|> dplyr::rename(ADMIN2Name = 1) |> mutate(
            ADMIN2Name = stringr::str_replace_all(string = ADMIN2Name,pattern = ".*\\|", replacement = "")
          ) |> mutate(
            ADMIN1Name = maditr::vlookup(lookup_value = ADMIN2Name,dict = dataset(),lookup_column = "ADMIN2Name",result_column ="ADMIN1Name")
          ) |> relocate(c(ADMIN1Name), .before = ADMIN2Name) |> mutate(
            across( where(is.numeric), ~round(.,1)  )
          )
        
        df_long_lcsadm2 <- df_lcsadm2 %>%
          pivot_longer(
            cols = starts_with("NoStrategies"):starts_with("EmergencyStrategies"),
            names_to = "Strategy",
            values_to = "Percentage") |> 
          mutate(Strategy = factor(Strategy, levels = strategy_order)) %>%
          arrange(Strategy)
        
        return(df_long_lcsadm2)
      }, error = function(e) {
        showNotification(paste("Error processing LCS data for Admin2:", e$message), 
                       type = "error", duration = 10)
        return(NULL)
      })
    })
    
    output$LHCSadm2_barplot <- renderEcharts4r({
      data <- LHCSadm2table()
      if (is.null(data)) return(NULL)
      
      data %>% filter(ADMIN1Name == req(input$admin2lhcs)) |> 
        group_by(Strategy) %>%
        e_charts(ADMIN2Name) %>%
        e_bar(Percentage, stack = "total", bind = Strategy) %>%
        e_color(unname(LHCS_colors)) %>%
        e_tooltip(trigger = "axis") %>%
        e_legend(left = "center") %>%
        e_y_axis(max = 100, name = "") %>%
        e_labels(
          position = "inside",
          formatter = htmlwidgets::JS("
      function(params) {
        return (params.value[1] || 0) + '%';
      }
    ")
        ) %>%
        e_tooltip(
          formatter = htmlwidgets::JS("
      function(params) {
        return params.seriesName + '<br>' +
               params.value[0] + ': ' + (params.value[1] || 0) + '%';
      }
    ")
        ) |> 
        e_toolbox_feature("saveAsImage")
    })
    
    # export report
    # plcs2html <- reactive({
    #   p18 <- LHCSadm2table() %>% filter(ADMIN1Name == req(input$admin2lhcs))  %>% ggplot(aes(x=ADMIN2Name, y = perc, fill = LhCSICat, text = paste(paste(ADMIN1Name), paste(ADMIN2Name,formattable::percent(perc)/100, sep = " "), sep="\n"))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=LHCS_colors)  +labs(x = "", y = "") +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p18,tooltip = "text")
    # })
    
    plcs2html <- reactive({
      LHCSadm2table() %>% filter(ADMIN1Name == req(input$admin2lhcs)) |> 
        group_by(Strategy) %>%
        e_charts(ADMIN2Name) %>%
        e_bar(Percentage, stack = "total", bind = Strategy) %>%
        e_color(unname(LHCS_colors)) %>%
        e_tooltip(trigger = "axis") %>%
        e_legend(left = "center") %>%
        e_y_axis(max = 100, name = "") %>%
        e_labels(
          position = "inside",
          formatter = htmlwidgets::JS("
      function(params) {
        return (params.value[1] || 0) + '%';
      }
    ")
        ) %>%
        e_tooltip(
          formatter = htmlwidgets::JS("
      function(params) {
        return params.seriesName + '<br>' +
               params.value[0] + ': ' + (params.value[1] || 0) + '%';
      }
    ")
        ) |> 
        e_toolbox_feature("saveAsImage")
    })
    
    #######################  Enquêteurs ########################### 
    
    output$lhcsenqueteur <- renderUI({
      selectInput(
        inputId = "enqueteurlhcs",label = "ADMIN1Name", choices = unique(data()$ADMIN1Name)
      )
    })
    
    # LHCSadm1EnumNametable <- reactive(req(dataset())%>% 
    #                                     group_by(EnuName,  ADMIN1Name) %>%  count(LhCSICat) %>% 
    #                                     mutate(perc = 100 * n / sum(n)) %>% ungroup() %>% select(-n) %>% mutate_if(is.numeric, round, 1)%>% 
    #                                     left_join(countsenumtable2(), by = "EnuName")
    # ) %>% bindCache(dataset())
    # 
    # # output$LHCSadm1EnumName_barplot <- renderPlotly({
    # #   p19 <- LHCSadm1EnumNametable() %>% filter(ADMIN1Name == req(input$enqueteurlhcs)) %>% ggplot(aes(x=EnuName, y = perc, fill = LhCSICat)) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=LHCS_colors)  +labs(x = "", y = "") +theme(axis.text.x=element_blank()) +scale_y_continuous(labels = scales::percent)
    # #   ggplotly(p19)
    # #   # + facet_wrap(ADMIN1Name ~ ., scales = "free")
    # # })
    # 
    # output$LHCSadm1EnumName_barplot <- renderPlotly({
    #   p19 <- LHCSadm1EnumNametable() %>% filter(ADMIN1Name == req(input$enqueteurlhcs)) %>% ggplot(aes(x=EnuName, y = perc, fill = LhCSICat, text = paste(paste(ADMIN1Name), paste(EnuName,formattable::percent(perc)/100, sep = " "), sep="\n"))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=LHCS_colors)  +labs(x = "", y = "")  +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p19,tooltip = "text")
    #   # + facet_wrap(ADMIN1Name ~ ., scales = "free")
    # })
    
    # Ajouter par moi
    LHCSadm1EnumNametable <- reactive({
      tryCatch({
        # Vérifier si LhCSICat existe dans le dataset
        data_vars <- names(dataset())
        if (!"LhCSICat" %in% data_vars) {
          showNotification("Livelihood Coping Strategy variables are missing. Please ensure all LCS variables are present in your dataset.", 
                         type = "warning", duration = 10)
          return(NULL)
        }
        
        df_lcsenum <- req(dataset()) |> 
          expss::cross_rpct(EnuName,list(LhCSICat),total_row_position = "none") |>   as_data_frame()|> dplyr::rename(EnuName = 1) |> mutate(
            EnuName = stringr::str_replace_all(string = EnuName,pattern = ".*\\|", replacement = "")
          ) |> mutate(
            ADMIN1Name = maditr::vlookup(lookup_value = EnuName,dict = dataset(),lookup_column = "EnuName",result_column ="ADMIN1Name")
          ) |> relocate(c(ADMIN1Name), .before = EnuName) |> mutate(
            across( where(is.numeric), ~round(.,1)  )
          )
        
        df_long_lcsenum <- df_lcsenum %>%
          pivot_longer(
            cols = starts_with("NoStrategies"):starts_with("EmergencyStrategies"),
            names_to = "Strategy",
            values_to = "Percentage") |> 
          mutate(Strategy = factor(Strategy, levels = strategy_order)) %>%
          arrange(Strategy)
        
        return(df_long_lcsenum)
      }, error = function(e) {
        showNotification(paste("Error processing LCS data for Enumerators:", e$message), 
                       type = "error", duration = 10)
        return(NULL)
      })
    })
    
    output$LHCSadm1EnumName_barplot <- renderEcharts4r({
      
      data <- LHCSadm1EnumNametable()
      if (is.null(data)) return(NULL)
      
      data %>% filter(ADMIN1Name == req(input$enqueteurlhcs)) |> 
        group_by(Strategy) %>%
        e_charts(EnuName) %>%
        e_bar(Percentage, stack = "total", bind = Strategy) %>%
        e_color(unname(LHCS_colors)) %>%
        e_tooltip(trigger = "axis") %>%
        e_legend(left = "center") %>%
        e_y_axis(max = 100, name = "") %>%
        e_labels(
          position = "inside",
          formatter = htmlwidgets::JS("
      function(params) {
        return (params.value[1] || 0) + '%';
      }
    ")
        ) %>%
        e_tooltip(
          formatter = htmlwidgets::JS("
      function(params) {
        return params.seriesName + '<br>' +
               params.value[0] + ': ' + (params.value[1] || 0) + '%';
      }
    ")
        ) |> 
        e_toolbox_feature("saveAsImage")
    })
    
    # export report
    # plcs3html <- reactive({
    #   p19 <- LHCSadm1EnumNametable() %>% filter(ADMIN1Name == req(input$enqueteurlhcs)) %>% ggplot(aes(x=EnuName, y = perc, fill = LhCSICat, text = paste(paste(ADMIN1Name), paste(EnuName,formattable::percent(perc)/100, sep = " "), sep="\n"))) +geom_bar(position="fill", stat = "identity") +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=LHCS_colors)  +labs(x = "", y = "")  +scale_y_continuous(labels = scales::percent)
    #   ggplotly(p19,tooltip = "text")
    # })
    
    plcs3html <- reactive({
      LHCSadm1EnumNametable() %>% filter(ADMIN1Name == req(input$enqueteurlhcs)) |> 
        group_by(Strategy) %>%
        e_charts(EnuName) %>%
        e_bar(Percentage, stack = "total", bind = Strategy) %>%
        e_color(unname(LHCS_colors)) %>%
        e_tooltip(trigger = "axis") %>%
        e_legend(left = "center") %>%
        e_y_axis(max = 100, name = "") %>%
        e_labels(
          position = "inside",
          formatter = htmlwidgets::JS("
      function(params) {
        return (params.value[1] || 0) + '%';
      }
    ")
        ) %>%
        e_tooltip(
          formatter = htmlwidgets::JS("
      function(params) {
        return params.seriesName + '<br>' +
               params.value[0] + ': ' + (params.value[1] || 0) + '%';
      }
    ")
        ) |> 
        e_toolbox_feature("saveAsImage")
    })
    
    
    
    
    # Partie export des graphiques --------------------------------------------
    # exportation global du rapport
    
    output$report <- downloadHandler(
      filename = "report.html",
      content = function(file) {
        tempReport <- file.path(tempdir(), "report_html.Rmd")
        file.copy("report_html.Rmd", tempReport, overwrite = TRUE)
        # Créer des versions autonomes pour le rapport
        safe_plot_func <- function(func_call, func_name) {
          tryCatch({
            result <- func_call
            if(is.null(result)) {
              cat(paste("Warning:", func_name, "returned NULL - variables may be missing\n"))
            }
            return(result)
          }, error = function(e) {
            cat(paste("Error in", func_name, ":", e$message, "\n"))
            return(NULL)
          })
        }
        
        params <- list(
          adv1 = safe_plot_func(pavaadmihtml(), "pavaadmihtml"),
          adv2 = safe_plot_func(pavaadmi2html(), "pavaadmi2html"),
          adv3 = safe_plot_func(pavaadmienqhtml(), "pavaadmienqhtml"), 
          adv4 = safe_plot_func(pavaadmienqhtmladm2(), "pavaadmienqhtmladm2"),
          sca1 = safe_plot_func(p4html(), "p4html"),
          sca2 = safe_plot_func(p5html(), "p5html"), 
          sca3 = safe_plot_func(p6html(), "p6html"), 
          fcsdayplot = safe_plot_func(pfcsdayhtml(), "pfcsdayhtml"),
          hds1 = safe_plot_func(phdds1html(), "phdds1html"),
          hds2 = safe_plot_func(phdds2html(), "phdds2html"), 
          hds3 = safe_plot_func(phdds3html(), "phdds3html"),
          rcs1 = safe_plot_func(prcsihtml(), "prcsihtml"), 
          rcs2 = safe_plot_func(prcsi2html(), "prcsi2html"), 
          rcs3 = safe_plot_func(prcsi3html(), "prcsi3html"), 
          rcsidayplot = safe_plot_func(prcsidayhtml(), "prcsidayhtml"),
          hhs1 = safe_plot_func(phhs1html(), "phhs1html"),
          hhs2 = safe_plot_func(phhs2html(), "phhs2html"), 
          hhs3 = safe_plot_func(phhs3html(), "phhs3html"),
          lcs1 = safe_plot_func(plcs1html(), "plcs1html"),
          lcs2 = safe_plot_func(plcs2html(), "plcs2html"),
          lcs3 = safe_plot_func(plcs3html(), "plcs3html")
        )
        
        id <- showNotification(
          "Generating comprehensive report with all available data...\nThis may take a few minutes. All visualizations will be included regardless of which tabs you visited.", 
          duration = NULL, 
          closeButton = FALSE,
          type = "message"
        )
        on.exit(removeNotification(id), add = TRUE)
        
        rmarkdown::render(tempReport,
                          output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
    # Fin de la syntaxe -------------------------------------------------------
    
    }
  
)