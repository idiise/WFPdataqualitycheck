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
library(leaflet)
library(readxl)
library(readr)

# library(cowplot)

shinyUI(
  
  argonDashPage(
    title = "Data Quality Check", 
    description = "Data Quality Check", 
    author = "Idrissa Dabo",
    navbar = argonDashNavbar(), 
    sidebar = argonDashSidebar(
      id = "sidebar", 
      side = "left",
      size = "lg", 
      skin = "dark",
      background = "white",
      brand_url = "https://www.wfp.org/",
      brand_logo = "logowfp.jpg",
      argonSidebarHeader("Main page"),
      argonSidebarMenu(
        argonSidebarItem(
          tabName = "accueil",
          icon = argonIcon(name = "paper-diploma", color = "success"),
          "Main"
        ),
        argonSidebarItem(
          tabName = "data",
          icon = argonIcon(name = "app", color = "success"),
          "Data"
        ),
        argonSidebarItem(
          tabName = "avancement",
          icon = argonIcon(name = "atom", color = "green"),
          "SUBMISSION"
        ),
        argonSidebarItem(
          tabName = "sca",
          icon = argonIcon(name = "atom", color = "green"),
          "FCS"
        ),
        argonSidebarItem(
          tabName = "hdds",
          icon = argonIcon(name = "atom", color = "green"),
          "HDDS"
        ),
        argonSidebarItem(
          tabName = "rcsi",
          icon = argonIcon(name = "atom", color = "green"),
          "rCSI"
        ),
        argonSidebarItem(
          tabName = "hhs",
          icon = argonIcon(name = "atom", color = "green"),
          "HHS"
        ),
        argonSidebarItem(
          tabName = "lhcs",
          icon = argonIcon(name = "atom", color = "green"),
          "LhCS"
        ),
        argonSidebarItem(
          tabName = "rapport",
          icon = argonIcon(name = "atom", color = "green"),
          "Generate Report"
        )
      )
    ), 
    header = argonDashHeader("Data Quality Check",align = "center"), 
    
    body = argonDashBody(
      argonTabItems(
        argonTabItem(
          tabName = "accueil",
          argonH1("Please read the indication below", display = 4),
          argonRow(
            #     argonCard(
            #     status = "primary",
            #     width = 12,
            #     title = "Indication",
            #     hover_lift = TRUE,
            #     shadow = TRUE,
            #     # src = "https://wfp-vam.github.io/RBD_FS_CH_guide_FR/combined-questionnaire-syntaxes-for-all-5-indicators.html",
            #     background_color = "default",
            #     icon = argonIcon("check-bold"),
            #      
            #        
            #          textInput("indication", "Argon is a great free UI package based on Bootstrap 4 
            # that includes the most important components and features.")
            #        
            #      
            #     
            #     
            #   )
            argonInfoCard(width = 12,
                          value = HTML(paste("1) Upload your database in SPSS (.sav), Excel (.xlsx, .xls), CSV (.csv), Stata (.dta), or text (.txt) format.",
                                             "2) The maximum size of the database to be upload into the application is set to 200MB.",
                                             "3) Check if your database contains the variable names shown in the table below and use the standard names in the Required Variable Names section.",
                                             "4) The variable ADMIN4Name is the most disaggregated area where the household is located. In some countries it can be considered as Admin5 but to use this application please rename by ADMIN4Name in your database.",
                                             "5) If there is a missing variable  such as the supervisor's name, please create an empty column and name it EnuSupervisorName.",
                                             "6) To get the results of key indicators."," Please use the standardized variable names available on surveydesigner ",a(href="https://www.surveydesigner.vam.wfp.org/",target="_blank", "Click here"),".", 
                                             "7) In the Data part you will only see the first 6 rows of your database after uploading. This is just to show that your database is uploaded.",
                                             "8) In the FCS part before exporting the tables please select All to have all the Table content.",
                                             "9) For livelihood coping stress strategies please use the name from surveydesigner as listed in the table below.",
                                             "."))
                          ,
                          title = "Indication",
                          icon = argonIcon("bulb-61"),
                          icon_background = "danger",
                          shadow = TRUE,
                          background_color = "default",
                          hover_lift = TRUE
            ),
            
            br(),
            argonTabItem(
              tabName = "indication",
              argonCard(
                status = "primary",
                width = 12,shadow = TRUE,
                dataTableOutput("varStandard")
              )
              
            )
            
            
          )
          
        ),
        argonTabItem(
          tabName = "data",
          argonCard(
            status = "primary",
            # width = 4,
            title = "Upload Dataset",
            radioButtons("file_format", "Choose File Format:",
                        choices = list(
                          "SPSS (.sav)" = "sav",
                          "Excel (.xlsx/.xls)" = "excel", 
                          "CSV (.csv)" = "csv",
                          "Stata (.dta)" = "dta",
                          "Text (.txt)" = "txt"
                        ),
                        selected = "excel"),
            fileInput("base", "Upload Data", 
                     accept = c(".sav", ".csv", ".xlsx", ".xls", ".dta", ".txt")),
            conditionalPanel(
              condition = "input.file_format == 'csv' || input.file_format == 'txt'",
              checkboxInput("header", "Header", TRUE),
              radioButtons("sep", "Separator",
                          choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                          selected = ","),
              radioButtons("quote", "Quote",
                          choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                          selected = '"')
            ),
            br(),
            verbatimTextOutput("file_status")
          ),
          br(),
          argonCard(
            status = "primary",
            width = 12,shadow = TRUE,
            
            
            dataTableOutput("apercu", height = "300px")
          )
          
        ),
        
        # avancemennt -------------------------------------------------------------
        
        
        argonTabItem(
          tabName = "avancement",
          argonH1("Submission", display = 4),
          argonRow(
            argonColumn(width = 12,
                        argonTabSet(
                          id = "tab_avancement",
                          card_wrapper = TRUE,
                          horizontal = TRUE,
                          circle = FALSE,
                          size = "sm",
                          width = 12,
                          iconList = lapply(X = 1:5, FUN = argonIcon, name ="sound-wave"),
                          # iconList = list("submission", "admin1", "admin2", "enqueteurs"),
                          argonTab(
                            tabName = "Submission",
                            active = T,
                            echarts4rOutput("submission", height = "600px",width= "100%")
                          ),
                          argonTab(
                            tabName = "Result by Admin1",
                            active = F,
                            plotlyOutput("adm1_graph", height = "600px") 
                          ),
                          argonTab(
                            tabName = "Result by Admin2",
                            active = F,
                            uiOutput("avancementAdmin1"),
                            plotlyOutput("adm2_graph",height = "600px")
                          ),
                          argonTab(
                            tabName = "Result by Enumerators",
                            active = F,
                            uiOutput("avancementenqueteur"),
                            plotlyOutput("adm1enum_graph", height = "600px")
                            # div(style = "margin: 40px 0; border-top: 2px solid #5e72e4;"),
                            # leafletOutput("map_enum")
                          ),
                          argonTab(
                            tabName = "Result by Enumerators by Admn2",
                            active = F,
                            uiOutput("avancementenqueteuradm2"),
                            uiOutput("enqueteuradm2"),
                            plotlyOutput("adm2enum_graph1", height = "600px")
                            # div(style = "margin: 40px 0; border-top: 2px solid #5e72e4;"),
                            # leafletOutput("map_enum_adm2")
                          )
                        )
            )
          )
        ),
        
        # SCA ---------------------------------------------------------------------
        
        
        argonTabItem(
          tabName = "sca",
          argonH1("Food Consumption Score", display = 4),
          argonRow(
            argonColumn(width = 12,
                        argonTabSet(
                          id = "tab_sca",
                          card_wrapper = TRUE,
                          horizontal = TRUE,
                          circle = FALSE,
                          size = "sm",
                          width = 12,
                          iconList = lapply(X = 1:8, FUN = argonIcon, name ="sound-wave"),
                          # iconList = list("submission", "admin1", "admin2", "enqueteurs"),
                          
                          argonTab(
                            tabName = "Result by Admin1",
                            active = T,
                            uiOutput("fcsseuiladmn1"),
                            echarts4rOutput("FCGadm1_barplot", height = "600px") 
                          ),
                          argonTab(
                            tabName = "Result by Admin2",
                            active = F,
                            uiOutput("fcsadmin2name"),uiOutput("fcsseuil"),
                            echarts4rOutput("FCGadm2_barplot",height = "700px")
                          ),
                          argonTab(
                            tabName = "Result by Enumerators",
                            active = F,
                            uiOutput("enqueteurfcs"),uiOutput("fcsseuilenq"),
                            echarts4rOutput("FCGadm1EnumName_barplot", height = "700px", width = "100%"),
                            div(style = "margin: 40px 0; border-top: 2px solid #5e72e4;"),
                            plotlyOutput("fcs_day")
                          ),
                          argonTab(
                            tabName = "Boxplot Enumerator",
                            active = F,
                            echarts4rOutput("FCSEnumNamebox", height = "600px",width= "100%")
                          ),
                          argonTab(
                            tabName = "Table Outlier FCS",
                            active = F,
                            dataTableOutput("FCSOutliertable", height = "600px")
                          ),
                          argonTab(
                            tabName = "Staple Food consumption",
                            active = F,
                            dataTableOutput("cereale", height = "600px")
                          ),
                          argonTab(
                            tabName = "Cereal plus vegetables or milk",
                            active = F,
                            dataTableOutput("cerealelegumelait")
                          ),
                          argonTab(
                            tabName = "Consumption of vegetables and leaves",
                            active = F,
                            dataTableOutput("legumefeuille")
                          )
                          
                        )
            )
          )
        ),
        
        # HDDS --------------------------------------------------------------------
        
        
        argonTabItem(
          tabName = "hdds",
          argonH1("Household Dietary Diversity Score (HDDS)", display = 4),
          argonRow(
            argonColumn(width = 12,
                        argonTabSet(
                          id = "tab_hdds",
                          card_wrapper = TRUE,
                          horizontal = TRUE,
                          circle = FALSE,
                          size = "sm",
                          width = 12,
                          iconList = lapply(X = 1:3, FUN = argonIcon, name ="sound-wave"),
                          # iconList = list("submission", "admin1", "admin2", "enqueteurs"),
                          
                          argonTab(
                            tabName = "Result by Admin1",
                            active = T,
                            echarts4rOutput("HDDSadm1_barplot", height = "600px") 
                          ),
                          argonTab(
                            tabName = "Result by Admin2",
                            active = F,
                            uiOutput("hddsAdmin2"),
                            echarts4rOutput("HDDSadm2_barplot",height = "700px")
                          ),
                          argonTab(
                            tabName = "Result by Enumerators",
                            active = F,
                            uiOutput("hddsenqueteur"),
                            echarts4rOutput("HDDSadm1EnumName_barplot", height = "700px", width = "100%")
                          )
                          
                        )
            )
          )
        ),
        
        # rCSI --------------------------------------------------------------------
        
        
        argonTabItem(
          tabName = "rcsi",
          argonH1("Reduced Copying Strategy Index (rCSI)", display = 4),
          argonRow(
            argonColumn(width = 12,
                        argonTabSet(
                          id = "tab_rcsi",
                          card_wrapper = TRUE,
                          horizontal = TRUE,
                          circle = FALSE,
                          size = "sm",
                          width = 12,
                          iconList = lapply(X = 1:5, FUN = argonIcon, name ="sound-wave"),
                          # iconList = list("submission", "admin1", "admin2", "enqueteurs"),
                          
                          argonTab(
                            tabName = "Result by Admin1",
                            active = T,
                            echarts4rOutput("rCSIadm1_barplot", height = "600px") 
                          ),
                          argonTab(
                            tabName = "Result by Admin2",
                            active = F,
                            uiOutput("rcsiAdmin2"),
                            echarts4rOutput("rCSIadm2_barplot",height = "700px")
                          ),
                          argonTab(
                            tabName = "Result by Enumerators",
                            active = F,
                            uiOutput("rcsienqueteur"),
                            echarts4rOutput("rCSIadm1EnumName_barplot", height = "700px", width = "100%"),
                            div(style = "margin: 40px 0; border-top: 2px solid #5e72e4;"),
                            plotlyOutput("rcsi_day")
                          ),
                          argonTab(
                            tabName = "Boxplot Enumerator",
                            active = F,
                            echarts4rOutput("rcsiEnumNamebox", height = "600px",width= "100%")
                          ),
                          argonTab(
                            tabName = "Table Outlier rCSI",
                            active = F,
                            dataTableOutput("rcsiOutliertable", height = "600px")
                          )
                          
                        )
            )
          )
        ),
        

# HHS ---------------------------------------------------------------------

        
        argonTabItem(
          tabName = "hhs",
          argonH1("Household Hunger Scale HHS", display = 4),
          argonRow(
            argonColumn(width = 12,
                        argonTabSet(
                          id = "tab_hhs",
                          card_wrapper = TRUE,
                          horizontal = TRUE,
                          circle = FALSE,
                          size = "sm",
                          width = 12,
                          iconList = lapply(X = 1:3, FUN = argonIcon, name ="sound-wave"),
                          # iconList = list("submission", "admin1", "admin2", "enqueteurs"),
                          
                          argonTab(
                            tabName = "Result by Admin1",
                            active = T,
                            echarts4rOutput("HHSadm1_barplot", height = "600px") 
                          ),
                          argonTab(
                            tabName = "Result by Admin2",
                            active = F,
                            uiOutput("hhsAdmin2"),
                            echarts4rOutput("HHSadm2_barplot",height = "700px")
                          ),
                          argonTab(
                            tabName = "Result by Enumerators",
                            active = F,
                            uiOutput("hhsenqueteur"),
                            echarts4rOutput("HHSadm1EnumName_barplot", height = "700px", width = "100%")
                          )
                          
                        )
            )
          )
        ),
        
        # LCS ---------------------------------------------------------------------
        
        
        argonTabItem(
          tabName = "lhcs",
          argonH1("Livelihood Coping Strategy Index", display = 4),
          argonRow(
            argonColumn(width = 12,
                        argonTabSet(
                          id = "tab_lcs",
                          card_wrapper = TRUE,
                          horizontal = TRUE,
                          circle = FALSE,
                          size = "sm",
                          width = 12,
                          iconList = lapply(X = 1:3, FUN = argonIcon, name ="sound-wave"),
                          # iconList = list("submission", "admin1", "admin2", "enqueteurs"),
                          
                          argonTab(
                            tabName = "Result by Admin1",
                            active = T,
                            echarts4rOutput("LHCSadm1_barplot", height = "600px") 
                          ),
                          argonTab(
                            tabName = "Result by Admin2",
                            active = F,
                            uiOutput("lhcsAdmin2"),
                            echarts4rOutput("LHCSadm2_barplot",height = "700px")
                          ),
                          argonTab(
                            tabName = "Result by Enumerators",
                            active = F,
                            uiOutput("lhcsenqueteur"),
                            echarts4rOutput("LHCSadm1EnumName_barplot", height = "700px", width = "100%")
                          )
                          
                        )
            )
          )
        ),
        

# rapport -----------------------------------------------------------------
      argonTabItem(
        tabName = "rapport",
        # tags$script(
        #   "alert('Visualiser tous les graphiques Admin1, Admin2 et enquêteur de tous les indicateurs avant de cliquer sur générer Rapport HTML')"
        # ),
        
        argonInfoCard(width = 12,
                      value = HTML("view all Admin1, Admin2 and Enumerators charts for all indicators before clicking Generate HTML Report below"),
                      title = "Indication",
                      icon = argonIcon("bulb-61"),
                      icon_background = "danger",
                      shadow = TRUE,
                      background_color = "default",
                      hover_lift = TRUE),
        br(),
        br(),
        argonInfoCard(
          width = 5,
          # title = "Download",
          # stat = 3.48,
          # stat_icon = "arrow-up",
          # description = "Since last month",
          icon = "Download",
          value = downloadButton("report", "Generate HTML Repport"),
          icon_background = "danger",
          hover_lift = FALSE
        ),
        
        # downloadButton("report", "Generer Rapport HTML"),
        # downloadButton("reportdocs", "Generer Rapport Word")
      )
        
      )
    ),
    footer = argonDashFooter(copyrights = "@Idrissa DABO",
                             src = "https://github.com/idiise")
  )  
  
  # End --------------------------------------------------------------------
  
  
)