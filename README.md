# LAFLA_Shiny_App

Hello! 

This application is the product of a pro-bono engagement by economic consultants at the Analysis Group (AG) 
to support the work done by the Legal Aid Foundation of Los Angeles (LAFLA). It includes a database and data 
visualizations for demographics, housing, local government, employment, and various other metrics in Los 
Angeles County. This application was built for the purpose of giving policymakers and non-profit leaders the 
data tools to address the rent burden and housing insecurity crises in Los Angeles. 

To run the application, perform the following:

  If this is your first time running the app from your R console, please enter the following code:
    
    list.of.packages <- c("data.table", "dplyr", "DT", "ggplot2", "ggpubr", "highcharter",
                        "janitor", "lubridate", "plotly", "readxl", "rsconnect", "shiny",
                        "shinydashboard", "shinyjs", "shinythemes", "shinyWidgets", "sqldf",
                        "stringr", "zoo", "tidyr", "devtools")
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    library(devtools)
    install_github('arilamstein/choroplethrZip@v1.5.0')
    
  Code to start the app:
    
    library(shiny)
    shiny::runGitHub("LAFLA_Shiny_App", "jdtrinh")
