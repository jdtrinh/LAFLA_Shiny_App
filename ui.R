library(data.table)
library(dplyr)
library(DT)
library(ggplot2)
library(ggpubr)
library(highcharter)
library(janitor)
library(lubridate)
library(plotly)
library(readxl)
library(rsconnect)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(sqldf)
library(stringr)
library(zoo)
library(tidyr)
library(devtools)
library(choroplethrZip)


##################################################################################
###                               User Interface                               ###
##################################################################################


## Define the User Interface ##

ui <- dashboardPage(
  
  dashboardHeader(title = "LAFLA Shiny App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Rent Burden Datasets",
               tabName = "datasets-tab",
               icon = icon("table")),
      menuItem(text = "Rent Burden Heat Map", 
               tabName = "heatmap-tab",
               icon = icon("map"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "datasets-tab",
              # App title ----
              titlePanel("Rent Burden - Data Download Demo"),
              
              # Sidebar layout with input and output definitions ----
              sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(
                  
                  # Input: Choose dataset ----
                  selectInput("dataset", "Choose a dataset:",
                              choices = c("Median Income", "Median Rent", "Tenure")),
                  
                  # Button
                  downloadButton("downloadData", "Download")
                  
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                  
                  tableOutput("table")
                  
                )
                
              )),
      tabItem(tabName = "heatmap-tab",
              fluidPage(
                # Set theme
                theme = shinytheme("spacelab"),
                # Some help text
                h2("LA County Heat Maps"),
                h4("Make selections to generate a heat map."),
                # Sidebar layout with a input and output definitions
                sidebarLayout(
                  # Inputs
                  sidebarPanel(
                    # Data to View
                    radioButtons(inputId = "HM_val", 
                                 label = "Select Data to View", 
                                 choices = c("Total Data", "Demographics", "Housing", "Other"), 
                                 selected = "Total Data"),
                    
                    
                    uiOutput("map_a_val"),
                    uiOutput("map_b_val")                    
                    
                  ),
                  
                  # Plotly Chart Area
                  
                  mainPanel(
                    
                    fluidRow(
                      splitLayout(cellWidths = c("50%", "50%"), 
                                  h2("Map of Variable A"),
                                  h2("Map of Variable B")),
                      splitLayout(cellWidths = c("50%", "50%"), 
                                  plotOutput(
                                    outputId = "HM_map_a",
                                    height = 600,
                                    width = 600),
                                  plotOutput(
                                    outputId = "HM_map_b",
                                    height = 600,
                                    width = 600)
                      ),
                      splitLayout(cellWidths = c("50%", "50%"),
                                  downloadButton('download_var_a_map', 'Download Var A Map'),
                                  downloadButton('download_var_b_map', 'Download Var B Map')
                      ),
                      h4(strong("Notes:")),
                      h4("1. The legend can represent dollars, count, or percentages."),
                      h4("2. The zip codes are manually selected, and might not represent the governement definition of Greater Los Angeles.\n\n"),
                      
                      splitLayout(cellWidths = c("50%", "50%"),
                                  h2("Variable A Data"),
                                  h2("Variable B Data")),

                      splitLayout(cellWidths = c("50%", "50%"),
                                  DT::dataTableOutput(outputId = "DT_table_a"),
                                  DT::dataTableOutput(outputId = "DT_table_b")),

                      plotlyOutput(outputId = "PL_chart")
                      
                      
                    )
                    
                  )
                  
                )
              )
              
              
              
              
              
              
              
      )
      
    ),
    
    
    tags$head(tags$style(HTML('
        /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #e11631;
                              }
                              
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #e11631;
                              }
                              
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #e11631;
                              }        
                              
                              ')))
  )
  
)