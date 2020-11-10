##################################################################################
###                                 Set-up                                     ###
##################################################################################


## Import Libraries ##

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

# choroplethrZip is on github
if (!"choroplethrZip" %in% installed.packages()[, "Package"]) {
  install_github('arilamstein/choroplethrZip@v1.5.0')
}
library(choroplethrZip)

options(scipen=999)
rm(list=ls())

mydata <- read_xlsx("./Input/Merged LAFLA Data.xlsx")
# Clean Out Unusable Variables
mydata <- mydata %>% mutate(
  `Supervisorial District 1` = str_replace(mydata$`Supervisorial District 1`, "X", "1"),
  `Supervisorial District 2` = str_replace(mydata$`Supervisorial District 2`, "X", "2"),
  `Supervisorial District 3` = str_replace(mydata$`Supervisorial District 3`, "X", "3"),
  `Supervisorial District 4` = str_replace(mydata$`Supervisorial District 4`, "X", "4"),
  `Supervisorial District 5` = str_replace(mydata$`Supervisorial District 5`, "X", "5"))
mydata$`Supervisorial District` = as.integer(coalesce(mydata$`Supervisorial District 1`,
                                                      mydata$`Supervisorial District 2`,
                                                      mydata$`Supervisorial District 3`,
                                                      mydata$`Supervisorial District 4`,
                                                      mydata$`Supervisorial District 5`))

mydata <- subset(mydata, select = -c(NAME, `Zip Code.x`, `Zip Code.y`, name, zip, `Area Name`,
                                     `Unlimited Civil (exclude Personal Injury)`,
                                     `Unlimited Civil (Personal Injury)`,
                                     `Family Law`, `Restraining Orders`, Probate,
                                     `Limited Civil (exclude Collection)`,
                                     `Limited Civil (Collection)`,
                                     `Limited Unlawful Detainer`, `Small Claims`,
                                     city, state, county, year, `Supervisorial District 1`,
                                     `Supervisorial District 2`, `Supervisorial District 3`, 
                                     `Supervisorial District 4`, `Supervisorial District 5`))
names(mydata)


path   <- "./Input/"
income <- read.csv(paste0(path, "income.csv"))
income <- row_to_names(income, row_number = 1)
colnames(income) <- c("id", "Name", "Median Household Income (Estimate)", "Median Household Income (MOE)")
rent <- read.csv(paste0(path, "rent.csv"))
rent <- row_to_names(rent, row_number = 1)
colnames(rent) <- c("id", "Name", "Median Gross Rent (Estimate)", "Median Gross Rent (MOE)")
tenure <- read.csv(paste0(path, "tenure.csv"))
tenure <- row_to_names(tenure, row_number = 1)
colnames(tenure) <- c("id", "Name", "Total (Estimate)", "Total (MOE)", "Owner Occupied (Estimate)", "Owner Occupied (MOE)",
                      "Renter Occupied (Estimate)", "Renter Occupied (MOE)")


##################################################################################
###                                 Server                                     ###
##################################################################################


## Define the Server Functionality ##

server <- function(input, output, session){

  # Set path
  Dir       <- "./"
  DirInput  <- "./Input"


  ###########################################################################
  # Import data
  ###########################################################################

  # Import federal housing data
  filename <- paste(DirInput, "Merged LAFLA Data (Full Joins).xlsx", sep="/")
  masterData <- mydata %>%
    # Make GEOID a character
    mutate(GEOID = as.character(GEOID))

  ###########################################################################
  # Filter data
  ###########################################################################

  # Call dataset from choroplethrZip
  data(zip.regions)

  # Filter to LA Zip codes from choroplethrZip
  LARegions <- as.character(c(90601, 90602, 90603, 90605, 90631, 91006, 91007, 91008, 91010, 91016, 91024,
                              91030, 91108, 91706, 91722, 91723, 91724, 91731, 91732, 91733, 91741,
                              91744, 91745, 91746, 91748, 91754, 91765, 91770, 91773, 91775, 91776,
                              91780, 91789, 91790, 91791, 91792, 91801, 91803, 90031, 90032, 90041,
                              90042, 90065, 91204, 91205, 90004, 90005, 90006, 90012, 90013, 90014,
                              90015, 90017, 90019, 90021, 90026, 90027, 90028, 90035, 90036, 90038,
                              90039, 90046, 90048, 90057, 90068, 90069, 90071, 90022, 90023, 90033,
                              90063, 90024, 90025, 90034, 90049, 90056, 90064, 90066, 90067, 90073,
                              90077, 90094, 90210, 90212, 90230, 90232, 90272, 90291, 90292, 90401,
                              90402, 90403, 90404, 90405, 90001, 90002, 90003, 90007, 90008, 90011,
                              90016, 90018, 90037, 90043, 90044, 90047, 90059, 90061, 90062, 90089,
                              90220, 90305, 90040, 90058, 90201, 90240, 90241, 90242, 90255, 90262,
                              90270, 90280, 90604, 90606, 90638, 90640, 90650, 90660, 90670, 90703,
                              90706, 90723, 90045, 90245, 90249, 90250, 90254, 90260, 90266, 90274,
                              90275, 90277, 90278, 90293, 90301, 90302, 90303, 90304, 90501, 90503,
                              90504, 90505, 90506, 90717, 90221, 90502, 90710, 90712, 90713, 90715,
                              90716, 90731, 90732, 90744, 90745, 90746, 90755, 90802, 90803, 90804,
                              90805, 90806, 90807, 90808, 90810, 90813, 90814, 90815, 90822, 90831))

  # The data.frame that you provide to zip_choropleth must have one column named
  # "region" and one column named "value". Your entries for "region" must exactly
  # match how regions are named in the map which choroplethr uses.

  Select_Var_A <- eventReactive(input$HM_map_a_val, {
    # Input interested variable
    var = c(input$HM_map_a_val)

    masterDataMap <- masterData %>%
      # Filter to LA FIPS code
      filter(GEOID %in% LARegions) %>%
      # Only keep interested variables
      select(GEOID, var)

    # Rename variables to work with package
    names(masterDataMap) <- c("region", "value")

    ###########################################################################
    # Create map
    ###########################################################################

    # Use the choroplethrZip package
    map <- zip_choropleth(masterDataMap,
                          # Filter to LA codes
                          zip_zoom = LARegions,
                          legend   = "Count") +
      # Change the title theme
      theme(plot.title = element_text(size = 12, face = "bold", hjust=0.5, margin = margin(b = 20, r=0, l=0, t=20)),
            legend.title = element_text( size=10, face="bold")) +
      # Add a title
      ggtitle(paste0(var,"\n", "Greater Los Angeles"))

    map
  })


  output$HM_map_a <- renderPlot({
    print(Select_Var_A())
  })

  Select_Var_B <- eventReactive(input$HM_map_b_val, {
    # Input interested variable
    var = c(input$HM_map_b_val)

    masterDataMap <- masterData %>%
      # Filter to LA FIPS code
      filter(GEOID %in% LARegions) %>%
      # Only keep interested variables
      select(GEOID, var)

    # Rename variables to work with package
    names(masterDataMap) <- c("region", "value")

    ###########################################################################
    # Create map
    ###########################################################################

    # Use the choroplethrZip package
    map <- zip_choropleth(masterDataMap,
                          # Filter to LA codes
                          zip_zoom = LARegions,
                          legend   = "Count") +
      # Change the title theme
      theme(plot.title = element_text(size = 12, face = "bold", hjust=0.5, margin = margin(b = 20, r=0, l=0, t=20)),
            legend.title = element_text( size=10, face="bold")) +
      # Add a title
      ggtitle(paste0(var,"\n", "Greater Los Angeles"))

    map
  })


  output$HM_map_b <- renderPlot({
    print(Select_Var_B())
  })

  output$download_var_a_map <- downloadHandler(
    filename = "Variable A Map.png",
    content = function(file) {
      png(file)
      print(Select_Var_A())
      dev.off()
    })

  output$download_var_b_map <- downloadHandler(
    filename = "Variable B Map.png",
    content = function(file) {
      png(file)
      print(Select_Var_B())
      dev.off()
    })

  select_variable_grouping <- eventReactive(input$HM_val, {

    if(input$HM_val == "Total Data"){
      cols = c(2:521)
      data <- mydata[, cols]
    }
    else if (input$HM_val == "Demographics"){
      cols = c(2:510)
      data <- mydata[, cols]
    }
    else if (input$HM_val == "Housing"){
      housing_vars  <- c("B25003", "B25070", "B25077", "B25075",
                         "B25064", "B25065", "B25066", "B25071",
                         "B25070", "B25034")
      other_housing <- c("federallyBackedLoans", "FannieMaeOwned", "GinnieMaeOwned",
                         "FreddieMacOwned", "FarmerMacOwned", "nonProfitHousingCount")
      data <- mydata[, grepl("B25003|B25070|B25077|B25075|B25064|B25065|B25066|B25071|B25070|B25034|federallyBackedLoans|FannieMaeOwned|GinnieMaeOwned|FreddieMacOwned|FarmerMacOwned|nonProfitHousingCount",
                             names(mydata))]
    }
    else if (input$HM_val == "Other"){
      cols = c(517:521)
      data <- mydata[, cols]
    }
    data
  })

  output$map_a_val <- renderUI(
    # Map Variable A
    shinyWidgets::pickerInput(inputId = "HM_map_a_val",
                              label = "Map Variable A",
                              choices = names(select_variable_grouping()),
                              selected = "Total Mid-March Employees",
                              multiple = FALSE,
                              options = list(`actions-box` = TRUE)
    )
  )

  output$map_b_val <- renderUI(
    # Map Variable B
    shinyWidgets::pickerInput(inputId = "HM_map_b_val",
                              label = "Map Variable B",
                              choices = names(select_variable_grouping()),
                              selected = "B25064_001",
                              multiple = FALSE,
                              options = list(`actions-box` = TRUE)
    )
  )

  Var_A <- eventReactive(input$HM_map_a_val, {
    # Input interested variable
    var = c(input$HM_map_a_val)

    masterDataMap <- masterData %>%
      # Filter to LA FIPS code
      filter(GEOID %in% LARegions) %>%
      # Only keep interested variables
      select(GEOID, var)

    # Rename variables to work with package
    names(masterDataMap) <- c("ZIP Code", input$HM_map_a_val)
    masterDataMap
  })

  Var_B <- eventReactive(input$HM_map_b_val, {
    # Input interested variable
    var = c(input$HM_map_b_val)

    masterDataMap <- masterData %>%
      # Filter to LA FIPS code
      filter(GEOID %in% LARegions) %>%
      # Only keep interested variables
      select(GEOID, var)

    # Rename variables to work with package
    names(masterDataMap) <- c("ZIP Code", input$HM_map_b_val)
    masterDataMap
  })

  output$DT_table_a <- DT::renderDataTable(
    Var_A(),
    rownames = FALSE,
  )

  output$DT_table_b <- DT::renderDataTable(
    Var_B(),
    rownames = FALSE,
  )

  Var_AB <- eventReactive(c(input$HM_map_a_val, input$HM_map_b_val), {
    # Input interested variable
    var_A = c(input$HM_map_a_val)
    var_B = c(input$HM_map_b_val)

    masterDataMap <- masterData %>%
      # Filter to LA FIPS code
      filter(GEOID %in% LARegions) %>%
      # Only keep interested variables
      select(GEOID, var_A, var_B)

    # Rename variables to work with package
    names(masterDataMap) <- c("ZIP Code", input$HM_map_a_val, input$HM_map_b_val)
    masterDataMap
  })

  output$PL_chart <- renderPlotly({
    plot_data = Var_AB()
    x <- plot_data[[input$HM_map_a_val]]
    y <- plot_data[[input$HM_map_b_val]]


    fig <- plot_ly(data = plot_data, x = ~x, y = ~y,
                   type = 'scatter',
                   marker = list(size = 10,
                                 color = 'rgba(255, 182, 193, .9)',
                                 line = list(color = 'rgba(152, 0, 0, .8)',
                                             width = 2)))
    fig <- fig %>% layout(title = paste(input$HM_map_a_val, " vs. ", input$HM_map_b_val, sep=""),
                          yaxis = list(title = input$HM_map_b_val,
                                       zeroline = FALSE),
                          xaxis = list(title = input$HM_map_a_val,
                                       zeroline = FALSE))


    fig
  })

  ### Aurora's Code ###

  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "Median Income" = income,
           "Median Rent" = rent,
           "Tenure" = tenure)
  })

  # Table of selected dataset ----
  output$table <- renderTable({
    datasetInput()
  })

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
}
