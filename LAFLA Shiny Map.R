list.of.packages <- c("proto", "data.table", "dplyr", "DT", "ggplot2", "ggpubr", "highcharter",
                      "janitor", "lubridate", "plotly", "readxl", "rsconnect", "shiny",
                      "shinydashboard", "shinyjs", "shinythemes", "shinyWidgets", "sqldf",
                      "stringr", "zoo", "tidyr", "devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(devtools)
install_github('arilamstein/choroplethrZip@v1.5.0')

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
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(sqldf)
library(stringr)
library(zoo)
library(tidyr)
library(devtools)
library(choroplethrZip)

options(scipen=999)
rm(list=ls())

mydata <- read_xlsx("./Input/Merged LAFLA Data (Full Joins).xlsx")

# Create a single flag for supervisoral distit
mydata <- mydata %>%
  mutate(supervisorial_district = gsub("  ", " ", str_trim(paste(ifelse(is.na(`Supervisorial District 1`), "", "1"),
                                                                 ifelse(is.na(`Supervisorial District 2`), "", "2"),
                                                                 ifelse(is.na(`Supervisorial District 3`), "", "3"),
                                                                 ifelse(is.na(`Supervisorial District 4`), "", "4"),
                                                                 ifelse(is.na(`Supervisorial District 5`), "", "5"),
                                                                 sep=" "))),
         # Only capitalize the first letter in each name 
         city = str_to_title(city))

# Clean Out Unusable Variables

mydata <- subset(mydata, select = -c(NAME, `Zip Code.x`, `Zip Code.y`, name, zip, `Area Name`,
                                     `Unlimited Civil (exclude Personal Injury)`,
                                     `Unlimited Civil (Personal Injury)`,
                                     `Family Law`, `Restraining Orders`, Probate,
                                     `Limited Civil (exclude Collection)`,
                                     `Limited Civil (Collection)`,
                                     `Limited Unlawful Detainer`, `Small Claims`,
                                     state, county, year, `Supervisorial District 1`,
                                     `Supervisorial District 2`, `Supervisorial District 3`, 
                                     `Supervisorial District 4`, `Supervisorial District 5`))
names(mydata)

# Create the list of variable names
columnChoices <- colnames(mydata)
zipChoices <- unique(mydata$GEOID)
cityChoices <- c("All Cities", unique(mydata$city))
supervisorChoices <- c("1", "2", "3", "4")
congressionalChoices <- c("TBD")


runApp(getwd())