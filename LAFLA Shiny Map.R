list.of.packages <- c("stringr", "data.table", "dplyr", "DT", "ggplot2", "ggpubr", "tidyr",
                      "janitor", "lubridate", "zoo", "readxl", "rsconnect", "shiny", "proto",
                      "shinydashboard", "shinyjs", "shinythemes", "shinyWidgets", "sqldf" #,
                      # "plotly", "highcharter", "devtools"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# library(devtools)
# install_github('arilamstein/choroplethrZip@v1.5.0')

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


runApp(getwd())