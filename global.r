#install.packages(c("dplyr", "DT", "shiny", "shinydashboard", "shinyjs", "shinythemes", "tidyr", "lubridate", "purrr", "stringr", "plotly", "DBI", "RPostgres"))

if(exists("xap.conn")){
  .libPaths("../R/3.6.3")
}

options(encoding = 'UTF-8')

library(dplyr)
library(DT)
library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyr)
library(lubridate)
library(purrr)
library(stringr)
library(plotly)
library(DBI)
library(RPostgres)

library(knitr)
library(kableExtra)
library(brew)

source("./reporting/reporting_utils.r")


