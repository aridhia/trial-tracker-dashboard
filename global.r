# Initialisation requirements e.g. library imports
if (exists("xap.conn")) {
  .libPaths("../R/3.6.3")
}

log_message <- function(message) {
  cat(file = stderr(), format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " - ", message, "\n")
}

library(DBI)
library(RPostgres)
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
library(shinyFiles)
# For reporting
library(knitr)
library(kableExtra)
library(brew)
library(dplR)

source("./reporting/report_generator.r")

