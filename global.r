# Initialisation requirements e.g. library imports
if (exists("xap.conn")) {
  .libPaths("../R/3.6.3")
}

# Utility function to print log messages with a timestamp
# we use cat() to stderr as that will render in the log in a Shiny app
log_message <- function(message) {
  cat(file = stderr(), format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " - ", message, "\n")
}

# Dependencies
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
library(rmarkdown)
library(knitr)
library(kableExtra)
library(brew)
library(dplR)
library(readr)
library(pander)


# Module(s)
source("./reporting/report_generator.r")
