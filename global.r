#install.packages(c("dplyr", "DT", "shiny", "shinydashboard", "shinyjs", "shinythemes", "tidyr", "lubridate", "purrr"))

library(dplyr)
library(DT)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinythemes)
library(tidyr)
library(lubridate)
library(purrr)
library(stringr)

date_data_transfer <- "2020-07-21"

trials <- get(load("/data/dat_processed_and_network.RData"))

id <- rownames(trials)
trials <- cbind(id=id, trials)

trials_subset <- trials %>%
  select(id,
      trial_id,
      public_title,
      institution,
      date_primary_completion,
      expected_enrollment,
      patient_setting,
      study_design_final,
      number_of_arms_final,
      url,
      corrected_treatment_name,
      outcome) %>%
  rename(title = public_title,
         completion_date = date_primary_completion,
         study_design = study_design_final,
         arms = number_of_arms_final,
         treatment = corrected_treatment_name) 

expected_enrollment_max <- max(trials_subset$expected_enrollment, na.rm = TRUE)
study_design_levels <- levels(factor(trials_subset$study_design))
completion_date_min <- min(as.Date(trials_subset$completion_date), na.rm = TRUE)
completion_date_max <- max(as.Date(trials_subset$completion_date), na.rm = TRUE)
today <- Sys.Date()
today_plus_one_month <- Sys.Date() %m+% months(1)
treatments <- trials_subset$treatment %>% 
  strsplit(", ") %>% reduce(c) %>% 
  strsplit(" + ", fixed = TRUE) %>% reduce(c) %>% 
  unique() %>% sort()
outcomes <- trials_subset$outcome %>% 
  strsplit(", ") %>% reduce(c) %>% 
  unique() %>% sort()
