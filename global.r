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
library(ggplot2)
library(plotly)

date_data_transfer <- "2020-07-21"

trials <- get(load("data/dat_processed_and_network.RData"))

trials_subset <- trials %>%
  select(trial_id,
      scientific_title,
      institution,
      date_primary_completion,
      expected_enrollment,
      patient_setting,
      study_design_final,
      number_of_arms_final,
      corrected_treatment_name,
      outcome)

expected_enrollment_max <- max(trials_subset$expected_enrollment, na.rm = TRUE)
study_design_levels <- levels(factor(trials_subset$study_design))
completion_date_min <- min(as.Date(trials_subset$date_primary_completion), na.rm = TRUE)
completion_date_max <- max(as.Date(trials_subset$date_primary_completion), na.rm = TRUE)
today <- Sys.Date()
today_plus_one_month <- Sys.Date() %m+% months(1)
treatments <- trials_subset$corrected_treatment_name %>% 
  strsplit(", ") %>% reduce(c) %>% 
  strsplit(" + ", fixed = TRUE) %>% reduce(c) %>%
  trimws() %>%
  unique() %>% sort()
outcomes <- trials_subset$outcome %>% 
  strsplit(", ") %>% reduce(c) %>% 
  trimws() %>%
  unique() %>% sort()

outcome_filter_function <- function(entry, outcomes, logic = "AND") {
  if (is.null(outcomes)) {return(TRUE)}
  
  entry_split <- entry %>%
    strsplit(", ") %>% reduce(c)
  
  if (logic == "AND") {
    return (all(outcomes %in% entry_split))
  } else if (logic == "OR") {
    return (any(outcomes %in% entry_split))
  }
}

treatment_filter_function <- function(entry, treatments, logic = "AND") {
  if (is.null(treatments)) {return(TRUE)}
  
  entry_split <- entry %>%
    strsplit(", ") %>% reduce(c) %>% 
    strsplit(" + ", fixed = TRUE) %>% reduce(c)
  
  if (logic == "AND") {
    return (all(treatments %in% entry_split))
  } else if (logic == "OR") {
    return (any(treatments %in% entry_split))
  }
}

get_count_of_treatments <- function(){
  
  treatment_count_df <- data.frame(treatment=character(), count=integer())
  
  for (treatment_item in treatments){
    counter <- 0
    for (row in 1:nrow(trials)){
      if (grepl(treatment_item, trials[row, "corrected_treatment_name"], fixed = TRUE)){
        counter <- counter + 1
      }
    }
    treatment_count_df <- rbind(treatment_count_df, data.frame(treatment=treatment_item, count=counter))
    # print(treatment_item)
    # print(counter)
  }
  treatment_count_df_sorted <- treatment_count_df[order(-treatment_count_df$count),]
  treatment_count_df_sorted <- head(treatment_count_df_sorted, 20)
  return(treatment_count_df_sorted)
}

get_count_of_outcomes <- function(){
  
  outcomes_count_df <- data.frame(outcomes=character(), count=integer())
  
  for (outcomes_item in outcomes){
    counter <- 0
    for (row in 1:nrow(trials)){
      if (grepl(outcomes_item, trials[row, "outcome"], fixed = TRUE)){
        counter <- counter + 1
      }
    }
    outcomes_count_df <- rbind(outcomes_count_df, data.frame(outcome=outcomes_item, count=counter))
    # print(outcomes_item)
    # print(counter)
  }
  outcomes_count_df_sorted <- outcomes_count_df[order(-outcomes_count_df$count),]
  outcomes_count_df_sorted <- head(outcomes_count_df_sorted, 20)
  return(outcomes_count_df_sorted)
}

get_completed_trials <- function(){
  counts <- table(status = trials$recruitment_status)
  counts_dataframe <- as.data.frame(counts)
  completed <- subset(counts_dataframe, (status == 'Completed'), select=c(Freq))
  return(completed[1,1])
}

treatment_count_df <- get_count_of_treatments()
outcomes_count_df <- get_count_of_outcomes()
completed_trials = get_completed_trials()
