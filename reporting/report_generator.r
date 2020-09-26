
brew2pdf <- function(input, output_dir, envir = parent.frame()) {

  brew_file_name <- basename(input)
  rnw_file_name <- sub(".brew$", ".Rnw", brew_file_name)
  tex_file_name <- sub(".brew$", ".tex", brew_file_name)
  pdf_file_name <- sub(".brew$", ".pdf", brew_file_name)

  rnw_file_path <- file.path(output_dir, rnw_file_name)
  tex_file_path <- file.path(output_dir, tex_file_name)
  pdf_file_path <- file.path(output_dir, pdf_file_name)

  brew(input, output = rnw_file_path, envir = envir)

  fileConn <- file(rnw_file_path)
  temp_file <- readLines(fileConn, encoding = "UTF-8")
  close(fileConn)
  fileConn <- file(tex_file_path)
  writeLines(temp_file, tex_file_path)
  close(fileConn)

  # when using figures knitr expects compilation to be done from the output directory
  # so unfortunately we need to temporarily change the working directory
  orig_wd <- setwd(output_dir)
  # return to the original working directory on function exit (including exit via error)
  on.exit(setwd(orig_wd))
  # knit2pdf(rnw_file_name, output = tex_file_name, envir = envir)

  tinytex::latexmk(tex_file_name, engine = "pdflatex")
}

# -------------------------------------------------------------------------------------------------------
# PDF output
# -------------------------------------------------------------------------------------------------------

generate_pdf_report <- function(input, trials, savepath) {
  input_brew = "./reporting/report_template.brew"

  output_dir = dirname(savepath)

  log_message(paste0('savepath: ', savepath))
  log_message(paste0('output_dir: ', output_dir))

  # pack up input into list
  options_list <- list()

  options_list$date_of_review <- Sys.Date()
  if (TRUE %in% input$flagged_trails) { options_list$flagged_trials_accepted <- "TRUE" } else { options_list$flagged_trials_accepted <- "FALSE" }
  if (FALSE %in% input$flagged_trails) { options_list$flagged_trials_rejected <- "TRUE" } else { options_list$flagged_trials_rejected <- "FALSE" }
  if ("NA" %in% input$flagged_trails) { options_list$flagged_trials_unreviewed <- "TRUE" } else { options_list$flagged_trials_unreviewed <- "FALSE" }

  eval_envir <- list2env(list(options_list = options_list, trials = trials), parent = baseenv())
  brew2pdf(input_brew, output_dir, envir = eval_envir)

  # cleanup
  # move file to correct location and name
  default_brew_gen_norm <- normalizePath(input_brew)

  default_pdf_gen_norm <- sub(".brew$", ".pdf", default_brew_gen_norm)
  if (file.exists(default_pdf_gen_norm)) {
    # savefile <- normalizePath(savefile)
    file.rename(default_pdf_gen_norm, savepath)
  }
  # if (file.exists(sub(".brew$", ".tex", default_brew_gen_norm))) {file.remove(sub(".brew$", ".tex", default_brew_gen_norm))}
  if (file.exists(sub(".brew$", ".Rnw", default_brew_gen_norm))) {
    file.remove(sub(".brew$", ".Rnw", default_brew_gen_norm))
  }
  if (dir.exists("./app_functions/report_module/figure")) {
    unlink("./app_functions/report_module/figure", recursive = TRUE)
  }
  log_message("COMPLETED REPORT CLEANUP")
  log_message(paste0("FILE SAVED AT: ", savepath))

  log_message("REPORT DONE")
}

# -------------------------------------------------------------------------------------------------------
# CSV output
# -------------------------------------------------------------------------------------------------------


generate_csv_report <- function(input, trials, savepath) {
  accepted_flag <- ''
  rejected_flag <- ''
  unreviewed_flag <- ''
  trials_without_expected_enrol <- ''
  completion_date_toggle_flag <- ''
  treatments_report <- ''
  outcomes_report <- ''
  trial_phase_report <- ''

  if ("TRUE" %in% input$flagged_trials) {
    accepted_flag <- 'Selected'
  } else {
    accepted_flag <- 'Not Selected'
  }

  if ("FALSE" %in% input$flagged_trials) {
    rejected_flag <- 'Selected'
  } else {
    rejected_flag <- 'Not Selected'
  }

  if ("NA" %in% input$flagged_trials) {
    unreviewed_flag <- 'Selected'
  } else {
    unreviewed_flag <- 'Not Selected'
  }

  if (input$enrollment_na_show == TRUE) {
    trials_without_expected_enrol <- 'Selected'
  } else {
    trials_without_expected_enrol <- 'Not Selected'
  }

  if (input$completion_date_toggle == TRUE) {
    completion_date_toggle_flag <- 'Selected'
  } else {
    completion_date_toggle_flag <- 'Not Selected'
  }

  if (is.null(input$trial_phase)) {
    trial_phase_report <- 'All'
  } else {
    trial_phase_report <- paste(input$trial_phase, collapse = "; ")
  }

  if (is.null(input$treatment)) {
    treatments_report <- 'All'
  } else {
    treatments_report <- paste(input$treatment, collapse = "; ")
  }

  if (is.null(input$outcome)) {
    outcomes_report <- 'All'
  } else {
    outcomes_report <- paste(input$outcome, collapse = "; ")
  }

  report_csv <- trials_subset_filtered()
  write(paste("Date Generated: ", Sys.Date()), file = savepath)
  write("Filter:", file = savepath, append = TRUE)
  write(paste("Trials with Accepted Flag: ", accepted_flag), file = savepath, append = TRUE)
  write(paste("Trials with Rejected Flag: ", rejected_flag), file = savepath, append = TRUE)
  write(paste("Trials with Unreviewed Flag: ", unreviewed_flag), file = savepath, append = TRUE)
  write(paste("Expected No. of Patients Greater than: ", input$expected_enrollment), file = savepath, append = TRUE)
  write(paste("Display Trials without Expected No of Patients Button Selected: ", trials_without_expected_enrol), file = savepath, append = TRUE)
  write(paste("Study Design: ", input$study_design), file = savepath, append = TRUE)
  write(paste("Trial Phase: ", trial_phase_report), file = savepath, append = TRUE)
  if (input$completion_date_toggle == TRUE) {
    write(paste("Filter by Completion Dates Selected: ", completion_date_toggle_flag), file = savepath, append = TRUE)
    write(paste("Trials Completed between: ", input$completion_date[1], " & ", input$completion_date[2]), file = savepath, append = TRUE)
  }
  write(paste0("Treatments Selected (", input$treatment_andor, "): ", treatments_report), file = savepath, append = TRUE)
  write(paste0("Outcomes Selected (", input$outcome_andor, "): ", outcomes_report), file = savepath, append = TRUE)
  write("", file = savepath, append = TRUE)
  write.table(trials_subset_filtered(), file = savepath, append = TRUE, sep = ",", row.names = FALSE)


}