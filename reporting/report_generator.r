# Utility function required for PDF reporting (old)
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

build_filter <- function(input) {
  settings <- c(
    'Date generated',
    'Accepted Trails',
    'Rejected Trials',
    'Unreviewed Trials',
    'Trials without Expected No of Patients',
    'Expected No. of Patients Greater than',
    'Study Design',
    'Trial Phase(s) included',
    'Treatment(s) included',
    'Outcome(s) included',
    'Completion date range'
  )

  values <- c(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    ifelse("TRUE" %in% input$flagged_trials, 'Included', 'Not Included'),
    ifelse("FALSE" %in% input$flagged_trials, 'Included', 'Not Included'),
    ifelse("NA" %in% input$flagged_trials, 'Included', 'Not Included'),
    ifelse(input$enrollment_na_show, 'Included', 'Not Included'),
    input$expected_enrollment,
    input$study_design,
    ifelse(is.null(input$trial_phase), 'All', paste(input$trial_phase, collapse = " OR ")),
    ifelse(is.null(input$treatment), 'All', paste(input$treatment, collapse = paste0(' ', input$treatment_andor, ' '))), 
    ifelse(is.null(input$outcome), 'All', paste(input$outcome, collapse = paste0(' ', input$outcome_andor, ' '))), 
    ifelse(input$completion_date_toggle, paste(input$completion_date[1], " - ", input$completion_date[2]), 'All')
  )

  filter <- data.frame(settings, values)
  names(filter) <- c('Setting', 'Value')

  summary(filter)
  return(filter)
}


# -------------------------------------------------------------------------------------------------------
# PDF output
# -------------------------------------------------------------------------------------------------------

generate_pdf_report <- function(input, trials, savepath) {
  log_message(paste0('PDF Report - Saving to: ', savepath))

  rmarkdown::render('./reporting/report.Rmd', params = c(filter = build_filter(input), df = trials))

  log_message("PDF Report - complete")
}


# -------------------------------------------------------------------------------------------------------
# CSV output
# -------------------------------------------------------------------------------------------------------

generate_csv_report <- function(input, trials, savepath) {
  log_message(paste0('CSV Report - Saving to: ', savepath))

  # write out the filter
  write.table(build_filter(input), file = savepath, append = TRUE, sep = ",", row.names = FALSE)
  # write a blank line
  write("", file = savepath, append = TRUE)
  # write the table itself
  write.table(trials, file = savepath, append = TRUE, sep = ",", row.names = FALSE)

  log_message("CSV Report - complete")
}
