# Take the current filter options and construct a dataframe for ease of display
build_filter <- function(input) {
  settings <- c(
    'Date generated (UTC)',
    'Accepted Trails',
    'Rejected Trials',
    'Unreviewed Trials',
    'Trials without Expected # of Patients',
    'Expected # of Patients Greater than',
    'Study Design',
    'Trial Phase(s) included',
    'Treatment(s) included',
    'Outcome(s) included',
    'Completion date range',
    'Sorted by column'
  )
  
  table_columns <- c("Trial Id", "Title", "Institution", "Completion", "Size", "Patient setting", "Study design", "Arms", "Treatment", "Outcome", "Phase", "Flag")
  # order_column <- input$trials_order$column
  # order_column_direction <- input$trials_order$direction
  order_column <- 4 # hard code by increasing completion date (ignore table sorting)
  order_column_direction <- "asc" # hard code by increasing completion date (ignore table sorting)
  
  
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
    ifelse(input$completion_date_toggle, paste(input$completion_date[1], " - ", input$completion_date[2]), 'All'),
    ifelse(order_column != "", paste0(table_columns[order_column], ", (", order_column_direction, ")"), "Unsorted")
  )

  filter <- data.frame(settings, values)
  names(filter) <- c('Setting', 'Value')

  # summary(filter)
  return(filter)
}

# -------------------------------------------------------------------------------------------------------
# Sanitize output for LaTeX
# -------------------------------------------------------------------------------------------------------

custom_sanitize <- function(x) {
	
	# # replace any ampersands so they are caught by filter
	# x <- gsub("\u26", "&", x)
	# x <- gsub("\uff06", "&", x)
	# x <- gsub("\ufe60", "&", x)
	
	# # can't remember what shiny::HTML was for. ignoring for now
	# x <- Hmisc::latexTranslate(enc2utf8(shiny::HTML(toString(x))))
	# x <- Hmisc::latexTranslate(enc2utf8(toString(x)))
	
	# # latexify seems to be a more robust function for doing this
	# but requires a couple of fixes
	# x <- dplR::latexify(enc2utf8(toString(x)), doublebackslash = TRUE)
	
	# # fix latexify quote changes
	# x <- gsub("\\\\textquotesingle", "'", x)
	# x <- gsub("\\\\textquotedbl", '"', x)
	
	# # fix guillemet changes
	# x <- gsub("\\\\guillemotleft", "$\\\\ll$", x)
	# x <- gsub("\\\\guillemotright", "$\\\\gg$", x)
	
	# # fix beta symbol
	# x <- gsub("\\\\ss", "$\\\\beta$", x)
	
	# # normal less/greater and equal sign
	# x <- gsub("\u2264", "$\\\\leq$", x)
	# x <- gsub("\u2265", "$\\\\geq$", x)
	
	# # slanted less/greater than and equal sign
	# x <- gsub("\u2a7d", "$\\\\leqslant$", x)
	# x <- gsub("\u2a7e", "$\\\\geqslant$", x)
	
	# # greek letters
	# x <- gsub("\u03b1", "$\\\\alpha$", x)
	# x <- gsub("\u03b2", "$\\\\beta$", x)
	# x <- gsub("\u03b3", "$\\\\gamma$", x)
	# x <- gsub("\u03b4", "$\\\\delta$", x)
	# x <- gsub("\u03b5", "$\\\\epsilon$", x)
	# x <- gsub("\u03b6", "$\\\\zeta$", x)
	# x <- gsub("\u03b7", "$\\\\eta$", x)
	# x <- gsub("\u03b8", "$\\\\theta$", x)
	# x <- gsub("\u03b9", "$\\\\iota$", x)
	# x <- gsub("\u03ba", "$\\\\kappa$", x)
	# x <- gsub("\u03bb", "$\\\\lambda$", x)
	# x <- gsub("\u03bc", "$\\\\mu$", x)
	# x <- gsub("\u03bd", "$\\\\nu$", x)
	# x <- gsub("\u03be", "$\\\\xi$", x)
	# x <- gsub("\u03bf", "$o$", x) # x <- gsub("\u03bf", "$\\\\omicron$", x) # Doesn't exist in latex?
	# x <- gsub("\u03c0", "$\\\\pi$", x)
	# x <- gsub("\u03c1", "$\\\\rho$", x)
	# x <- gsub("\u03c2", "$\\\\varsigma$", x)
	# x <- gsub("\u03c3", "$\\\\sigma$", x)
	# x <- gsub("\u03c4", "$\\\\tau$", x)
	# x <- gsub("\u03c5", "$\\\\upsilon$", x)
	# x <- gsub("\u03c6", "$\\\\phi$", x)
	# x <- gsub("\u03c7", "$\\\\chi$", x)
	# x <- gsub("\u03c8", "$\\\\psi$", x)
	# x <- gsub("\u03c9", "$\\\\omega$", x)
	
	# remove any other unicode character 
	x <- enc2utf8(toString(x))
	x <- gsub("[^\x20-\x7E]", "", x)
	
	return(x)

}

# -------------------------------------------------------------------------------------------------------
# PDF output
# -------------------------------------------------------------------------------------------------------

generate_pdf_report <- function(input, trials, savepath) {
  log_message(paste0('PDF Report - Saving to: ', savepath))

  output_dir <- dirname(savepath)
  output_file <- basename(savepath)
  filter <- build_filter(input)
  
  df <- trials 
  # sort trials by same table sorting
  
  # Sanitze output for text columns
  df$scientific_title          <- lapply(df$scientific_title, custom_sanitize)
  df$institution               <- lapply(df$institution, custom_sanitize)
  df$patient_setting           <- lapply(df$patient_setting, custom_sanitize)
  df$study_design_final        <- lapply(df$study_design_final, custom_sanitize)
  df$corrected_treatment_name  <- lapply(df$corrected_treatment_name, custom_sanitize)
  df$outcome                   <- lapply(df$outcome, custom_sanitize)
  df$phase                     <- lapply(df$phase, custom_sanitize)

  # Reduce the size of 'Flag' to [Y, N, -]
  df$flag                      <- lapply(df$flag, function(x) { ifelse(is.na(x), '-', ifelse(x, 'Y', 'N')) })

  rmarkdown::render('./reporting/report.Rmd', output_file=output_file, output_dir=output_dir, intermediates_dir=output_dir, clean=TRUE)

  log_message("PDF Report - complete")
}

# -------------------------------------------------------------------------------------------------------
# CSV output
# -------------------------------------------------------------------------------------------------------

generate_csv_report <- function(input, trials, savepath) {
  log_message(paste0('CSV Report - Saving to: ', savepath))

  # write out the filter
  write.table(build_filter(input), file = savepath, append = FALSE, sep = ",", row.names = FALSE)
  # write a blank line
  write("", file = savepath, append = TRUE)
  # write column headers without it throwing a warning
  write(paste0(names(trials), collapse=","), file = savepath, append = TRUE)
  # write the table itself
  write.table(trials, file = savepath, append = TRUE, sep = ",", row.names = FALSE, col.names=FALSE)

  log_message("CSV Report - complete")
}