brew2pdf <- function(input, output_dir, envir = parent.frame(), ...) {
  
  brew_file_name <- basename(input)
  rnw_file_name <- sub(".brew$", ".Rnw", brew_file_name)
  tex_file_name <- sub(".brew$", ".tex", brew_file_name)
  pdf_file_name <- sub(".brew$", ".pdf", brew_file_name)
  
  rnw_file_path <- file.path(output_dir, rnw_file_name)
  tex_file_path <- file.path(output_dir, tex_file_name)
  pdf_file_path <- file.path(output_dir, pdf_file_name)
  
  brew(input, output = rnw_file_path, envir = envir)
  
  fileConn<-file(rnw_file_path)
  temp_file <- readLines(fileConn, encoding = "UTF-8")
  close(fileConn)
  fileConn<-file(tex_file_path)
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

gen_report <- function(input, trials, input_brew = "./reporting/report_template.brew",
                       output_dir = "./reporting", ...) {
  # pack up input into list
  options_list <- list()
  
  options_list$date_of_review <- Sys.Date()
  if (TRUE %in% input$flagged_trails) {options_list$flagged_trials_accepted <- "TRUE"} else {options_list$flagged_trials_accepted <- "FALSE"}
  if (FALSE %in% input$flagged_trails) {options_list$flagged_trials_rejected <- "TRUE"} else {options_list$flagged_trials_rejected <- "FALSE"}
  if ("NA" %in% input$flagged_trails) {options_list$flagged_trials_unreviewed <- "TRUE"} else {options_list$flagged_trials_unreviewed <- "FALSE"}
  
  eval_envir <- list2env(list(options_list = options_list, trials = trials), parent=baseenv())
  brew2pdf(input_brew, output_dir, envir = eval_envir, ...)
  
  print("REPORT DONE")
}