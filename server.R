options(connectionObserver = NULL)

server <- function(input, output, session) {
  # fix for mini_app greying-out after 10 min of inactivity
  autoInvalidate <- reactiveTimer(30000)
  observe({
    autoInvalidate()
    # Print a dot - no need to log
    print('.')
  })

  # TODO - refactor this to the database
  date_data_transfer <- "2020-09-15"

  # -------------------------------------------------------------------------------------------------------
  # Fetch data from the database
  # -------------------------------------------------------------------------------------------------------
  # Test the connection - 
  shinyjs::addClass(id="error_message", class="hidden")
  # 
  Sys.setenv(PGHOST = "10.0.0.4") # ensures WS can connect to database even if DNS fails
  if (!dbCanConnect(RPostgres::Postgres(), dbname = Sys.getenv("PGDATABASE"), 
                                          host = Sys.getenv("PGHOST"), 
                                          port = Sys.getenv("PORT"), 
                                          user = Sys.getenv("PGUSER"), 
                                          password = Sys.getenv("PGPASSWORD"))) {
    # Sys.getenv("PGHOST") maybe blank if default but it's informative anyway                    
    error <- paste0('Failed to connect to database: (', Sys.getenv("PGHOST"), ')')                     
  
    # Fatal error: Log, notify and display an error
    log_message(error)
    showNotification(error, duration = 15, type = "error")
    output$error_message <- renderText({error})
    shinyjs::addClass(id = "loading_screen", class = "hidden")
    shinyjs::removeClass(id="error_message", class="hidden")
  } else {
    con <- dbConnect(RPostgres::Postgres(), dbname = Sys.getenv("PGDATABASE"), 
                                            host = Sys.getenv("PGHOST"), 
                                            port = Sys.getenv("PORT"), 
                                            user = Sys.getenv("PGUSER"), 
                                            password = Sys.getenv("PGPASSWORD"))

    log_message(paste0('Connected to database: (', Sys.getenv("PGHOST"), ')'))
  
    trials_original = RPostgres::dbGetQuery(con, "SELECT * FROM combined_view;")

    trials <- trials_original %>% select(-c(state_name, state_lon, state_lat, country_name, iso3_code))
    # %>% unique() # unique not working anymore with new columns (?)
    trials <- trials[!duplicated(trials$trial_id),]

    log_message(paste0('Loaded ', nrow(trials_original), ' in trials_original'))
    log_message(paste0('Unique trials: ', nrow(trials)))

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
                            outcome,
                            phase,
                            flag)

    expected_enrollment_max <- max(trials_subset$expected_enrollment, na.rm = TRUE)
    study_design_levels <- levels(factor(trials_subset$study_design))
    phases <- levels(factor(trials_subset$phase))
    completion_date_min <- min(as.Date(trials_subset$date_primary_completion), na.rm = TRUE)
    completion_date_max <- max(as.Date(trials_subset$date_primary_completion), na.rm = TRUE)
    today <- Sys.Date()
    today_plus_one_month <- Sys.Date() %m+% months(1)
    treatments <- trials_subset$corrected_treatment_name %>%
                      strsplit(", ") %>% 
                      reduce(c) %>%
                      strsplit(" + ", fixed = TRUE) %>% 
                      reduce(c) %>%
                      trimws() %>%
                      unique() %>% 
                      sort()

    outcomes <- trials_subset$outcome %>%
                  strsplit(", ") %>% 
                  reduce(c) %>%
                  trimws() %>%
                  unique() %>% 
                  sort()

    outcome_filter_function <- function(entry, outcomes, logic = "AND") {
      if (is.null(outcomes)) { return(TRUE) }

      entry_split <- entry %>%
                      strsplit(", ") %>% 
                      reduce(c)

      if (logic == "AND") {
        return(all(outcomes %in% entry_split))
      } else if (logic == "OR") {
        return(any(outcomes %in% entry_split))
      }
    }

    treatment_filter_function <- function(entry, treatments, logic = "AND") {
      if (is.null(treatments)) { return(TRUE) }

      entry_split <- entry %>%
        strsplit(", ") %>% reduce(c) %>%
        strsplit(" + ", fixed = TRUE) %>% reduce(c)

      if (logic == "AND") {
        return(all(treatments %in% entry_split))
      } else if (logic == "OR") {
        return(any(treatments %in% entry_split))
      }
    }

    review_filter_function <- function(entry, reviewed) {
      if (is.null(reviewed)) { return(FALSE) }
      if (!is.na(entry)) {
        if ("TRUE" %in% reviewed && as.logical(entry)) { return(TRUE) }
        if ("FALSE" %in% reviewed && !as.logical(entry)) { return(TRUE) }
      } else {
        if ("NA" %in% reviewed && is.na(entry)) { return(TRUE) }
      }
      return(FALSE)
    }

    phase_filter_function <- function(entry, phase) {
      if (is.null(phase)) { return(TRUE) }
      phases <- levels(factor(entry))
      return(any(phase %in% phases))
    }



    get_count_of_treatments <- function() {

      treatments <- trials_subset %>% 
                      separate_rows(corrected_treatment_name, sep = ", ") %>%
                      separate_rows(corrected_treatment_name, sep = " \\+ ") %>%
                      group_by(trial_id, corrected_treatment_name)
                      
      treatments <- summarise(treatments, count = n())
      treatments <- treatments$corrected_treatment_name

      treatment_count_df <- as.data.frame(table(treatments))
      treatment_count_df <- treatment_count_df[!(treatment_count_df$treatments == "Other" | treatment_count_df$treatments == "SOC"),]
      treatment_count_df_sorted <- treatment_count_df[order(-treatment_count_df$Freq),]
      treatment_count_df_sorted <- head(treatment_count_df_sorted, 10)
      return(treatment_count_df_sorted)
    }

    get_count_of_outcomes <- function() {

      outcomes <- trials_subset %>% 
                    separate_rows(outcome, sep = ", ") %>%
                    separate_rows(outcome, sep = " \\+ ") %>%
                    group_by(trial_id, outcome)

      outcomes <- summarise(outcomes, count = n())
      outcomes <- outcomes$outcome

      outcomes_count_df <- as.data.frame(table(outcomes))
      outcomes_count_df_sorted <- outcomes_count_df[order(-outcomes_count_df$Freq),]
      outcomes_count_df_sorted <- head(outcomes_count_df_sorted, 10)
      return(outcomes_count_df_sorted)
    }

    get_completed_trials <- function() {
      counts <- table(status = trials$recruitment_status)
      counts_dataframe <- as.data.frame(counts)
      completed <- subset(counts_dataframe, (status == 'Completed'), select = c(Freq))
      return(completed[1, 1])
    }

    treatment_count_df <- get_count_of_treatments()
    outcomes_count_df <- get_count_of_outcomes()
    completed_trials = get_completed_trials()

    shinyjs::addClass(id = "loading_screen", class = "hidden")

    # -------------------------------------------------------------------------------------------------------
    # Filter side panel
    # -------------------------------------------------------------------------------------------------------

    ## Pulled from UI because these depend on data being loaded (which is now removed from Global.r)
    output$input_selection_sidepanel <- renderUI({
      tagList(
        paste("Data last updated: ", date_data_transfer, sep = ""),
        hr(),
        tags$b('Generate Report:'),
        br(),
        shinySaveButton("generate_csv_report", "CSV", 'Select download location...', filetype = ("csv")),
        shinySaveButton("generate_pdf_report", "PDF", 'Select download location...', filetype = ("pdf")),
        img(id = "report_generating_gif", src = 'report_generating.gif', style = "width: 20px", class = "hidden"),
        hr(),
        checkboxGroupInput("flagged_trials", label = "Display trials with flag:", inline = FALSE, choices = list("Accepted" = TRUE, "Rejected" = FALSE, "Unreviewed" = "NA"), selected = c(TRUE, FALSE, "NA")),
        hr(),
        sliderInput("expected_enrollment", "Expected No. of Patients Size at Least:", min = 0, max = 4000, step = 100, value = 80),
        checkboxInput("enrollment_na_show", label = "Display trials without Expected No. of Patients", value = FALSE),
        hr(),
        selectInput("study_design", "Study Design:", choices = append(study_design_levels, "All", after = 0), selected = "All"),
        selectInput("trial_phase", "Trial Phase:", choices = phases, multiple = TRUE, selected = NULL),
        hr(),
        dateRangeInput("completion_date", "Completion Date Between:", start = today, end = today_plus_one_month, min = completion_date_min, max = completion_date_max),
        checkboxInput("completion_date_toggle", label = "Filter by Completion Date", value = FALSE),
        hr(),
        selectInput("treatment", "Treatment:", choices = treatments, multiple = TRUE, selected = NULL),
        radioButtons("treatment_andor", label = "Treatment Filter Logic", choices = c("AND", "OR"), selected = "AND", inline = TRUE),
        hr(),
        selectInput("outcome", "Outcome:", choices = outcomes, multiple = TRUE, selected = NULL),
        radioButtons("outcome_andor", label = "Outcome Filter Logic", choices = c("AND", "OR"), selected = "AND", inline = TRUE),
        hr()
      )
    })

    ## trails / trials_subset now need to be reactive to respond to data changing
    trials_reactive <- reactiveVal(trials)
    trials_subset_reactive <- reactiveVal(trials_subset)

    trials_subset_filtered <- reactive({
      if (is.null(input$expected_enrollment)) {
        # prevents filter error on first render but allows initial generation of datatable (which is isolated so only occurs once)
        trials_subset_reactive()
      } else {
        trials_subset_reactive() %>% filter((expected_enrollment >= input$expected_enrollment) | (input$enrollment_na_show & is.na(expected_enrollment)),
                                study_design_final %in% input$study_design | input$study_design == "All",
                                as.logical(lapply(phase, phase_filter_function, input$trial_phase)),
                                as.Date(date_primary_completion) >= input$completion_date[1] & as.Date(date_primary_completion) <= input$completion_date[2] | !input$completion_date_toggle,
                                as.logical(lapply(outcome, outcome_filter_function, input$outcome, input$outcome_andor)),
                                as.logical(lapply(corrected_treatment_name, treatment_filter_function, input$treatment, input$treatment_andor)),
                                as.logical(lapply(flag, review_filter_function, input$flagged_trials))
        )
      }
    })

    trials_filtered <- reactive({
      if (is.null(input$expected_enrollment)) {
        trials_reactive() # prevents filter error on first render but allows initial generation of datatable (which is isolated)
      } else {
        trials_reactive() %>% filter((expected_enrollment >= input$expected_enrollment) | (input$enrollment_na_show & is.na(expected_enrollment)),
                                study_design_final %in% input$study_design | input$study_design == "All",
                                as.logical(lapply(phase, phase_filter_function, input$trial_phase)),
                                as.Date(date_primary_completion) >= input$completion_date[1] & as.Date(date_primary_completion) <= input$completion_date[2] | !input$completion_date_toggle,
                                as.logical(lapply(outcome, outcome_filter_function, input$outcome, input$outcome_andor)),
                                as.logical(lapply(corrected_treatment_name, treatment_filter_function, input$treatment, input$treatment_andor)),
                                as.logical(lapply(flag, review_filter_function, input$flagged_trials))
        )
      }
    })

    # -------------------------------------------------------------------------------------------------------
    # Modal Dialog for reviewing trials
    # -------------------------------------------------------------------------------------------------------

    trialModal <- function(trial, fade) {
      log_message(paste0('Display modal dialog for trial: ', trial$trial_id, ' (', trial$flag, ')'))

      arms <- tagList()
      if (!is.null(trial$number_of_arms_final) && !is.na(trial$number_of_arms_final)) {
        for (n in 1:min(trial$number_of_arms_final, 7)) {
          arm_column <- paste0("tx", as.character(n), "_category")

          if (!is.null(trial[[arm_column]]) && !is.na(trial[[arm_column]])) {
            arms <- tagAppendChild(arms, column(2,
                      div(tags$b(paste0("Arm ", n))),
                      div(trial[[arm_column]])
              )
            )
          }
        }
      }

      modalDialog(
        title = {
        if (is.null(trial$flag) || is.na(trial$flag)) {
          fluidRow(column(width = 2, trial$trial_id))
        } else {
          if (trial$flag) {
            fluidRow(
                column(width = 2, trial$trial_id),
                column(width = 1, div(icon("check"), div("  ", style = "white-space: pre;"), style = "font-size: 200%; max-height: 1px; display: flex; align-items: center;")),
                column(width = 3, paste0("Accepted by: ", trial$user_submitted)),
                style = "display: flex; align-items: center;"
              )
          } else {
            fluidRow(
                column(width = 2, trial$trial_id),
                column(width = 1, div(icon("times"), div("  ", style = "white-space: pre;"), style = "font-size: 200%; max-height: 1px; display: flex; align-items: center;")),
                column(width = 3, paste0("Rejected by: ", trial$user_submitted)),
                style = "display: flex; align-items: center;"
              )
          }
        }
      }, #end title
        tabsetPanel(
          tabPanel("Details",
          br(),
          fluidPage(
            fluidRow(
              column(12,
                h4(trial$scientific_title)
              )
            ),
            tags$br(),
            fluidRow(
              column(4,
                div(tags$b("Institution")),
                div(trial$institution)
              ),
              column(8,
                    div(tags$b("URL")),
                    div(tags$a(href = trial$url, target = "_blank", trial$url))

              ),
            ),
            tags$br(),
            fluidRow(
              column(3,
                    div(tags$b("Date Registered")),
                    div(ifelse(is.null(format(trial$date_registered, "%d/%m/%Y")),
                                "N/A",
                                format(trial$date_registered, "%d/%m/%Y")
                    ))
              ),
              column(3,
                    div(tags$b("Date Updated")),
                    div(ifelse(is.null(format(trial$date_updated, "%d/%m/%Y")),
                                "N/A",
                                format(trial$date_updated, "%d/%m/%Y")
                    ))
              ),
              column(3,
                    div(tags$b("Trial Start Date")),
                    div(ifelse(is.null(format(trial$trial_start_date, "%d/%m/%Y")),
                                "N/A",
                                format(trial$trial_start_date, "%d/%m/%Y")
                    ))
              ),
              column(3,
                    div(tags$b("Primary Completion")),
                    div(ifelse(is.null(format(trial$date_primary_completion, "%d/%m/%Y")),
                                "N/A",
                                format(trial$date_primary_completion, "%d/%m/%Y"))
              ))
            ),
            tags$br(),
            fluidRow(
              column(3,
                    div(tags$b("Patient Setting")),
                    div(trial$patient_setting)
              ),
              column(3,
                    div(tags$b("Expected No. of Patients")),
                    div(trial$expected_enrollment)
              ),
              column(3,
                    div(tags$b("Recruitment Status")),
                    div(trial$recruitment_status)
              ),
              column(3,
                    div(tags$b("Patient Age")),
                    div(trial$age)
              )
            ),
            tags$br(),
            fluidRow(
              column(3,
                    div(tags$b("Therapy Target")),
                    div(trial$therapy_target)
              ),
              column(3,
                    div(tags$b("Covid-19 Status")),
                    div(trial$covid19_status)
              ),
              column(3,
                    div(tags$b("Study Design")),
                    div(trial$study_design_final)
              ),
              column(3,
                    div(tags$b("Blinding")),
                    div(trial$blinding_final)
              )
            ),
            tags$br(),
            fluidRow(
              column(3,
                    div(tags$b("Treatments")),
                    div(trial$corrected_treatment_name)
              ),
              column(3,
                    div(tags$b("Phase")),
                    div(trial$phase)
              ),

              column(6,
                    div(tags$b("Outcomes")),
                    div(trial$outcome)
              )
            ),
            tags$br(),
            fluidRow(
              div(tags$b("Trial Arms")),
              arms
            )
          )
        ),
        tabPanel("Review",
                fluidRow(
                  column(width = 2,
                          radioButtons("review_selection", "Accept or Reject?", choices = list("Accept" = TRUE, "Reject" = FALSE), selected = if (!is.null(trial$flag) && !is.na(trial$flag)) { trial$flag } else { FALSE }, inline = TRUE),
                          textInput("review_user_submitting", "User Submitting")
                  ),
                  column(width = 6,
                          textAreaInput("review_comments", "Comments", height = "200px", value = if (!is.null(trial$note) && !is.na(trial$note)) { trial$note } else { "" }) %>%
                            shiny::tagAppendAttributes(style = 'width: 100%;')
                  ),
                ),
                actionButton("review_submit", "Submit"),
                div(id = "user_blank_error", class = "hidden", style = "color: #ce0000;", "User is required for submisson.")
                )

        ),

        footer = fluidRow(
                actionButton("modal_prev", "Prev"),
                actionButton("modal_next", "Next"),
                modalButton("Dismiss")
        ),
        easyClose = TRUE,
        fade = fade
      )
    }

    # -------------------------------------------------------------------------------------------------------
    # Report generation
    # -------------------------------------------------------------------------------------------------------

    roots <- c(files = "./..")
    defaultRoot = "files"
    shinyFileSave(input, "generate_csv_report", roots = roots, defaultPath = "",
                  defaultRoot = defaultRoot, session = session)

    shinyFileSave(input, "generate_pdf_report", roots = roots, defaultPath = "",
                  defaultRoot = defaultRoot, session = session)

    # TODO - could these two handlers be templated?
    observeEvent(input$generate_csv_report, {
      if (!is.integer(input$generate_csv_report)) {
        # shinyFiles check if file has been selected
        roots <- c(files = "./..")
        savepath <- parseSavePath(roots, input$generate_csv_report)
        savepath <- savepath$datapath
        log_message(paste('generate_csv_report', savepath))

        shinyjs::removeClass(id = "report_generating_gif", class = "hidden")
        shinyjs::addClass(id = "generate_csv_report", class = "disabled")
        shinyjs::addClass(id = "generate_pdf_report", class = "disabled")
        
        # order_column <- input$trials_order$column
        # order_column_direction <- input$trials_order$direction
        order_column <- 4 # hard code by increasing completion date (ignore table sorting)
        order_column_direction <- "asc" # hard code by increasing completion date (ignore table sorting)
        
        if (order_column != "") {
          order_column_name <- names(trials_subset_filtered())[order_column]
          order_column <- trials_subset_filtered()[[order_column_name]]
          if (order_column_direction == "asc") {
            sorted_columns <- trials_subset_filtered()[order(order_column, decreasing = FALSE), ]
          } else if (order_column_direction == "desc") {
            sorted_columns <- trials_subset_filtered()[order(order_column, decreasing = TRUE), ]
          }
          
        } else {
          sorted_columns <- trials_subset_filtered()
        }
        
        rownames(sorted_columns) <- 1:nrow(sorted_columns) # required otherwise Rmd pdf creationg fails (not sure why?) here as well for redundancy
        trycatch_output <- tryCatch(
          expr = {
            generate_csv_report(input, sorted_columns, savepath)
            TRUE
          },
          warning = function(e) {
            log_message(toString(e))
            FALSE
          },
          error = function(e) {
            log_message(toString(e))
            FALSE
          }
        )
        
        if (trycatch_output) {
          shinyjs::addClass(id = "report_generating_gif", class = "hidden")
          shinyjs::removeClass(id = "generate_csv_report", class = "disabled")
          shinyjs::removeClass(id = "generate_pdf_report", class = "disabled")
          showNotification("CSV report complete", duration = 5, type = "message")
        } else {
          shinyjs::addClass(id = "report_generating_gif", class = "hidden")
          shinyjs::removeClass(id = "generate_csv_report", class = "disabled")
          shinyjs::removeClass(id = "generate_pdf_report", class = "disabled")
          showNotification(HTML("Looks like something went wrong. Please try again.<br>If the problem persists please contact Service Desk."), duration = NULL, type = "error")
        }
      }
    })

    observeEvent(input$generate_pdf_report, {
      if (!is.integer(input$generate_pdf_report)) {
        # shinyFiles check if file has been selected
        roots <- c(files = "./..")
        savepath <- parseSavePath(roots, input$generate_pdf_report)
        savepath <- savepath$datapath
        log_message(paste('generate_pdf_report', savepath))

        shinyjs::removeClass(id = "report_generating_gif", class = "hidden")
        shinyjs::addClass(id = "generate_csv_report", class = "disabled")
        shinyjs::addClass(id = "generate_pdf_report", class = "disabled")
        
        # order_column <- input$trials_order$column
        # order_column_direction <- input$trials_order$direction
        order_column <- 4 # hard code by increasing completion date (ignore table sorting)
        order_column_direction <- "asc" # hard code by increasing completion date (ignore table sorting)
        
        if (order_column != "") {
          order_column_name <- names(trials_subset_filtered())[order_column]
          order_column <- trials_subset_filtered()[[order_column_name]]
          if (order_column_direction == "asc") {
            sorted_columns <- trials_subset_filtered()[order(order_column, decreasing = FALSE), ]
          } else if (order_column_direction == "desc") {
            sorted_columns <- trials_subset_filtered()[order(order_column, decreasing = TRUE), ]
          }
          
        } else {
          sorted_columns <- trials_subset_filtered()
        }
        rownames(sorted_columns) <- 1:nrow(sorted_columns) # required otherwise Rmd pdf creationg fails (not sure why?)
        
        trycatch_output <- tryCatch(
          expr = {
            generate_pdf_report(input, sorted_columns, savepath)
            TRUE
          },
          warning = function(e) {
            log_message(toString(e))
            FALSE
          },
          error = function(e) {
            log_message(toString(e))
            FALSE
          }
        )
        
        print(trycatch_output)
        
        if (trycatch_output) {
          shinyjs::addClass(id = "report_generating_gif", class = "hidden")
          shinyjs::removeClass(id = "generate_csv_report", class = "disabled")
          shinyjs::removeClass(id = "generate_pdf_report", class = "disabled")
          showNotification("PDF report complete", duration = 5, type = "message")
        } else {
          shinyjs::addClass(id = "report_generating_gif", class = "hidden")
          shinyjs::removeClass(id = "generate_csv_report", class = "disabled")
          shinyjs::removeClass(id = "generate_pdf_report", class = "disabled")
          showNotification(HTML("Looks like something went wrong. Please try again.<br>If the problem persists please contact Service Desk."), duration = NULL, type = "error")
        }
      }
    })

    # -------------------------------------------------------------------------------------------------------
    # The main table displaying trial metadata
    # -------------------------------------------------------------------------------------------------------

    output$trials <- renderDataTable(
      isolate(trials_subset_filtered()), # Isolated this so that data table is updated via 'proxy' instead
      rownames = TRUE,
      colnames = c("Trial Id", "Title", "Institution", "Completion", "Size", "Patient setting", "Study design", "Arms", "Treatment", "Outcome", "Phase", "Flag"),
      plugins = "ellipsis",
      options = list(pageLength = 25,
                    dom = 'iftpl',
                    columnDefs = list(
                        list(
                        targets = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
                        render = JS("$.fn.dataTable.render.ellipsis( 15, false )")
                        ),
                        list(
                          targets = 12,
                          render = JS("function(data, type, row) {
                                        if (typeof(data) == 'undefined' || data == null) {return(\"\")};
                                        if (data) {return (\"Accepted\")};
                                        if (!data) {return (\"Rejected\")};
                                      }"
                          )
                        )
                    ),
                    rowCallback = JS("function( row, data, dataIndex ) {
                                          // console.log(data);
                                          if (typeof(data[12]) != 'undefined' && data[12] != null){
                                            if ( data[12] ) {
                                              $(row).addClass( 'accepted' );
                                            } else if ( !data[12] ) {
                                              $(row).addClass( 'rejected' );
                                            }
                                          }
                                        }"
                    ),
                    headerCallback = JS("function(thead, data, start, end, display){
                                          Shiny.setInputValue('trials_display_order', display)
                                        }"
                    )
      ),
      callback =  JS("table.on('order.dt', function () {
                    // This will show: 'Ordering on column 1 (asc)', for example
                    var order = table.order();
                    if (order.length > 0) {
                      //console.log('Ordering on column '+order[0][0]+' ('+order[0][1]+')' )
                      Shiny.setInputValue('trials_order', {column: order[0][0], direction: order[0][1]});
                    } else {
                      //console.log('Empty ordering')
                      Shiny.setInputValue('trials_order', {column: '', direction: ''});
                    }
                  })"
      ),
      selection = 'single',
      class = "display nowrap",
      server = TRUE # allows reloading of data.
    )

    # This proxy datatable allows updating data/selected items dynamically without rerendering the whole table again
    proxy <- dataTableProxy('trials') # creates a proxy of the datatable to allow manipulation (ie data editing) without regenerating the whole table
    observeEvent(trials_subset_filtered(), {
      replaceData(proxy, trials_subset_filtered(), resetPaging = FALSE, clearSelection = FALSE) # update the data when trials_subset_filtered() is edited
    })
    
    # observeEvent(input$trials_order, {
    #   print(input$trials_order)
    #   
    #   if (!(input$trials_order$column == "")) {
    #     order_column_name <- names(trials_subset_filtered())[input$trials_order$column]
    #     order_column <- trials_subset_filtered()[[order_column_name]]
    #     print(order_column_name)
    #     if (input$trials_order$direction == "asc") {
    #       sorted_columns <- trials_subset_filtered()[order(order_column, decreasing = FALSE), ]
    #     } else {
    #       sorted_columns <- trials_subset_filtered()[order(order_column, decreasing = TRUE), ]
    #     }
    #     
    #     print(sorted_columns$trial_id[1:10])
    #     print(nrow(sorted_columns))
    #     print(nrow(trials_subset_filtered()))
    # 
    #   }
    # })

    # shinyjs::runjs(
    #   "console.log(\"RUNNING JS... \");
    #   console.log($('#trials'));
    #   console.log($('#trials').dataTable);
    #   $('#trials').dataTable( {
    #     \"createdRow\": function( row, data, dataIndex ) {
    #       if ( data[10] == \"TRUE\" ) {
    #         $(row).addClass( 'accepted' );
    #       } else if ( data[10] == \"FALSE\" ) {
    #         $(row).addClass( 'rejected' );
    #       }
    #     }
    #   } )"
    # )
    
    shinyjs::runjs(
      "console.log(\"RUNNING JS... \");
      console.log($('#trials'));
      //console.log($('#trials').data().datatable.order())
      $('#trials').data().datatable.on('order.dt', function () {
        // This will show: 'Ordering on column 1 (asc)', for example
        var order = $('#trials').data().datatable.order();
        console.log('Ordering on column '+order[0][0]+' ('+order[0][1]+')' )
      })"
    )

    currentRow <- reactiveVal(NULL)
    modal_fade <- reactiveVal(TRUE)
    review_display_edge_case <- reactiveVal(FALSE)

    ignore_row_selected <- reactiveVal(FALSE) # toggle to allow ignoring modal generation on particular actions
    observeEvent(input$trials_rows_selected, {
      if (ignore_row_selected()) { ignore_row_selected(FALSE) }
      else {
        modal_fade(TRUE)
        if (input$trials_rows_selected) {
          if (!is.null(currentRow()) && currentRow() <= length(input$trials_rows_all) && input$trials_rows_all[[currentRow()]] == input$trials_rows_selected) {
            trial <- trials_filtered()[input$trials_rows_selected,]
            showModal(trialModal(trial, fade = modal_fade()))
            if (!is.null(trial$flag) && !is.na(trial$flag)) {
              if (trial$flag) {
                shinyjs::runjs('$("#shiny-modal")[0].children[0].children[0].children[0].classList.add("accepted")')
                shinyjs::runjs('$("#shiny-modal")[0].children[0].children[0].children[2].classList.add("accepted")')
              } else {
                shinyjs::runjs('$("#shiny-modal")[0].children[0].children[0].children[0].classList.add("rejected")')
                shinyjs::runjs('$("#shiny-modal")[0].children[0].children[0].children[2].classList.add("rejected")')
              }
            }
            modal_fade(FALSE)
          } else {
            currentRow(match(c(input$trials_rows_selected), input$trials_rows_all))
          }
        }
      }
    })


    observeEvent(currentRow(), {
      shinyjs::runjs("modal_scroll_y = $('#shiny-modal')[0].scrollTop")
      trial <- trials_filtered()[input$trials_rows_all[[currentRow()]],]
      showModal(trialModal(trial, fade = modal_fade()))
      shinyjs::runjs("$('#shiny-modal')[0].scrollTop = modal_scroll_y")
      modal_fade(FALSE)
      if (!is.null(trial$flag) && !is.na(trial$flag)) {
        if (trial$flag) {
          shinyjs::runjs('$("#shiny-modal")[0].children[0].children[0].children[0].classList.add("accepted")')
          shinyjs::runjs('$("#shiny-modal")[0].children[0].children[0].children[2].classList.add("accepted")')
        } else {
          shinyjs::runjs('$("#shiny-modal")[0].children[0].children[0].children[0].classList.add("rejected")')
          shinyjs::runjs('$("#shiny-modal")[0].children[0].children[0].children[2].classList.add("rejected")')
        }
      }
    })



    observe({
      shinyjs::toggleState("modal_prev", currentRow() > 1)
    })

    observe({
      shinyjs::toggleState("modal_next", (currentRow() < nrow(trials_filtered()) || (review_display_edge_case() && nrow(trials_filtered() != 0))))
    })

    observe({
      input$trials_rows_selected
      shinyjs::toggleState("modal_prev", currentRow() > 1)
    })

    observe({
      input$trials_rows_selected
      shinyjs::toggleState("modal_next", (currentRow() < nrow(trials_filtered()) || (review_display_edge_case() && nrow(trials_filtered() != 0))))
    })

    observeEvent(input$modal_prev, {
      # edge case doesn't appear when moving back
      currentRow(currentRow() - 1)
      review_display_edge_case(FALSE) # edge case doesn't persist after moving
      ignore_row_selected(TRUE) # prevents regeneration of modal when moving selected row
      selectRows(proxy, input$trials_rows_all[[currentRow()]]) # moves selected row on datatable
      # TODO move page as well with this
    })

    observeEvent(input$modal_next, {
      # fixing an edge case where currentRow disappears when
      # submitting a review which is not filtered to be displayed
      if (review_display_edge_case()) {
        temp_row <- currentRow()
        currentRow(NULL)
        currentRow(temp_row)
      } else {
        currentRow(currentRow() + 1)
      }
      review_display_edge_case(FALSE) # edge case doesn't persist after moving
      ignore_row_selected(TRUE) # prevents regeneration of modal when moving selected row
      selectRows(proxy, input$trials_rows_all[[currentRow()]]) # moves selected row on datatable
      # TODO move page as well with this
    })

    observeEvent(input$review_submit, {
      if (is.null(input$review_user_submitting) || input$review_user_submitting == "") {
        shinyjs::removeClass(id = "user_blank_error", class = "hidden")
      } else {
        shinyjs::addClass(id = "user_blank_error", class = "hidden")

        trial <- trials_filtered()[input$trials_rows_all[[currentRow()]],]

        ### TODO REQUIRE QUOTATION ESCAPE ###
        query <-  paste0("INSERT INTO trk_reviews (trial_id, flag, user_submitted, note) VALUES (",
                  paste0("'", gsub("'", "''", trial$trial_id, fixed = TRUE), "', "),
                  paste0("'", as.character(input$review_selection), "', "),
                  paste0("'", gsub("'", "''", input$review_user_submitting, fixed = TRUE), "', "),
                  paste0("'", gsub("'", "''", input$review_comments, fixed = TRUE), "'"),
                  ");"
        ) # End query
        # cat(file=stderr(), query)

        res <- RPostgres::dbSendQuery(con, query)
        log_message(paste0('Review added for trial: ', trial$trial_id))

        # edge case where newly create review should is not included in filter. upsets all of the row counting.
        if (!(input$review_selection %in% input$flagged_trials)) { 
          review_display_edge_case(TRUE) 
        }

        trials_new <- trials_reactive()
        trials_subset_new <- trials_subset_reactive()

        if (length(which(trials_new$trial_id == trial$trial_id)) > 0 && length(which(trials_subset_new$trial_id == trial$trial_id)) > 0) {

          row_value_trials <- which(trials_new$trial_id == trial$trial_id)[1]
          trials_new[row_value_trials, c("flag", "user_submitted", "note", "review_date_created")] <- c(as.logical(input$review_selection), input$review_user_submitting, input$review_comments, as.character(Sys.time()))
          trials_reactive(trials_new)

          row_value_trials_subset <- which(trials_subset_new$trial_id == trial$trial_id)[1]
          trials_subset_new[row_value_trials_subset, c("flag")] <- c(as.logical(input$review_selection), input$review_score)
          trials_subset_reactive(trials_subset_new)
        }

        if (input$review_selection) {
          shinyjs::runjs('$("#shiny-modal")[0].children[0].children[0].children[0].classList.remove("rejected")')
          shinyjs::runjs('$("#shiny-modal")[0].children[0].children[0].children[2].classList.remove("rejected")')
          shinyjs::runjs('$("#shiny-modal")[0].children[0].children[0].children[0].classList.add("accepted")')
          shinyjs::runjs('$("#shiny-modal")[0].children[0].children[0].children[2].classList.add("accepted")')
        } else {
          shinyjs::runjs('$("#shiny-modal")[0].children[0].children[0].children[0].classList.remove("accepted")')
          shinyjs::runjs('$("#shiny-modal")[0].children[0].children[0].children[2].classList.remove("accepted")')
          shinyjs::runjs('$("#shiny-modal")[0].children[0].children[0].children[0].classList.add("rejected")')
          shinyjs::runjs('$("#shiny-modal")[0].children[0].children[0].children[2].classList.add("rejected")')
        }

        if (input$review_selection) {
          shinyjs::runjs(gsub("[\r\n]", "", paste0('$("#shiny-modal")[0].children[0].children[0].children[0].innerHTML = \'',
            tags$h4(class = "modal-title",
              fluidRow(
                column(width = 2, trial$trial_id),
                column(width = 1, div(icon("check"), div("  ", style = "white-space: pre;"), style = "font-size: 200%; max-height: 1px; display: flex; align-items: center;")),
                column(width = 3, paste0("Accepted by: ", input$review_user_submitting)),
                style = "display: flex; align-items: center;"
              )
            ), '\'; //console.log($("#shiny-modal")[0]); console.log($("#shiny-modal")[0].children[0].children[0].children[0].innerHTML)'
          )))
        } else {
          shinyjs::runjs(gsub("[\r\n]", "", paste0('$("#shiny-modal")[0].children[0].children[0].children[0].innerHTML = \'',
            tags$h4(class = "modal-title",
              fluidRow(
                column(width = 2, trial$trial_id),
                column(width = 1, div(icon("times"), div("  ", style = "white-space: pre;"), style = "font-size: 200%; max-height: 1px; display: flex; align-items: center;")),
                column(width = 3, paste0("Rejected by: ", input$review_user_submitting)),
                style = "display: flex; align-items: center;"
              ),
            ), '\'; //console.log($("#shiny-modal")[0]); console.log($("#shiny-modal")[0].children[0].children[0].children[0].innerHTML)'
          )))
        }
      }
    })

    # -------------------------------------------------------------------------------------------------------
    # Summary page
    # -------------------------------------------------------------------------------------------------------

    output$noOfOutcomes <- renderPlotly({
      fig <- plot_ly(outcomes_count_df,
                      y = ~reorder(outcomes, Freq),
                      x = ~Freq,
                      orientation = 'h',
                      type = "bar",
                      marker = list(color = c('rgb(54, 153, 177)')),
                      hoverinfo = 'text',
                      text = ~paste('</br> No. of Trials: ', Freq)
        )

      fig <- fig %>% layout(title = "No. of Trials by Outcome (Top 10)",
                              xaxis = list(title = "No. of Trials"),
                              yaxis = list(title = "Outcomes"))
    })

    output$noOfTreatments <- renderPlotly({
      fig <- plot_ly(treatment_count_df,
                      y = ~reorder(treatments, Freq),
                      x = ~Freq,
                      orientation = 'h',
                      type = "bar",
                      marker = list(color = c('rgb(54, 153, 177)')),
                      hoverinfo = 'text',
                      text = ~paste('</br> No. of Trials: ', Freq)
        )

      fig <- fig %>% layout(title = "No. of Trials by Treatment (Top 10)",
                              xaxis = list(title = "No. of Trials"),
                              yaxis = list(title = "Treatments"))
    })


    output$noOfMonths <- renderPlotly({
      trials$no_of_months_until_readout <- (interval((Sys.Date()), (trials$date_primary_completion)) %/% months(1))
      trials_date <- trials %>% filter(date_primary_completion >= Sys.Date())
      no_of_months <- trials_date %>% filter(no_of_months_until_readout <= 11 & no_of_months_until_readout > -1)
      no_of_months$no_of_months_until_readout <- no_of_months$no_of_months_until_readout + 1
      counts <- table(readout_months = no_of_months$no_of_months_until_readout)
      counts_dataframe <- as.data.frame(counts)

      fig <- plot_ly(counts_dataframe,
          x = ~readout_months,
          y = ~Freq,
          type = "bar",
          marker = list(color = c('rgb(54, 153, 177)')),
          hoverinfo = 'text',
          text = ~paste('</br> No. of Trials: ', Freq)
        )

      fig <- fig %>% layout(title = "No. of Trials by Months until Completion",
                            xaxis = list(title = "Months"),
                            yaxis = list(title = "No. of Trials"))

      return(fig)
    })


    output$completedTrials <- renderText({
      paste("Number of Completed Trials: ", completed_trials)
    })

    ## PLOTLY CLICK TO FILTER ##
    ## Have to run intervals in javascript until the plotly plot has been rendered
    ## otherwise there is no data. Once plot has been rendered attaches an event
    ## that returns the closest data when clicked as a shiny input
    shinyjs::runjs(HTML(
        "var intv = setInterval(function(){
          var $el = $('#noOfMonths');
          if ( $el.length > 0 ) {
            if (typeof $el[0].on !== 'undefined') {
              clearInterval(intv);
              $el[0].on('plotly_click', function(data){
                  Shiny.setInputValue('noOfMonths_click', data.points[0].x);
                  //console.log(data.points[0].x);
              });
            }
          }
        }, 500);"
      ))

    observeEvent(input$noOfMonths_click, {
      updateCheckboxGroupInput(session, "flagged_trials", selected = c(TRUE, FALSE, "NA"))
      updateSliderInput(session, "expected_enrollment", value = 80)
      updateCheckboxInput(session, "enrollment_na_show", value = FALSE)
      updateSelectInput(session, "study_design", selected = "All")
      updateSelectInput(session, "trial_phase", selected = character(0))
      updateDateRangeInput(session, "completion_date", start = Sys.Date() %m+% months(as.numeric(input$noOfMonths_click) - 1), end = Sys.Date() %m+% months(as.numeric(input$noOfMonths_click)))
      updateCheckboxInput(session, "completion_date_toggle", value = TRUE)
      updateSelectInput(session, "treatment", selected = character(0))
      # updateRadioButtons(session,"treatment_andor", selected = "AND")
      updateSelectInput(session, "outcome", selected = character(0))
      # updateRadioButtons(session,"outcome_andor", label = "Outcome Filter Logic", choices = c("AND", "OR"), selected = "AND", inline = TRUE)

      updateNavbarPage(session, "navbar", "Trial Selection")
    })

    Sys.Date() %m+% months(1)

    shinyjs::runjs(HTML(
        "var intv = setInterval(function(){
          var $el = $('#noOfTreatments');
          if ( $el.length > 0 ) {
            if (typeof $el[0].on !== 'undefined') {
              clearInterval(intv);
              $el[0].on('plotly_click', function(data){
                  Shiny.setInputValue('noOfTreatments_click', data.points[0].y);
                  // console.log(data.points[0].y);
              });
            }
          }
        }, 500);"
      ))

    observeEvent(input$noOfTreatments_click, {
      updateCheckboxGroupInput(session, "flagged_trials", selected = c(TRUE, FALSE, "NA"))
      updateSliderInput(session, "expected_enrollment", value = 80)
      updateCheckboxInput(session, "enrollment_na_show", value = FALSE)
      updateSelectInput(session, "study_design", selected = "All")
      updateSelectInput(session, "trial_phase", selected = character(0))
      # updateDateRangeInput(session,"completion_date", start = today, end = today_plus_one_month, min = completion_date_min, max = completion_date_max)
      updateCheckboxInput(session, "completion_date_toggle", value = FALSE)
      updateSelectInput(session, "treatment", selected = input$noOfTreatments_click)
      # updateRadioButtons(session,"treatment_andor", selected = "AND")
      updateSelectInput(session, "outcome", selected = character(0))
      # updateRadioButtons(session,"outcome_andor", label = "Outcome Filter Logic", choices = c("AND", "OR"), selected = "AND", inline = TRUE)

      updateNavbarPage(session, "navbar", "Trial Selection")
    })

    shinyjs::runjs(HTML(
        "var intv = setInterval(function(){
          var $el = $('#noOfOutcomes');
          if ( $el.length > 0 ) {
            if (typeof $el[0].on !== 'undefined') {
              clearInterval(intv);
              $el[0].on('plotly_click', function(data){
                  Shiny.setInputValue('noOfOutcomes_click', data.points[0].y);
                  //console.log(data.points[0].y);
              });
            }
          }
        }, 500);"
      ))

    observeEvent(input$noOfOutcomes_click, {
      updateCheckboxGroupInput(session, "flagged_trials", selected = c(TRUE, FALSE, "NA"))
      updateSliderInput(session, "expected_enrollment", value = 80)
      updateCheckboxInput(session, "enrollment_na_show", value = FALSE)
      updateSelectInput(session, "study_design", selected = "All")
      updateSelectInput(session, "trial_phase", selected = character(0))
      # updateDateRangeInput(session,"completion_date", start = today, end = today_plus_one_month, min = completion_date_min, max = completion_date_max)
      updateCheckboxInput(session, "completion_date_toggle", value = FALSE)
      updateSelectInput(session, "treatment", selected = character(0))
      # updateRadioButtons(session,"treatment_andor", selected = "AND")
      updateSelectInput(session, "outcome", selected = input$noOfOutcomes_click)
      # updateRadioButtons(session,"outcome_andor", label = "Outcome Filter Logic", choices = c("AND", "OR"), selected = "AND", inline = TRUE)

      updateNavbarPage(session, "navbar", "Trial Selection")
    })
  } # End of connected clause
} # End of server function