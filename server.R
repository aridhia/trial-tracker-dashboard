options(connectionObserver = NULL)


server <- function(input, output, session) {
  
  # fix for mini_app greying-out after 10 min of inactivity
  autoInvalidate <- reactiveTimer(30000)
  observe({
    autoInvalidate()
    cat(".")
  })
  
  ###############################################################################################
  ##################################    MOVED FROM GLOBAL.R    ##################################
  ###############################################################################################
  
  date_data_transfer <- "2020-07-29"
  con                <- ''
  
  # Set Variables for Enviorment
  if(exists("xap.conn")){
    con <- xap.conn
  } else {
    con <- dbConnect(RPostgres::Postgres(), dbname=Sys.getenv("PGDATABASE"), host=Sys.getenv("PGHOST"), user=Sys.getenv("PGUSER"), password=Sys.getenv("PGPASSWORD"))
  }
  
  trials_original = RPostgres::dbGetQuery(con, "SELECT * FROM combined_view;")
  
  trials <- trials_original %>%
    select(-c(state_name, state_lon, state_lat, country_name, iso3_code)) 
  # %>% unique() # unique not working anymore with new columns (?)
  trials <- trials[!duplicated(trials$trial_id), ]
  
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
           flag,
           rating)
  
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
  
  review_filter_function <- function(entry, reviewed) {
    if (is.null(reviewed)) {return(FALSE)}
    if (!is.na(entry)) {
      if ("TRUE" %in% reviewed && as.logical(entry)) {return(TRUE)}
      if ("FALSE" %in% reviewed && !as.logical(entry)) {return(TRUE)}
    }else{
      if ("NA" %in% reviewed && is.na(entry)) {return(TRUE)}
    }
    return(FALSE)
  }
  shinyjs::addClass(id="loading_screen", class="hidden")
  
  
  get_count_of_treatments <- function(){
    
    treatments <- trials_subset$corrected_treatment_name %>% 
      strsplit(", ") %>% reduce(c) %>% 
      strsplit(" + ", fixed = TRUE) %>% reduce(c) %>%
      trimws() %>% sort()
    treatment_count_df <- as.data.frame(table(treatments))
    treatment_count_df <- treatment_count_df[!(treatment_count_df$treatments == "Other" | treatment_count_df$treatments == "SOC"),]
    treatment_count_df_sorted <- treatment_count_df[order(-treatment_count_df$Freq),]
    treatment_count_df_sorted <- head(treatment_count_df_sorted, 10)
    return(treatment_count_df_sorted)
  }
  
  get_count_of_outcomes <- function(){

    outcomes <- trials_subset$outcome %>% 
      strsplit(", ") %>% reduce(c) %>% 
      strsplit(" + ", fixed = TRUE) %>% reduce(c) %>%
      trimws() %>% sort()
    outcomes_count_df <- as.data.frame(table(outcomes))
    outcomes_count_df_sorted <- outcomes_count_df[order(-outcomes_count_df$Freq),]
    outcomes_count_df_sorted <- head(outcomes_count_df_sorted, 10)
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
  
  ###############################################################################################
  ###############################################################################################
  ###############################################################################################
  
  
  
  ## Pulled from UI because these depend on data being loaded (which is now removed from Global.r)
  output$input_selection_sidepanel <- renderUI({
    tagList(
      paste("Data last updated: ", date_data_transfer, sep=""),
      hr(),
      checkboxGroupInput("flagged_trails", label="Display trials with flag:", inline=TRUE, choices=list("Accepted"= TRUE, "Rejected"=FALSE, "Unreviewed"="NA"), selected = c(TRUE, FALSE, "NA")),
      hr(),
      sliderInput("expected_enrollment", "Expected enrollment size at least:", min = 0, max = 4000, step = 100, value=80),
      checkboxInput("enrollment_na_show", label="Display trials without expected enrollment", value = FALSE),
      hr(),
      selectInput("study_design", "Study design:", choices = append(study_design_levels, "All", after=0), selected = "All"),
      hr(),
      dateRangeInput("completion_date", "Completion date between:", start = today, end = today_plus_one_month, min = completion_date_min, max = completion_date_max),
      checkboxInput("completion_date_toggle", label="Filter by Completion Date", value = FALSE),
      hr(),
      selectInput("treatment", "Treatment:", choices = treatments, multiple = TRUE, selected = NULL),
      radioButtons("treatment_andor", label = "Treatment Filter Logic", choices = c("AND", "OR"), selected = "AND", inline = TRUE),
      hr(),
      selectInput("outcome", "Outcome:", choices = outcomes, multiple = TRUE, selected = NULL),
      radioButtons("outcome_andor", label = "Outcome Filter Logic", choices = c("AND", "OR"), selected = "AND", inline = TRUE)
    )
  })
  
  ## trails / trials_subset now need to be reactive to respond to data changing
  trials_reactive <- reactiveVal(trials)
  trials_subset_reactive <- reactiveVal(trials_subset)
  
                             
  trials_subset_filtered <- reactive({
    if (is.null(input$expected_enrollment)) {
      trials_subset_reactive() # prevents filter error on first render but allows initial generation of datatable (which is isolated so only occurs once)
    } else {
      trials_subset_reactive() %>% filter((expected_enrollment >= input$expected_enrollment) | (input$enrollment_na_show & is.na(expected_enrollment)),
                               study_design_final %in% input$study_design | input$study_design == "All",
                               as.Date(date_primary_completion) >= input$completion_date[1] & as.Date(date_primary_completion) <= input$completion_date[2] | !input$completion_date_toggle,
                               as.logical(lapply(outcome, outcome_filter_function, input$outcome, input$outcome_andor)),
                               as.logical(lapply(corrected_treatment_name, treatment_filter_function, input$treatment, input$treatment_andor)),
                               as.logical(lapply(flag, review_filter_function, input$flagged_trails))
      )
    }
  })
  
  trials_filtered <- reactive({
    if (is.null(input$expected_enrollment)) {
      trials_reactive() # prevents filter error on first render but allows initial generation of datatable (which is isolated)
    } else {
      trials_reactive() %>% filter(expected_enrollment >= input$expected_enrollment,
                               study_design_final %in% input$study_design | input$study_design == "All",
                               as.Date(date_primary_completion) >= input$completion_date[1] & as.Date(date_primary_completion) <= input$completion_date[2] | !input$completion_date_toggle,
                               as.logical(lapply(outcome, outcome_filter_function, input$outcome, input$outcome_andor)),
                               as.logical(lapply(corrected_treatment_name, treatment_filter_function, input$treatment, input$treatment_andor)),
                               as.logical(lapply(flag, review_filter_function, input$flagged_trails))
      )
    }
  })

  output$trials <- renderDataTable(
    isolate(trials_subset_filtered()), # Isolated this so that data table is updated via 'proxy' instead
    rownames=TRUE,
    colnames = c("Trial Id", "Title", "Institution", "Completion", "Size", "Patient setting", "Study design", "Arms", "Treatment", "Outcome", "Flag", "Rating"),
    plugins = "ellipsis", 
    options = list(pageLength = 25,
                   columnDefs = list(
                      list(
                       targets = c(1,2,3,4,5,6,7,8,9,10),
                       render = JS("$.fn.dataTable.render.ellipsis( 15, false )")
                      ),
                      list(
                        targets = 11,
                        render = JS("function(data, type, row) {
                                      if (typeof(data) == 'undefined' || data == null) {return(\"\")};
                                      if (data) {return (\"Accepted\")};
                                      if (!data) {return (\"Rejected\")};
                                     }"
                        )
                      )
                   ),
                   rowCallback = JS("function( row, data, dataIndex ) {
                                        console.log(data);
                                        if (typeof(data[11]) != 'undefined' && data[11] != null){
                                          if ( data[11] ) {
                                            $(row).addClass( 'accepted' );
                                          } else if ( !data[11] ) {
                                            $(row).addClass( 'rejected' );
                                          }
                                        }
                                      }"
                   )
                   ), 
    selection = 'single',
    class = "display nowrap",
    server = TRUE # allows reloading of data.
  )
  
  # This proxy datatable allows updating data/selected items dynamically without rerendering the whole table again
  proxy <- dataTableProxy('trials') # creates a proxy of the datatable to allow manipulation (ie data editing) without regenerating the whole table
  observeEvent( trials_subset_filtered(), {
    replaceData(proxy, trials_subset_filtered(), resetPaging = FALSE, clearSelection = FALSE) # update the data when trials_subset_filtered() is edited
  })
  
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
  
  currentRow <- reactiveVal(NULL)
  modal_fade <- reactiveVal(TRUE)
  review_display_edge_case <- reactiveVal(FALSE)
  
  ignore_row_selected <- reactiveVal(FALSE) # toggle to allow ignoring modal generation on particular actions
  observeEvent(input$trials_rows_selected, {
    if (ignore_row_selected()) {ignore_row_selected(FALSE)} 
    else {
      modal_fade(TRUE)
      if (input$trials_rows_selected) {
        if (!is.null(currentRow()) && currentRow() <= length(input$trials_rows_all) && input$trials_rows_all[[currentRow()]] == input$trials_rows_selected) {
          trial <- trials_filtered()[input$trials_rows_selected,]
          showModal(trialModal(trial, fade=modal_fade()))
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
          currentRow( match(c(input$trials_rows_selected), input$trials_rows_all) )
        }
      }
    }
  })
  
  
  observeEvent(currentRow(), {
    shinyjs::runjs("modal_scroll_y = $('#shiny-modal')[0].scrollTop")
    trial <- trials_filtered()[input$trials_rows_all[[currentRow()]],]
    # print(trial)
    showModal(trialModal(trial, fade=modal_fade()))
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
               
  trialModal <- function(trial, fade) {
    # print(trial)
    arms <- tagList()
    if (!is.null(trial$number_of_arms_final) && !is.na(trial$number_of_arms_final)) {
      for (n in 1:min(trial$number_of_arms_final,7)) {
        arm_column <- paste0("tx", as.character(n), "_category")
        
        if (!is.null(trial[[arm_column]]) && !is.na(trial[[arm_column]])) {
          arms <- tagAppendChild(arms, column(2, 
                                         div(tags$b(paste0("Arm ", n))),
                                         div(trial[[arm_column]])
          ))
        }
      }
    }
    
    
    
    modalDialog(
      title = {
        if (is.null(trial$flag) || is.na(trial$flag)) {
          fluidRow(column(width=2,trial$trial_id))
        } else {
          if (trial$flag) {
            fluidRow(
              column(width=2, trial$trial_id),
              column(width=1, div(icon("check"), div("  ", style="white-space: pre;"), trial$rating, style="font-size: 200%; max-height: 1px; display: flex; align-items: center;")),
              column(width=3, paste0("Accepted by: ", trial$user_submitted )),
              style="display: flex; align-items: center;"
            )
          } else {
            fluidRow(
              column(width=2, trial$trial_id),
              column(width=1, div(icon("times"), div("  ", style="white-space: pre;"), trial$rating, style="font-size: 200%; max-height: 1px; display: flex; align-items: center;")),
              column(width=3, paste0("Rejected by: ", trial$user_submitted )),
              style="display: flex; align-items: center;"
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
                   div(tags$a(href=trial$url,target= "_blank", trial$url))
  
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
                 column(width=2,
                        radioButtons("review_selection", "Accept or Reject?", choices=list("Accept"= TRUE, "Reject"=FALSE), selected=if (!is.null(trial$flag) && !is.na(trial$flag)) {trial$flag} else {FALSE}, inline=TRUE),
                        textInput("review_user_submitting", "User Submitting")
                 ),
                 column(width=4,
                        sliderInput("review_score", "Review Score", min=0, max=100, step=1, value=if (!is.null(trial$rating) && !is.na(trial$rating)) {trial$rating} else {50} )
                 ),
                 column(width=6,
                        textAreaInput("review_comments", "Comments", height="200px", value=if (!is.null(trial$note) && !is.na(trial$note)) {trial$note} else {""}) %>%
                          shiny::tagAppendAttributes(style = 'width: 100%;')
                 ),
               ),
               actionButton("review_submit", "Submit"),
               div(id="user_blank_error", class="hidden", style="color: #ce0000;", "User is required for submisson.")
               )
      
      ),
      
      footer = fluidRow(
               actionButton("modal_prev","Prev"),
               actionButton("modal_next","Next"),
               modalButton("Dismiss")
      ),
      easyClose = TRUE,
      fade = fade
    )
  }
  
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
  
  observeEvent(input$modal_prev,{
    # edge case doesn't appear when moving back
    currentRow( currentRow() - 1 )
    review_display_edge_case(FALSE) # edge case doesn't persist after moving
    ignore_row_selected(TRUE) # prevents regeneration of modal when moving selected row
    selectRows(proxy, input$trials_rows_all[[currentRow()]]) # moves selected row on datatable
    # TODO move page as well with this
  })
  
  observeEvent(input$modal_next,{
    # fixing an edge case where currentRow disappears when
    # submitting a review which is not filtered to be displayed
    if (review_display_edge_case()) {
      temp_row <- currentRow()
      currentRow( NULL )
      currentRow( temp_row )
    } else {
      currentRow( currentRow() + 1 )
    }
    review_display_edge_case(FALSE) # edge case doesn't persist after moving
    ignore_row_selected(TRUE) # prevents regeneration of modal when moving selected row
    selectRows(proxy, input$trials_rows_all[[currentRow()]]) # moves selected row on datatable
    # TODO move page as well with this
  })
  
  observeEvent(input$review_submit, {
    if (is.null(input$review_user_submitting) || input$review_user_submitting == "") {
      shinyjs::removeClass(id="user_blank_error", class="hidden")
    } else {
      shinyjs::addClass(id="user_blank_error", class="hidden")
      
      trial <- trials_filtered()[input$trials_rows_all[[currentRow()]],]
      
      ### TODO REQUIRE QUOTATION ESCAPE ###
      query <- paste0("INSERT INTO trk_reviews (trial_id, flag, rating, user_submitted, note) VALUES (",
              paste0("'", gsub("'", "''", trial$trial_id, fixed=TRUE), "', "),
              paste0("'", as.character(input$review_selection), "', "),
              paste0(as.character(input$review_score), ", "),
              paste0("'", gsub("'", "''", input$review_user_submitting, fixed=TRUE), "', "),
              paste0("'", gsub("'", "''", input$review_comments, fixed=TRUE), "'"),
            ");"
      )
      print(query)
      res <- RPostgres::dbSendQuery(con,query)
      print(res)
      
      # edge case where newly create review should is not included in filter. upsets all of the row counting.
      if (!(input$review_selection %in% input$flagged_trails)) {review_display_edge_case(TRUE)}
      
      trials_new <- trials_reactive()
      trials_subset_new <- trials_subset_reactive() 
      
      if (length(which(trials_new$trial_id == trial$trial_id)) > 0 && length(which(trials_subset_new$trial_id == trial$trial_id)) > 0) {
        
        row_value_trials <- which(trials_new$trial_id == trial$trial_id)[1]
        trials_new[row_value_trials, c("flag", "rating", "user_submitted", "note", "review_date_created")] <- c(as.logical(input$review_selection), input$review_score, input$review_user_submitting, input$review_comments, as.character(Sys.time()))
        trials_reactive(trials_new)
        
        row_value_trials_subset <- which(trials_subset_new$trial_id == trial$trial_id)[1]
        trials_subset_new[row_value_trials_subset, c("flag", "rating")] <- c(as.logical(input$review_selection), input$review_score)
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
          tags$h4(class="modal-title",
            fluidRow(
              column(width=2, trial$trial_id),
              column(width=1, div(icon("check"), div("  ", style="white-space: pre;"), input$review_score, style="font-size: 200%; max-height: 1px; display: flex; align-items: center;")),
              column(width=3, paste0("Accepted by: ", input$review_user_submitting )),
              style="display: flex; align-items: center;"
            )
          ), '\';console.log($("#shiny-modal")[0]); console.log($("#shiny-modal")[0].children[0].children[0].children[0].innerHTML)'
        )))
      } else {
        shinyjs::runjs(gsub("[\r\n]", "", paste0('$("#shiny-modal")[0].children[0].children[0].children[0].innerHTML = \'',
          tags$h4(class="modal-title",
            fluidRow(
              column(width=2, trial$trial_id),
              column(width=1, div(icon("times"), div("  ", style="white-space: pre;"), input$review_score, style="font-size: 200%; max-height: 1px; display: flex; align-items: center;")),
              column(width=3, paste0("Rejected by: ", input$review_user_submitting )),
              style="display: flex; align-items: center;"
            ),
          ), '\';console.log($("#shiny-modal")[0]); console.log($("#shiny-modal")[0].children[0].children[0].children[0].innerHTML)'
        )))
      }
        
      
    }
    
  })
  
  
  
  #############################################################
  #### SUMMARY PAGE
  #############################################################
  
    elapsed_months <- function(end_date, start_date) {
      final_months <- 0
      ed <- as.POSIXlt(end_date)
      sd <- as.POSIXlt(start_date)
      final_months <- 12 * (ed$year - sd$year) + (ed$mon - sd$mon)
      # if (final_months >= 6 && final_months <= 12) {
      #   final_months <- "7-12"
      # }
      return(final_months)
    }
    
    output$noOfUsers <- renderPlotly({
      options(scipen=10000)
      more_than_200_users <- trials %>% filter(expected_enrollment >= 200 & expected_enrollment <= 15000)
      fig <- plot_ly(more_than_200_users,
                     x = ~expected_enrollment,
                     type = "histogram",
                     marker=list(color=c('rgb(54, 153, 177)'))
      )
      
      fig <- fig %>% layout(title = "Distribution of No. Users Per Trial Limits: Between 200 & 15000",
                            xaxis = list(title = "Outcome"),
                            yaxis = list(title = "Expected Enrollment Bins"))
    })
    
    
    
    output$noOfOutcomes <- renderPlotly({
      fig <- plot_ly(outcomes_count_df,
                     y = ~reorder(outcomes, Freq),
                     x = ~Freq,
                     orientation = 'h',
                     type = "bar",
                     marker=list(color=c('rgb(54, 153, 177)')),
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
                     marker=list(color=c('rgb(54, 153, 177)')),
                     hoverinfo = 'text',
                     text = ~paste('</br> No. of Trials: ', Freq)
      )
      
      fig <- fig %>% layout(title = "No. of Trials by Treatment (Top 10)",
                            xaxis = list(title = "No. of Trials"),
                            yaxis = list(title = "Treatments"))
    })
    
  
    output$noOfMonths <- renderPlotly({
      trials$no_of_months_until_readout = elapsed_months(trials$date_primary_completion, Sys.Date())
      no_of_months <- trials %>% filter(no_of_months_until_readout <= 12 & no_of_months_until_readout >= 1)
      counts <- table(readout_months = no_of_months$no_of_months_until_readout)
      counts_dataframe <- as.data.frame(counts)

      fig <- plot_ly(counts_dataframe,
        x = ~readout_months,
        y = ~Freq,
        type = "bar",
        marker=list(color=c('rgb(54, 153, 177)')),
        hoverinfo = 'text',
        text = ~paste('</br> No. of Trials: ', Freq)
      )
      
      fig <- fig %>% layout(title = "No. of Trials by Months until Completion",
                           xaxis = list(title = "Months"),
                           yaxis = list(title = "No. of Trials"))
    })
    
    
    output$completedTrials <- renderText({
      paste("Number of Completed Trials: ", completed_trials)
    }
    )

}