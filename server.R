server <- function(input, output, session) {
  trials_reactive <- reactiveVal(trials)
  trials_subset_reactive <- reactiveVal(trials_subset)
  
                             
  trials_subset_filtered <- reactive(
    trials_subset_reactive() %>% filter((expected_enrollment >= input$expected_enrollment) | (input$enrollment_na_show & is.na(expected_enrollment)),
                             study_design_final %in% input$study_design | input$study_design == "All",
                             as.Date(date_primary_completion) >= input$completion_date[1] & as.Date(date_primary_completion) <= input$completion_date[2] | !input$completion_date_toggle,
                             as.logical(lapply(outcome, outcome_filter_function, input$outcome, input$outcome_andor)),
                             as.logical(lapply(corrected_treatment_name, treatment_filter_function, input$treatment, input$treatment_andor))
                             )
  )
  
  trials_filtered <- reactive(
    trials_reactive() %>% filter(expected_enrollment >= input$expected_enrollment,
                             study_design_final %in% input$study_design | input$study_design == "All",
                             as.Date(date_primary_completion) >= input$completion_date[1] & as.Date(date_primary_completion) <= input$completion_date[2] | !input$completion_date_toggle,
                             as.logical(lapply(outcome, outcome_filter_function, input$outcome, input$outcome_andor)),
                             as.logical(lapply(corrected_treatment_name, treatment_filter_function, input$treatment, input$treatment_andor))
    )
  )

  output$trials <- renderDataTable(
    trials_subset_filtered(), 
    rownames=TRUE,
    colnames = c("Trial Id", "Title", "Institution", "Completion", "Size", "Patient setting", "Study design", "Arms", "Treatment", "Outcome", "Flag", "Rating"),
    plugins = "ellipsis", 
    options = list(pageLength = 25,
                   columnDefs = list(list(
                     targets = c(1,2,3,4,5,6,7,8,9,10),
                     render = JS("$.fn.dataTable.render.ellipsis( 15, false )")
                   ))), 
    selection = 'single',
    class = "display nowrap",
    server = TRUE # allows reloading of data.
  )
  
  proxy <- dataTableProxy('trials') # creates a proxy of the datatable to allow manipulation (ie data editing) without regenerating the whole table
  observeEvent( trials_subset_filtered(), {
    replaceData(proxy, trials_subset_filtered()) # update the data when trials_subset_filtered() is edited
  })
  
  currentRow <- reactiveVal(NULL)
  modal_fade <- reactiveVal(TRUE)
  
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
                   div(tags$b("Date Registerd")),
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
                   div(tags$b("Expected Enrollment")),
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
    shinyjs::toggleState("modal_prev", currentRow() != 1)
  })
  
  observe({
    shinyjs::toggleState("modal_next", currentRow() != nrow(trials_filtered()))
  })
  
  observe({
    input$trials_rows_selected
    shinyjs::toggleState("modal_prev", currentRow() != 1)
  })
  
  observe({
    input$trials_rows_selected
    shinyjs::toggleState("modal_next", currentRow() != nrow(trials_filtered()))
  })
  
  observeEvent(input$modal_prev,{
    currentRow( currentRow() - 1 )
    ignore_row_selected(TRUE) # prevents regeneration of modal when moving selected row
    selectRows(proxy, input$trials_rows_all[[currentRow()]]) # moves selected row on datatable
    # TODO move page as well with this
  })
  
  observeEvent(input$modal_next,{
    currentRow( currentRow() + 1 )
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
      
      trials_new <- trials_reactive() 
      trials_new[trials_new$trial_id == trial$trial_id,][c("flag", "rating", "user_submitted", "note", "review_date_created")] <- data.frame(c(input$review_selection),c(input$review_score),c(input$review_user_submitting),c(input$review_comments), c(Sys.time()))
      trials_reactive(trials_new)
      
      trials_subset_new <- trials_subset_reactive() 
      trials_subset_new[trials_subset_new$trial_id == trial$trial_id,][c("flag", "rating")] <- data.frame(c(input$review_selection),c(input$review_score))
      trials_subset_reactive(trials_subset_new)
      
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
                     y = ~reorder(outcome, count),
                     x = ~count,
                     orientation = 'h',
                     type = "bar",
                     marker=list(color=c('rgb(54, 153, 177)')),
                     hoverinfo = 'text',
                     text = ~paste('</br> No. of Trials: ', count)
      )
      
      fig <- fig %>% layout(title = "No. of Trials by Outcome (Top 20)",
                            xaxis = list(title = "Outcome"),
                            yaxis = list(title = "No. of Trials"))
    })
    
    output$noOfTreatments <- renderPlotly({
      fig <- plot_ly(treatment_count_df,
                     y = ~reorder(treatment, count),
                     x = ~count,
                     orientation = 'h',
                     type = "bar",
                     marker=list(color=c('rgb(54, 153, 177)')),
                     hoverinfo = 'text',
                     text = ~paste('</br> No. of Trials: ', count)
      )
      
      fig <- fig %>% layout(title = "No. of Trials by Treatment (Top 20)",
                            xaxis = list(title = "Treatment"),
                            yaxis = list(title = "No. of Trials"))
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