server <- function(input, output, session) {
                             
  trials_subset_filtered <- reactive(
    trials_subset %>% filter((expected_enrollment >= input$expected_enrollment) | (input$enrollment_na_show & is.na(expected_enrollment)),
                             study_design_final %in% input$study_design | input$study_design == "All",
                             as.Date(date_primary_completion) >= input$completion_date[1] & as.Date(date_primary_completion) <= input$completion_date[2] | !input$completion_date_toggle,
                             as.logical(lapply(outcome, outcome_filter_function, input$outcome, input$outcome_andor)),
                             as.logical(lapply(corrected_treatment_name, treatment_filter_function, input$treatment, input$treatment_andor))
                             )
  )
  
  trials_filtered <- reactive(
    trials %>% filter(expected_enrollment >= input$expected_enrollment,
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
    class = "display nowrap"
  )
  
  currentRow <- reactiveVal(NULL)
  modal_fade <- reactiveVal(TRUE)
  
  observeEvent(input$trials_rows_selected, {
    modal_fade(TRUE)
    if (input$trials_rows_selected) {
      if (!is.null(currentRow()) && currentRow() <= length(input$trials_rows_all) && input$trials_rows_all[[currentRow()]] == input$trials_rows_selected) {
        trial <- trials_filtered()[input$trials_rows_selected,]
        showModal(trialModal(trial, fade=modal_fade()))
        modal_fade(FALSE)
      } else {
        currentRow( match(c(input$trials_rows_selected), input$trials_rows_all) )
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
      title = trial$trial_id,
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
  })
  
  observeEvent(input$modal_next,{
    currentRow( currentRow() + 1 )
  })
  
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