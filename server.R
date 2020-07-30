server <- function(input, output, session) {
                             
  trials_subset_filtered <- reactive(
    trials_subset %>% filter(expected_enrollment >= input$expected_enrollment,
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
    colnames = c("Trial Id", "Title", "Institution", "Completion", "Size", "Patient setting", "Study design", "Arms", "Treatment", "Outcome"),
    plugins = "ellipsis", 
    options = list(pageLength = 25,
                   columnDefs = list(list(
                     targets = c(1,2,3,4,5,6,7,8,9,10),
                     render = JS("$.fn.dataTable.render.ellipsis( 15, false )")
                   ))), 
    selection = 'single',
    class = "display nowrap"
  )
  
  observeEvent(input$trials_rows_selected, {
    if (input$trials_rows_selected) {
      trial <- trials_filtered()[input$trials_rows_selected,]
      # print(trial)
      showModal(trialModal(trial))
    }
  })
               
  
  trialModal <- function(trial) {
    # print(trial)
    arms <- tagList()
    if (!is.na(trial$number_of_arms_final)) {
      for (n in 1:min(trial$number_of_arms_final,7)) {
        arm_column <- paste0("tx", as.character(n), "_category")
        
        if (!is.na(trial[[arm_column]])) {
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
      
      
      easyClose = TRUE
    )
  }
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
      more_than_200_users <- trials %>% filter(expected_enrollment > 200 & expected_enrollment <= 15000)
      ggplotly(
      ggplot(data=more_than_200_users, aes(x=expected_enrollment)) + 
        geom_histogram(fill="#3699b1", breaks=seq(0, 15000, by=200)) +
        ggtitle("Distribution of No. Users Per Trial Limits: Between 200 & 15000") +
        ylab("No. of Trials") + xlab("Expected Enrollment Bins")
      )
    })
    
    
    output$noOfOutcomes <- renderPlotly({
      ggplotly(
      ggplot(data=outcomes_count_df, aes(x = reorder(outcome, count) , y=count)) +
        geom_bar(stat='identity', width = 0.5, fill="#3699b1" ) +
        coord_flip() +
        ggtitle("No. of Trials by Outcome (Top 20)") +
        ylab("No. of Trials") + xlab("Outcome"))
    })
    
    output$noOfTreatments <- renderPlotly({
      ggplotly(
      ggplot(data=treatment_count_df, aes(x = reorder(treatment, count) , y=count)) +
        geom_bar(stat='identity', width=0.5, fill="#3699b1") +
        coord_flip() + 
        ggtitle("No. of Trials by Treatment (Top 20)") +
        ylab("No. of Trials") + xlab("Treatment"))
    })
    
    output$noOfMonths <- renderPlotly({
      # Hold the 0 - 6 Months DataFrame
      trials$no_of_months_until_readout = elapsed_months(trials$date_primary_completion, Sys.Date())
      # trials %>% rowwise() %>% mutate(no_of_months_until_readout = elapsed_months(trials$date_primary_completion, Sys.Date()))
      # 
      # 
      # no_of_months_string <- trials %>% filter(is.character(no_of_months_until_readout) == TRUE)
      # no_of_months_int <- trials %>% filter(is.character(no_of_months_until_readout) == FALSE)
      # 
      no_of_months <- trials %>% filter(no_of_months_until_readout <= 12 & no_of_months_until_readout >= 1)
      # no_of_months <- rbind(no_of_months_int, no_of_months_string)
      
    
      counts <- table(readout_months = no_of_months$no_of_months_until_readout)
      counts_dataframe <- as.data.frame(counts)
      ggplotly(
      ggplot(data=counts_dataframe, aes(x = readout_months , y=Freq)) +
        geom_bar(stat='identity', width = 0.5, fill="#3699b1") +
        ggtitle("No. of Trials by Months until Completion") +
        ylab("No. of Trials") + xlab("Months until Completion"))
    })
    
    output$completedTrials <- renderText({
      paste("Number of Completed Trials: ", completed_trials)
    }
    )
    
    # output$trialSeverity <- renderPlot({
    #   counts <- table(readout_months = trials$patient_setting)
    #   counts_dataframe <- as.data.frame(counts)
    #   ggplot(data=counts_dataframe, aes(x = patient_setting , y=Freq)) +
    #     geom_bar(stat='identity', width = 2) +
    #     coord_flip() +
    #     ggtitle("No. of Trials by Patient Setting") +
    #     ylab("No. of Trials") + xlab("Months untill Readout")
    #   
    # })    

}