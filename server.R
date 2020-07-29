server <- function(input, output, session) {
  
  elapsed_months <- function(end_date, start_date) {
    ed <- as.POSIXlt(end_date)
    sd <- as.POSIXlt(start_date)
    12 * (ed$year - sd$year) + (ed$mon - sd$mon)
  }
  
  
  trials_subset_filtered <- reactive(
    trials_subset %>% filter(expected_enrollment >= input$expected_enrollment,
                             study_design %in% input$study_design,
                             as.Date(completion_date) >= input$completion_date[1] & as.Date(completion_date) <= input$completion_date[2]#,
#                           #str_detect(treatment, input$treatment),
                             #str_detect(outcome, input$outcome)
                             )
  )                           

  output$trials <- renderDataTable(
    trials_subset_filtered(), rownames=TRUE, options = list(pageLength = 25), selection = 'single'
  )
  
  observeEvent(input$trials_rows_selected,
               {
                 showModal(modalDialog(
                   title = "Details about trial",
                   trials[trials$id == trials_subset_filtered()[input$trials_rows_selected, 'id'],]
                 ))
               })
  
    output$noOfUsers <- renderPlot({
      more_than_200_users <- trials %>% filter(expected_enrollment > 200)
      print(max(more_than_200_users$expected_enrollment))
      ggplot(data=more_than_200_users, aes(expected_enrollment)) + 
        geom_histogram(bins=12)

    })
    
  
    output$noOfOutcomes <- renderPlot({
      counts <- table(Outcome = trials$outcome)
      counts_dataframe <- as.data.frame(counts)
      
      ggplot(data=counts_dataframe, aes(x = Outcome , y=Freq)) +
        geom_bar(stat='identity', width = 2) +
        coord_flip()
            
    })
    
    output$noOfMonths <- renderPlot({
     trials$no_of_months_untill_readout = elapsed_months(trials$date_primary_completion, Sys.Date())
     counts <- table(readout_months = trials$no_of_months_untill_readout)
     counts_dataframe <- as.data.frame(counts)
     ggplot(data=counts_dataframe, aes(x = readout_months , y=Freq)) +
       geom_bar(stat='identity', width = 2) +
       coord_flip() +
       ggtitle("No. of Trials by Months untill Readout") +
       ylab("No. of Trials") + xlab("Months untill Readout")
    })
    
    output$trialSeverity <- renderPlot({
      counts <- table(readout_months = trials$patient_setting)
      counts_dataframe <- as.data.frame(counts)
      ggplot(data=counts_dataframe, aes(x = patient_setting , y=Freq)) +
        geom_bar(stat='identity', width = 2) +
        coord_flip() +
        ggtitle("No. of Trials by Patient Setting") +
        ylab("No. of Trials") + xlab("Months untill Readout")
    })
      
            
}