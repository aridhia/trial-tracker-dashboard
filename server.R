server <- function(input, output, session) {
  
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
            
}