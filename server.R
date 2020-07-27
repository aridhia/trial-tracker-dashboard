server <- function(input, output, session) {
  
  trials_subset_filtered <- reactive(
    trials_subset %>% filter(expected_enrollment >= input$expected_enrollment,
                             study_design_final %in% input$study_design,
                             as.Date(date_primary_completion) >= input$completion_date[1] & as.Date(date_primary_completion) <= input$completion_date[2]
                             )
  )                           

  output$trials <- renderDataTable(
    trials_subset_filtered(), 
    rownames=TRUE,
    colnames = c("Id", "Trial Id", "Title", "Institution", "Completion", "Size", "Patient setting", "Study design", "Arms", "URL", "Treatment", "Outcome"),
    plugins = "ellipsis", 
    options = list(pageLength = 25,
                   columnDefs = list(list(
                     targets = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     render = JS("$.fn.dataTable.render.ellipsis( 15, false )")
                   ))), 
    selection = 'single',
    class = "display nowrap"
  )
  
  observeEvent(input$trials_rows_selected,
               {
                 showModal(modalDialog(
                   title = "Details about trial",
                   trials[trials$id == trials_subset_filtered()[input$trials_rows_selected, 'id'],]
                 ))
               })
            
}