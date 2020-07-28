server <- function(input, output, session) {
                             
  trials_subset_filtered <- reactive(
    trials_subset %>% filter(expected_enrollment >= input$expected_enrollment,
                             study_design_final %in% input$study_design,
                             as.Date(date_primary_completion) >= input$completion_date[1] & as.Date(date_primary_completion) <= input$completion_date[2]
                             )
  )
  
  trials_filtered <- reactive(
    trials %>% filter(expected_enrollment >= input$expected_enrollment,
                             study_design_final %in% input$study_design,
                             as.Date(date_primary_completion) >= input$completion_date[1] & as.Date(date_primary_completion) <= input$completion_date[2]
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
                 div(tags$a(href=trial$url, trial$url))
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
        )
      ),
      
      easyClose = TRUE
    )
    
  }
            
}