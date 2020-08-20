ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
		    .modal-dialog{ width: 80%;}
		    .modal-title{font-weight: 900;}
		    .modal-header.accepted{background-color: #7bd881;}
		    .modal-header.rejected{background-color: #f58484;}
		    .modal-footer.accepted{background-color: #7bd881;}
		    .modal-footer.rejected{background-color: #f58484;}
		    
		    
		    .svg-container {
		    margin-top:40px !important;
		    }
		    .modebar-container {
		    top:-30px !important;
		    }
		    table.dataTable.display tbody tr.odd.accepted {
		      background-color: #abfbb0;
		    }
		    table.dataTable.display tbody tr.odd.rejected {
		      background-color: #fbd3d3;
		    }
		    table.dataTable.display tbody tr.even.accepted {
		      background-color: #c2fdc5;
		    }
		    table.dataTable.display tbody tr.even.rejected {
		      background-color: #ffe0e0;
		    }
      ")
    )
  ),
  theme = shinytheme("spacelab"),
  navbarPage("COVID-19 TRIAL DASHBOARD",
    tabPanel("Trial Selection",
      sidebarPanel(
        useShinyjs(),
        width=3,
        paste("Data last updated: ", date_data_transfer, sep=""),
        hr(),
        sliderInput("expected_enrollment", "Expected enrollment size at least:", min = 0, max = 4000, step = 100, value = 200),
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
      ),
      mainPanel(
        dataTableOutput("trials")
      )
    ),
    tabPanel("Summary",
             fluidRow(
               column(6, textOutput("completedTrials")),
               column(6, )),
             hr(),
             fluidRow(
               column(6, plotlyOutput("noOfMonths")),
               column(6, plotlyOutput("noOfUsers"))),
             hr(),
             fluidRow(
               column(6, plotlyOutput("noOfOutcomes")),
               column(6, plotlyOutput("noOfTreatments")))
    )
  )
)