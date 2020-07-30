ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
		    .modal-dialog{ width: 80%},
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