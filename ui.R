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
    tabPanel("Trials",
      sidebarPanel(
        useShinyjs(),
        width=3,
        paste("Data last updated: ", date_data_transfer, sep=""),
        hr(),
        sliderInput("expected_enrollment", "Expected enrollment size at least:", min = 0, max = 1000, step = 1, value = 200),
        hr(),
        selectInput("study_design", "Study design:", choices = study_design_levels, multiple = TRUE, selected = "Randomised"),
        hr(),
        dateRangeInput("completion_date", "Completion date between:", start = today, end = today_plus_one_month, min = completion_date_min, max = completion_date_max),
        hr(),
        selectInput("treatment", "Treatment:", choices = treatments, multiple = TRUE, selected = NULL),
        hr(),
        selectInput("outcome", "Outcome:", choices = outcomes, multiple = TRUE, selected = NULL)
      ),
      mainPanel(
        dataTableOutput("trials")
      )
    )
  )
)