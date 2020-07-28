ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        #cohort_status {
          white-space: nowrap;
        }
		    #participant_selection, #cohort_status, #demographics {
			  white-space: nowrap;
		    }
		    .tab-content {
		    margin: 20px 0 0 0;
		    width: 100%;

		    }
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
          sliderInput("expected_enrollment", "Expected enrollment at least:", min = 0, max = 1000, step = 1, value = 200),
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
        )),
    tabPanel("Summary",
      textOutput("summaryOutput"))
  )
)