# The User interface
#
# 2 Tabs:
# - Trial Selection: for the trial table and filter controls,
# - Summary: high level summary charts of the trials in the database
#
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  navbarPage("Trial Tracker Dashboard", id = "navbar", position = "static-top",
    tabPanel("Trial Selection",
      sidebarPanel(
        useShinyjs(),
        width = 2,
        uiOutput("input_selection_sidepanel")
      ),
      mainPanel(
        div(id = "loading_screen", style = "display: inline-block",
              h4(style = "text-align: center; position: relative; top: 235px; font-size: 200%", "Loading trial data..."),
              img(src = 'loading_gif.gif')
            ),
# div(class = "dataTables_info", id = "DataTables_Table_0_info",),
        dataTableOutput("trials")
      )
    ),
    tabPanel("Summary",
      fluidRow(
        column(6, textOutput("completedTrials")),
        column(6,)
      ),
      hr(),
      fluidRow(
        column(12, plotlyOutput("noOfMonths"))
      ),
      hr(),
      fluidRow(
        column(6, plotlyOutput("noOfOutcomes")),
        column(6, plotlyOutput("noOfTreatments"))
      )
    )
  )
)