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
        uiOutput("input_selection_sidepanel"),
        
      ),
      mainPanel(
        div(id="loading_screen", style="display: inline-block",
              h4(style="text-align: center; position: relative; top: 235px; font-size: 200%", "Loading trial data..."),
              img(src='loading_gif.gif')
            ),
        dataTableOutput("trials")
      )
    ),
    tabPanel("Summary",
             fluidRow(
               column(6, textOutput("completedTrials")),
               column(6, )),
             hr(),
             fluidRow(
               column(12, plotlyOutput("noOfMonths"))),
             hr(),
             fluidRow(
               column(6, plotlyOutput("noOfOutcomes")),
               column(6, plotlyOutput("noOfTreatments")))
    )
  )
)