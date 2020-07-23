#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# This will get replaced with XAP.read_table() function


load("./data/dat_processed_and_network.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
    titlePanel("Covid Trial Tracker"),
    
    tabsetPanel(
        type = "tabs",
        tabPanel("Summary",
                 # Summary Tab code goes in here.
                 mainPanel(textOutput(
                     "summary_main_output"
                 ))),
        tabPanel("Clinical Trials",
                 # Clinical Trials Tab Code goes in here.
                 sidebarLayout(
                     sidebarPanel(textOutput("trial_sidebar_output")),
                     
                     # Show a plot of the generated distribution
                     mainPanel(tableOutput("clinical_trial_data"))
                 ))
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$summary_main_output <-
        renderText("This is the main section for the summary tab.")
    output$trial_sidebar_output <-
        renderText("This is the side bar.")
    output$clinical_trial_data <- renderTable(head(dat))
}

# Run the application
shinyApp(ui = ui, server = server)
