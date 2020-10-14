library('shiny')

# User Interface/UI

ui <- fluidPage(

  titlePanel(
    'Slider and Text input update'
  ), # titlePanel

  mainPanel(

    # Slider input
    sliderInput(
      inputId = 'sliderValue',
      label = 'Slider value',
      min = 0,
      max = 1000,
      value = 500
    ), # sliderInput

    # Text input
    textInput(
      inputId = 'textValue',
      value = 500,
      label = NULL
    ) # textInput

  ) # mainPanel

) # fluidPage


# Server logic

server <- function(input, output, session)
{
  observeEvent(input$textValue,{
    if(as.numeric(input$textValue) != input$sliderValue)
    {
      updateSliderInput(
        session = session,
        inputId = 'sliderValue',
        value = input$textValue
      ) # updateSliderInput
    }#if


  })

  observeEvent(input$sliderValue,{
    if(as.numeric(input$textValue) != input$sliderValue)
    {
      updateTextInput(
        session = session,
        inputId = 'textValue',
        value = input$sliderValue
      ) # updateTextInput

    }#if

  })


}

# Run the application
shinyApp(ui = ui, server = server)
