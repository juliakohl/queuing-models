library(shiny)
library(queueing)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  inputs <- reactive({c(input$arrivals_put, input$no_served)})
  
  output$report <- renderText({
    lambda = input$no_arrivals
    mu = input$no_served
    input_mm1 <- NewInput.MM1(lambda = lambda, mu = mu, n = 0)
    
    # Create queue class object
    output_mm1 <- QueueingModel(input_mm1)
    
    # Get queue model report
    y <- capture.output(Report(output_mm1))
    return(y)

    # Get queue model summary
    #summary(output_mm1)
  })
})
