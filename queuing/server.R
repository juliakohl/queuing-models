library(shiny)
library(queueing)
library(plotly)

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
  
  output$dist <- renderPlot({
    lambda = input$no_arrivals
    mu = input$no_served
    input_mm1 <- NewInput.MM1(lambda = lambda, mu = mu, n = 0)
    curve(dpois(x, input_mm1$lambda),
          from = 0, 
          to = 20, 
          type = "b", 
          lwd = 2,
          xlab = "Number of customers",
          ylab = "Probability",
          main = "Poisson Distribution for Arrival Process",
          ylim = c(0, 0.25),
          n = 21)
  })
})
