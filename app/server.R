library(shiny)
library(queueing)
library(plotly)
library(kableExtra)
library(readxl)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  #inputs <- reactive({c(input$arrivals_put, input$no_served)})
  
  el <- reactive({
    lambda = input$no_arrivals
    mu = input$no_served
    if(input$system == "M/M/1"){
      input_m <- NewInput.MM1(lambda = lambda, mu = mu, n = 0)
    }else{
      if(input$system == "M/M/c"){
        c = input$n_servers
        input_m <- NewInput.MMC(lambda=lambda, mu=mu, c=c, n=0, method=0)
      }
    }
    
    # Create queue class object
    output_m <- QueueingModel(input_m)
    
    return(summary(output_m)$el)
  })
  
  
  data_raw <- reactive({
    inFile <- input$arrivals
    
    if (is.null(inFile))
      return(NULL)
    
    return(read_excel(inFile$datapath,
               sheet = "Sheet1"))
  })
  
  evaluation <- data.frame()
  data <- NULL
  
  observeEvent(input$read, {
    inFile <- input$arrivals
    
    if (is.null(inFile))
      return(NULL)
    
    data <<- read_excel(inFile$datapath,
               sheet = "Sheet1")
    
    output$inputdist <- renderPlotly({

      x <- data %>% group_by_at(input$column) %>% summarise(n = n()) %>% group_by(n) %>% summarise(count = n())
      x <- rbind(x, c(0, input$totalunits - sum(x$count)))
      
      # input data
      fig <- plot_ly(x=x$n, y=x$count, type = "bar", name = 'Input data')
      
      m <- x$n * x$count
      lambda <- sum(m) / input$totalunits
      cat(lambda)
      
      # poisson
      tmp <- data.frame(y = dpois(seq(0,nrow(x)-1),lambda)*420, x = seq(0,nrow(x)-1))
      fig <- fig %>% add_trace(tmp, x =tmp$x, y=tmp$y, type = 'scatter', mode='lines', name = 'Poisson')
      
      # normal dist
      tmp <- data.frame(y = dnorm(seq(0,nrow(x)-1),lambda)*420, x = seq(0,nrow(x)-1))
      fig <- fig %>% add_trace(tmp, x =tmp$x, y=tmp$y, type = 'scatter', mode='lines', name = 'Normal')
      
      
      fig
    })
    
    output$dist <- renderPlotly({
      # lambda = input$no_arrivals
      # mu = input$no_served
      # 
      # data <- data.frame(y = dpois(seq(0,2*lambda),lambda), x = seq(0,2*lambda))
      # fig <- plot_ly(data, x = ~x, y = ~y, type = 'scatter', mode = 'lines')
      # 
      # fig
      
      if(is.null(data)){
        return(NULL)
      }
      
      x <- data %>% group_by_at(input$column) %>% summarise(n = n()) %>% group_by(n) %>% summarise(count = n())
      x <- rbind(x, c(0, input$totalunits - sum(x$count)))
      
      m <- x$n * x$count
      lambda <- sum(m) / input$totalunits
      
      tmp <- data.frame(y = dpois(seq(0,2*lambda),lambda), x = seq(0,2*lambda))
      fig <- plot_ly(tmp, x = ~x, y = ~y, type = 'scatter', mode = 'lines')
      
      fig
    })  
  })
  
  observeEvent(input$save, {
    if(nrow(evaluation)==0){
      evaluation <<- el()
    }else{
      evaluation <<- rbind(evaluation, el())
    }
    
    output$eval <- function () {evaluation %>% 
      knitr::kable("html") %>%
      kable_styling("striped", full_width = T)
    }
  })
  
  observeEvent(input$no_served, {
    updateSliderInput(session, "no_arrivals", max = input$no_served -1)
  })
  
  output$report <- renderText({
    lambda = input$no_arrivals
    mu = input$no_served
    if(input$system == "M/M/1"){
      input_m <- NewInput.MM1(lambda = lambda, mu = mu, n = 0)
    }else{
      if(input$system == "M/M/c"){
        c = input$n_servers
        input_m <- NewInput.MMC(lambda=lambda, mu=mu, c=c, n=0, method=0)
      }
    }
    
    # Create queue class object
    output_m <- QueueingModel(input_m)
    
    #View(summary(output_m)$el)
    
    # Get queue model report
    y <- capture.output(Report(output_m))
    return(y)
  })
  
  # output$dist <- renderPlot({
  #   lambda = input$no_arrivals
  #   mu = input$no_served
  #   
  #   #View(data.frame(y = dpois(seq(0,2*lambda),lambda), x = seq(0,2*lambda)))
  #   
  #   if(input$system == "M/M/1"){
  #     input_m <- NewInput.MM1(lambda = lambda, mu = mu, n = 0)
  #     curve(dpois(x, lambda),
  #           from = 0, 
  #           to = 2*lambda, 
  #           type = "b", 
  #           lwd = 2,
  #           xlab = "Number of customers",
  #           ylab = "Probability",
  #           main = "Poisson Distribution for Arrival Process",
  #           ylim = c(0, dpois(lambda, lambda)+0.1),
  #           n = input_m$lambda*2+1)
  #   }else{
  #     if(input$system == "M/M/c"){
  #       c = input$n_servers
  #       input_m <- NewInput.MMC(lambda=lambda, mu=mu, c=c, n=0, method=0)
  #       #mit plotly seq(10) und dpois(seq(10), lambda)
  #       curve(dpois(x, input_m$lambda),
  #             from = 0, 
  #             to = 2*lambda, 
  #             type = "b", 
  #             lwd = 2,
  #             xlab = "Number of customers",
  #             ylab = "Probability",
  #             main = "Poisson Distribution for Arrival Process",
  #             ylim = c(0, dpois(lambda, lambda)+0.1),
  #             n = lambda*2+1)
  #     }
  #   }
  # })
  
  
  
})
