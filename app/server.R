library(shiny)
library(queueing)
library(plotly)
library(kableExtra)
library(readxl)
library(dplyr)
library(fitdistrplus)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  #inputs <- reactive({c(input$arrivals_put, input$no_served)})
  
  el <- reactive({
    inFile <- input$arrivals
    
    if (is.null(inFile))
      return(NULL)
    
    data <- read_excel(inFile$datapath,
                        sheet = "Sheet1")
    arrivals <- c()
    
    x <- data %>% group_by_at(input$column) %>% summarise(n = n()) %>% group_by(n) %>% summarise(count = n())
    x <- rbind(x, c(0, input$totalunits - sum(x$count)))
    
    mu <- x$n * x$count
    lambda <- sum(m) / input$totalunits
    mu <- input$no_served #sum(m)
    if(input$n_servers == 1){
      input_m <- NewInput.MM1(lambda = lambda, mu = mu, n = 0)
    }else{
      if(input$n_servers > 1){
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
    arrivals <<- c()
    
    x <<- data %>% group_by_at(input$column) %>% summarise(n = n()) %>% group_by(n) %>% summarise(count = n())
    x <<- rbind(x, c(0, input$totalunits - sum(x$count)))
    
    m <<- x$n * x$count
    lambda <<- sum(m) / input$totalunits
    
    updateSliderInput(session = session, 'no_served', value = lambda + 1, min = ceiling (lambda))
    
    
    output$inputdist <- renderPlotly({
      
      # input data
      fig <- plot_ly(x=x$n, y=x$count, type = "bar", name = 'Input data')
      
      # poisson
      tmp <- data.frame(y = dpois(seq(0,nrow(x)-1),lambda)*420, x = seq(0,nrow(x)-1))
      fig <- fig %>% add_trace(tmp, x =tmp$x, y=tmp$y, type = 'scatter', mode='lines', name = 'Poisson')
      
      # normal dist
      tmp <- data.frame(y = dnorm(seq(0,nrow(x)-1),lambda)*420, x = seq(0,nrow(x)-1))
      fig <- fig %>% add_trace(tmp, x =tmp$x, y=tmp$y, type = 'scatter', mode='lines', name = 'Normal')
      
      #gamma
      #fit <- fitdist(data[[input$column]], distr = "gamma", method = "mle")
      #tmp <- data.frame(y = dgamma(seq(0,nrow(x)-1),fit$estimate[1],fit$estimate[2]) * 420, x = seq(0,nrow(x)-1))
      #fig <- fig %>% add_trace(x = tmp$x, y = tmp$y, type = 'scatter', mode='lines', name = 'Gamma')
      
      fig
    })
    
  })
  
  observeEvent(input$simulate, {
    
    # if(input$arrival_bhvr == 'Poisson'){
    #   arrivals <<- round(rpois(50,lambda))
    # }else{ 
    #   if(input$arrival_bhvr == 'Normal'){
    #     arrivals <<- round(rnorm(50,lambda))
    #     arrivals[arrivals < 0] <- 0
    #   }else{
    #     if(input$arrival_bhvr == 'Constant'){
    #       arrivals <<- rep(round(lambda),50)
    #     }
    #   }
    # }
    
    arrivals <<- round(rpois(50,lambda))
    queue <<- rep(0,50)
    n <- 2
    for (a in arrivals) {
      if(n>length(arrivals)){ next }
      if(n>2){
        queue[n] <<- queue[n-1] + a - input$no_served
        queue[n] <<- ifelse(queue[n] < 0, 0, queue[n])
      }else{
        queue[n] <<- ifelse(a>input$no_served,a-input$no_served,0)
      }
      n <- n+1
    }
    
    output$dist <- renderPlotly({
      plot_ly(x = seq(1,50), y = arrivals, type = 'bar', name = 'arrivals') %>% add_trace(y=queue, name='queue')
    })
    
    
    output$report <- renderText({
      x <<- data %>% group_by_at(input$column) %>% summarise(n = n()) %>% group_by(n) %>% summarise(count = n())
      x <<- rbind(x, c(0, input$totalunits - sum(x$count)))
      
      m <<- x$n * x$count
      lambda <<- sum(m) / input$totalunits
      if(input$n_servers == 1){
        input_m <- NewInput.MM1(lambda = lambda, mu = input$no_served, n = 0)
      }else{
        if(input$n_servers > 1){
          c = input$n_servers
          input_m <- NewInput.MMC(lambda=lambda, mu=input$no_served, c=c, n=0, method=0)
        }
      }
      
      # Create queue class object
      output_m <- QueueingModel(input_m)
      
      #View(summary(output_m)$el)
      
      # Get queue model report
      y <- capture.output(Report(output_m))
      return(y)
    })
    
    
  })
  
  observeEvent(input$save, {
    if(nrow(evaluation)==0){
      evaluation <<- el()
    }else{
      evaluation <<- rbind(evaluation, el())
    }
    
    tmp <- data.frame(`Average arrival rate (lambda)` = evaluation$lambda, `Service rate (mu)` = evaluation$mu)
    
    #Probability of zero unit in the queue (Po)
    #average queue length (Lq )
    #Average number of units in the system (Ls)
    #Average waiting time of an arrival (Wq) 
    #Average waiting time of an arrival in the system (Ws) 
    
    output$eval <- function () {
      #evaluation %>% 
      tmp %>% 
        knitr::kable("html") %>%
        kable_styling("striped", full_width = T)
    }
  })
  
  
  
})
