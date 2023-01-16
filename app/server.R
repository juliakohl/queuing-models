library(shiny)
library(queueing)
library(plotly)
library(kableExtra)
library(readxl)
library(dplyr)
library(fitdistrplus)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  el <- reactive({
    lambda <- input$lambda #sum(m) / input$totalunits
    mu <- input$no_served #sum(m)
    
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
    #output_m
    return(cbind(round(summary(output_m)$el,2),Throughput=round(output_m$Throughput,2)))
  })
  
  costs <- reactive({
    wcpu <- input$waitingcost
    ar <- input$lambda
    ar_r <- seq(ar/10,ar+ar/10*9,ar/10)
    scps <- input$servicecost
    c <- input$n_servers
    #if(c == 1){}
    costs <- c()
    
    for(n in 1:length(L_r)){
      lambda <- ar_r[n]
      mu <- input$no_served 
      
      if(lambda >= mu){
        costs <- c(costs, tmp)
        next
      }
      
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
      L <- output_m$L
      
      
      tmp <- wcpu * L + scps * c
      costs <- c(costs, tmp)
    }
    
    return(data.frame(Lambda = ar_r, Costs = costs))
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
    
    #updateNumericInput(session = session, 'no_served', value = lambda + 1, min = ceiling(lambda))
    updateNumericInput(session = session, 'lambda', value = lambda, min = ceiling (lambda))
    
    
    output$inputdist <- renderPlotly({
      
      # input data
      fig <- plot_ly(x=x$n, y=x$count, type = "bar", name = 'Input data') %>% 
        layout(xaxis = list(title = 'Number of arrivals per unit time'),
               yaxis = list(title = 'Count'))
      
      # poisson
      tmp <- data.frame(y = dpois(seq(0,nrow(x)-1),lambda)*420, x = seq(0,nrow(x)-1))
      fig <- fig %>% add_trace(tmp, x =tmp$x, y=tmp$y, type = 'scatter', mode='lines', name = 'Poisson')
      
      # normal dist
      #tmp <- data.frame(y = dnorm(seq(0,nrow(x)-1),lambda)*420, x = seq(0,nrow(x)-1))
      #fig <- fig %>% add_trace(tmp, x =tmp$x, y=tmp$y, type = 'scatter', mode='lines', name = 'Normal')
      
      #gamma
      #fit <- fitdist(data[[input$column]], distr = "gamma", method = "mle")
      #tmp <- data.frame(y = dgamma(seq(0,nrow(x)-1),fit$estimate[1],fit$estimate[2]) * 420, x = seq(0,nrow(x)-1))
      #fig <- fig %>% add_trace(x = tmp$x, y = tmp$y, type = 'scatter', mode='lines', name = 'Gamma')
      
      fig
    })
    
  })
  
  observeEvent(input$simulate, {
    
    arrivals <<- round(rpois(30,input$lambda))
    queue <<- rep(0,30)
    overflow <<- rep(0,30)
    n <- 1
    
    for (a in arrivals) {
      if(n>length(arrivals)){ next }
      if(n>1){
        queue[n] <<- ifelse(queue[n-1] <= input$no_served,0,queue[n-1] - input$no_served)
        queue[n] <<- queue[n] + a
          #queue[n-1] + a - input$no_served
          #queue[n] <<- ifelse(queue[n] < 0, 0, queue[n])
      }else{
        queue[n] <<- a
      }
      
      # if(input$capacity > 0){
      #   if(queue[n] > input$capacity){
      #     overflow[n] <<- queue[n] - input$capacity
      #     queue[n] <<- queue[n] - overflow[n]
      #   }
      # }
      
      n <- n+1
    }
    
    # output$dist <- renderPlotly({
    #   fig <- plot_ly(x = seq(1,30), y = arrivals, type = 'bar', name = 'arrivals', marker = list(color='cadetblue')) %>%
    #     add_trace(y=queue, name='queue', marker = list(color='green'))
    #   if(input$capacity > 0){
    #     fig <- fig %>% add_trace(y=overflow, name='Capacity overflow', marker = list(color='red'))
    #   }
    #   fig
    # })
    
    
    output$report <- renderText({
      #x <<- data %>% group_by_at(input$column) %>% summarise(n = n()) %>% group_by(n) %>% summarise(count = n())
      #x <<- rbind(x, c(0, input$totalunits - sum(x$count)))
      
      #m <<- x$n * x$count
      #lambda <<- sum(m) / input$totalunits
      #if(input$capacity == 0){
        if(input$n_servers == 1){
          input_m <- NewInput.MM1(lambda = input$lambda, mu = input$no_served, n = 0)
        }else{
          if(input$n_servers > 1){
            c = input$n_servers
            input_m <- NewInput.MMC(lambda=input$lambda, mu=input$no_served, c=c, n=0, method=0)
          }
        }
      # }else{
      #   if(input$n_servers == 1){
      #     input_m <- NewInput.MM1K(lambda = input$lambda, mu = input$no_served, k = input$capacity)
      #   }else{
      #     if(input$n_servers > 1){
      #       c = input$n_servers
      #       input_m <- NewInput.MMCK(lambda=input$lambda, mu=input$no_served, c=c, k = input$capacity)
      #     }
      #   }
      # }

      
      # Create queue class object
      output_m <- QueueingModel(input_m)
      for(i in 2:15){
        output_m[i][[1]] <- round(output_m[i][[1]],2)
      }
      
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
    
    tmp <- data.frame(`Avg. arrival rate (lambda)` = evaluation$lambda, 
                      `Service rate (mu)` = evaluation$mu, 
                      `Number of servers (c)` = evaluation$c,
                      `Prob. of 0 in queue (P0)` = evaluation$P0,
                      Throughput = evaluation$Throughput,
                      #`Prob. of k in queue (Pk)` = evaluation$Pk,
                      `Avg. queue length (Lq)` = evaluation$Lq,
                      `Avg. waiting time of an arrival (Wq)` = evaluation$Wq,
                      check.names = FALSE)
    
    #Probability of zero unit in the queue (Po)
    #average queue length (Lq )
    #Average number of units in the system (Ls)
    #Average waiting time of an arrival (Wq) 
    #Average waiting time of an arrival in the system (Ws) 
    
    output$eval <- function () {
      evaluation %>% 
      #tmp %>% 
        knitr::kable("html") %>%
        kable_styling("striped", full_width = T)
    }
    
    output$costplot <- renderPlotly({
      df <- costs()
      x <- seq(1,19,1)
      y1 <- costs()
      #df <- data.frame(servicelevel = x, costs = y1)
      #y2 <- 
      
        plot_ly(df, x=~Lambda, y=~Costs, type = 'scatter', mode = 'lines', name = 'Arrival Rate')  %>% 
          #add_trace(y=~y1+1, type = 'scatter', mode = 'lines', name = 'Cost of waiting time') %>% 
          layout(yaxis = list(title= 'Costs'))
      })
  })
  

  
})

# # input data
# fig <- plot_ly(x=x$n, y=x$count, type = "bar", name = 'Input data') %>% 
#   layout(xaxis = list(title = 'Number of arrivals per unit time'),
#          yaxis = list(title = 'Count'))
# 
# # poisson
# tmp <- data.frame(y = dpois(seq(0,nrow(x)-1),lambda)*420, x = seq(0,nrow(x)-1))
# fig <- fig %>% add_trace(tmp, x =tmp$x, y=tmp$y, type = 'scatter', mode='lines', name = 'Poisson')
