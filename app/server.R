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
  
  costs_lambda <- reactive({
    wcpu <- input$waitingcost
    ar <- input$lambda
    ar_r <- seq(ar/10,ar+ar/10*9,ar/10)
    scps <- input$servicecost
    c <- input$n_servers
    #if(c == 1){}
    costs <- c()
    rev <- c()
    profit <- c()
    
    for(n in 1:length(ar_r)){
      lambda <- ar_r[n]
      mu <- input$no_served 
      
      if(lambda >= mu){
        costs <- c(costs, NA)
        rev <- c(rev, NA)
        profit <- c(profit, NA)
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
      Rev <- output_m$Throughput * input$revenue
      tmp <- wcpu * L + scps * c
      p <- Rev - tmp
      
      costs <- c(costs, tmp)
      rev <- c(rev, Rev)
      profit <- c(profit, p)
    }
    
    return(data.frame(Lambda = ar_r, Costs = costs, Revenue = rev, Profit = profit))
  })
  
  costs_mu <- reactive({
    wcpu <- input$waitingcost
    lambda <- input$lambda
    mu <- input$no_served
    mu_r <- seq(mu/10,mu+mu/10*9,mu/10)
    scps <- input$servicecost
    c <- input$n_servers
    costs <- c()
    rev <- c()
    profit <- c()
    
    for(n in 1:length(mu_r)){
      m <- mu_r[n]
      
      if(lambda >= m){
        costs <- c(costs, NA)
        rev <- c(rev, NA)
        profit <- c(profit, NA)
        next
      }
      
      if(c == 1){
        input_m <- NewInput.MM1(lambda = lambda, mu = m, n = 0)
      }else{
        if(c > 1){
          input_m <- NewInput.MMC(lambda=lambda, mu=m, c=c, n=0, method=0)
        }
      }
      
      # Create queue class object
      output_m <- QueueingModel(input_m)
      L <- output_m$L
      Rev <- output_m$Throughput * input$revenue
      tmp <- wcpu * L + scps * c
      p <- Rev - tmp
      
      costs <- c(costs, tmp)
      rev <- c(rev, Rev)
      profit <- c(profit, p)
    }
    
    return(data.frame(Mu = mu_r, Costs = costs, Revenue = rev, Profit = profit))
  })
  
  costs_c <- reactive({
    wcpu <- input$waitingcost
    lambda <- input$lambda
    mu <- input$no_served
    scps <- input$servicecost
    c <- input$n_servers
    
    if(c < 3){
      c_r <- seq(1,5,1) 
    }else{
      c_r <- seq(c-2,c+2,1)
    }
    costs <- c()
    rev <- c()
    profit <- c()
    
    for(n in 1:5){
      m <- mu
      c <- c_r[n]
      
      if(c == 1){
        input_m <- NewInput.MM1(lambda = lambda, mu = m, n = 0)
      }else{
        if(c > 1){
          input_m <- NewInput.MMC(lambda=lambda, mu=m, c=c, n=0, method=0)
        }
      }
      
      # Create queue class object
      output_m <- QueueingModel(input_m)
      L <- output_m$L
      Rev <- output_m$Throughput * input$revenue
      tmp <- wcpu * L + scps * c
      p <- Rev - tmp
      
      costs <- c(costs, tmp)
      rev <- c(rev, Rev)
      profit <- c(profit, p)
    }
    
    return(data.frame(C = c_r, Costs = costs, Revenue = rev, Profit = profit))
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
  profit_df <- NULL
  
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
  
  observeEvent(input$read, {
    
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
    
    
  })
  
  observeEvent(input$save, {
    
    output$report <- renderText({

      if(input$n_servers == 1){
        input_m <- NewInput.MM1(lambda = input$lambda, mu = input$no_served, n = 0)
      }else{
        if(input$n_servers > 1){
          c = input$n_servers
          input_m <- NewInput.MMC(lambda=input$lambda, mu=input$no_served, c=c, n=0, method=0)
        }
      }
      
      
      # Create queue class object
      output_m <- QueueingModel(input_m)
      for(i in 2:15){
        output_m[i][[1]] <- round(output_m[i][[1]],2)
      }
      
      # Get queue model report
      y <- capture.output(Report(output_m))
      return(y)
    })
    
    
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
                      `Avg. number of customers in the system (L)` = evaluation$L,
                      `Avg. queue length (Lq)` = evaluation$Lq,
                      `Avg. time a customer spends in the system (W)` = evaluation$W,
                      `Avg. waiting time of an arrival (Wq)` = evaluation$Wq,
                      check.names = FALSE)
    
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
    
    output$costplot <- renderPlotly({
      
      df <- costs_lambda()
      fig <- plot_ly(df, x=~Costs, y=~Lambda, type = 'scatter', mode = 'lines', name = 'Arrival Rate')  %>% 
        #add_trace(y=~y1+1, type = 'scatter', mode = 'lines', name = 'Cost of waiting time') %>% 
        layout(yaxis = list(title= 'X'))
      
      df <- costs_mu()
      fig <- fig %>% add_trace(x=df$Costs, y=df$Mu, type = 'scatter', mode = 'lines', name = 'Service rate')
      
      df <- costs_c()
      fig <- fig %>% add_trace(x=df$Costs, y=df$C, type = 'scatter', mode = 'lines', name = 'Number of servers')
      
      fig
    })
    
    output$revplot <- renderPlotly({
      
      df <- costs_lambda()
      fig <- plot_ly(df, x=~Revenue, y=~Lambda, type = 'scatter', mode = 'lines', name = 'Arrival Rate')  %>% 
        #add_trace(y=~y1+1, type = 'scatter', mode = 'lines', name = 'Cost of waiting time') %>% 
        layout(yaxis = list(title= 'X'))
      
      df <- costs_mu()
      fig <- fig %>% add_trace(x=df$Revenue, y=df$Mu, type = 'scatter', mode = 'lines', name = 'Service rate')
      
      df <- costs_c()
      fig <- fig %>% add_trace(x=df$Revenue, y=df$C, type = 'scatter', mode = 'lines', name = 'Number of servers')
      
      fig
    })
    
    output$proplot <- renderPlotly({
      
      df <- costs_lambda()
      fig <- plot_ly(df, x=~Profit, y=~Lambda, type = 'scatter', mode = 'lines', name = 'Arrival Rate')  %>% 
        #add_trace(y=~y1+1, type = 'scatter', mode = 'lines', name = 'Cost of waiting time') %>% 
        layout(yaxis = list(title= 'X'))
      
      df <- costs_mu()
      fig <- fig %>% add_trace(x=df$Profit, y=df$Mu, type = 'scatter', mode = 'lines', name = 'Service rate')
      
      df <- costs_c()
      fig <- fig %>% add_trace(x=df$Profit, y=df$C, type = 'scatter', mode = 'lines', name = 'Number of servers')
      
      fig
    })
    
    
    output$breakeven <- renderPlotly({
      
      data <- calculateBE()
      t <- seq(1,length(data),1)
      df <- data.frame(Time = t, Profit = data)
      
      plot_ly(df, x=~Time, y=~Profit, type = 'bar')
      
      
    })
    
    if(c == 1){
      input_m <- NewInput.MM1(lambda = input$lambda, mu = input$no_served, n = 0)
    }else{
      if(c > 1){
        input_m <- NewInput.MMC(lambda=input$lambda, mu = input$no_served, c=input$n_servers, n=0, method=0)
      }
    }
    
    # Create queue class object
    output_m <- QueueingModel(input_m)
    Tp <- output_m$Throughput
    base_profit <- Tp * input$revenue
    
    base <- c(base_profit)
    for(i in 2:20){
      tmp <- base[length(base)]
      base <- c(base, tmp+base_profit)
    }
    
    profit_df <<- data.frame(Profit = base, check.names = FALSE)
    
  })
  
  m <<- 1
  observeEvent(input$compare, {
    
    plus <- calculateBE()
    if(length(plus) > 20){
      plus <- plus[1:20]
    }
    
    profit_df$`Additional Profit` <<- paste(round(plus,2),'€')
    profit_df$`Uplift %` <<- paste(round(plus/profit_df$Profit,2),'%')
    colnames(profit_df)[ncol(profit_df)-1] <<- paste(colnames(profit_df)[ncol(profit_df)-1],' ',as.character(m))
    colnames(profit_df)[ncol(profit_df)] <<- paste(colnames(profit_df)[ncol(profit_df)],' ',as.character(m))
    m <<- m+1
        
    output$results <- function () {
      profit_df %>% 
        knitr::kable("html") %>%
        kable_styling("striped", full_width = T)
    }
    
  })
  
  calculateBE <- function(wcpu = input$waitingcost, lambda = input$lambda, nl = input$lambda + input$add_ar, scps = input$servicecost, mu = input$no_served, nm = input$no_served + input$add_sr, c = input$n_servers, nc = input$n_servers + input$add_c){
    costs <- c()
    rev <- c()
    cumprofit <- c()
    continue <- TRUE
    n <- 1
    i <- 1
    
    while(continue){
      
      if(lambda >= mu){
        return(NULL)
      }
      
      if(c == 1){
        input_m <- NewInput.MM1(lambda = lambda, mu = mu, n = 0)
      }else{
        if(c > 1){
          input_m <- NewInput.MMC(lambda = lambda, mu = mu, c = c, n = 0, method = 0)
        }
      }
      
      # Create queue class object
      output_m <- QueueingModel(input_m)
      
      L <- output_m$L
      Rev <- output_m$Throughput * input$revenue
      tmp <- wcpu * L + scps * c
      p <- Rev - tmp
      
      if(nc == 1){
        input_m <- NewInput.MM1(lambda = nl, mu = nm, n = 0)
      }else{
        if(nc > 1){
          input_m <- NewInput.MMC(lambda = nl, mu = nm, c = nc, n = 0, method = 0)
        }
      }
      
      # Create queue class object
      output_m <- QueueingModel(input_m)
      
      Ln <- output_m$L
      Revn <- output_m$Throughput * input$revenue
      tmpn <- wcpu * Ln + scps * nc
      pn <- Revn - tmpn
      
      
      if(length(cumprofit)==0){
        improvement_cost <- input$trainingcost * input$add_sr + input$marketingcost * input$add_ar + input$hiringcost * input$add_c
        cumprofit <- pn - p - improvement_cost
      }else{
        np <- cumprofit[length(cumprofit)]+pn-p
        cumprofit <- c(cumprofit, np)
        if(cumprofit[length(cumprofit)] > 0){
          n <- n+1
        }
      }
      
      if((n>1) & (i >= 20)){
        continue <- FALSE
      }
      i <- i+1
    }
    return(cumprofit)
  }
  
})

# # input data
# fig <- plot_ly(x=x$n, y=x$count, type = "bar", name = 'Input data') %>% 
#   layout(xaxis = list(title = 'Number of arrivals per unit time'),
#          yaxis = list(title = 'Count'))
# 
# # poisson
# tmp <- data.frame(y = dpois(seq(0,nrow(x)-1),lambda)*420, x = seq(0,nrow(x)-1))
# fig <- fig %>% add_trace(tmp, x =tmp$x, y=tmp$y, type = 'scatter', mode='lines', name = 'Poisson')
