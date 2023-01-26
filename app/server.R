library(shiny)
library(queueing)
library(plotly)
library(kableExtra)
library(readxl)
library(dplyr)
library(fitdistrplus)

# Define server logic
shinyServer(function(input, output, session) {
  
  el <- reactive({
    lambda <- input$lambda 
    mu <- input$no_served 
    
    if(input$n_servers == 1){
      input_m <- NewInput.MM1(lambda = lambda, mu = input$no_served * input$n_servers, n = 0)
    }else{
      if(input$n_servers > 1){
        c = input$n_servers
        input_m <- NewInput.MMC(lambda=lambda, mu=input$no_served * input$n_servers, c=c, n=0, method=0)
      }
    }
    
    # Create queue class object
    output_m <- QueueingModel(input_m) 
    
    return(cbind(round(summary(output_m)$el,2),Throughput=round(output_m$Throughput,2)))
  })
  
  costs_lambda <- reactive({
    wcpu <- input$waitingcost
    ar <- input$lambda
    ar_r <- seq(ar/10,ar+ar/10*9,ar/10)
    scps <- input$servicecost
    c <- input$n_servers
    costs <- c()
    rev <- c()
    profit <- c()
    
    for(n in 1:length(ar_r)){
      lambda <- ar_r[n]
      mu <- input$no_served * c
      
      if(lambda >= mu){
        costs <- c(costs, NA)
        rev <- c(rev, NA)
        profit <- c(profit, NA)
        next
      }
      
      if(input$n_servers == 1){
        input_m <- NewInput.MM1(lambda = lambda, mu = input$no_served * c, n = 0)
      }else{
        if(input$n_servers > 1){
          c = input$n_servers
          input_m <- NewInput.MMC(lambda=lambda, mu=input$no_served * c, c=c, n=0, method=0)
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
    mu <- input$no_served * input$n_servers
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
    mu <- input$no_served * input$n_servers
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

      }else{
        queue[n] <<- a
      }
      
      n <- n+1
    }
    
    
  })
  
  observeEvent(input$save, {
    
    output$report <- renderText({
      c <- input$n_servers
      if(c == 1){
        input_m <- NewInput.MM1(lambda = input$lambda, mu = input$no_served, n = 0)
      }else{
        if(c > 1){
          input_m <- NewInput.MMC(lambda=input$lambda, mu=input$no_served * c, c=c, n=0, method=0)
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
    
    reportdf <- data.frame(`Avg. arrival rate (lambda)` = evaluation$lambda, 
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
    
    output$eval <- function () {
      #evaluation %>% 
      reportdf %>% 
        knitr::kable("html") %>%
        kable_styling("striped", full_width = T)
    }
    
    output$costplot <- renderPlotly({
      
      df <- costs_lambda()
      fig <- plot_ly(df, x=~Costs, y=~Lambda, type = 'scatter', mode = 'lines', name = 'Arrival Rate')  %>% 
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
      
      z <- c(input$add_ar, input$add_sr, input$add_c)

      if(input$lambda+input$add_ar >= (input$no_served+input$add_sr)*(input$n_servers+input$add_c) ){
        showNotification("Error. Lambda can not be greater than mu.", type = 'error')
        NULL
      }else{
        data <- calculateBE()
        t <- seq(0,(length(data)-1)*420,420)
        df <- data.frame(Time = t, Profit = data)
  
        plot_ly(df, x=~Time, y=~Profit, type = 'bar')
      }
    })
    
    if(input$lambda >= (input$no_served+input$add_sr)*(input$n_servers+input$add_c)){
      showNotification("Error. Lambda can not be greater than mu.")
    }else{
      c <- input$n_servers
      if(c == 1){
        input_m <- NewInput.MM1(lambda = input$lambda, mu = input$no_served, n = 0)
      }else{
        if(c > 1){
          input_m <- NewInput.MMC(lambda=input$lambda, mu = input$no_served*input$n_servers, c=input$n_servers, n=0, method=0)
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
      
      profit_df <<- data.frame(Period = seq(0,(length(base)-1)*420,420), Profit = round(base,2), check.names = FALSE)
    }
  })
  
  mmm <<- 1
  observeEvent(input$compare, {
    
    plus <- calculateBE()
    if(length(plus) > 20){
      plus <- plus[1:20]
    }
    
    profit_df$`Additional Profit` <<- paste(round(plus,2),'€')
    profit_df$`Uplift %` <<- paste(round(plus/profit_df$Profit,2),'%')
    colnames(profit_df)[ncol(profit_df)-1] <<- paste(colnames(profit_df)[ncol(profit_df)-1],' ',as.character(mmm))
    colnames(profit_df)[ncol(profit_df)] <<- paste(colnames(profit_df)[ncol(profit_df)],' ',as.character(mmm))
    mmm <<- mmm+1
        
    output$results <- function () {
      profit_df %>% mutate(Profit = paste(Profit,'€')) %>% 
        knitr::kable("html") %>%
        kable_styling("striped", full_width = T)
    }
    
  })
  
  
  # ===================================================================================================================
  # This function calculates the total cumulative profit of a system given certain inputs. 
  # It takes in various parameters such as waiting cost, lambda, mu, 
  # number of servers, additional servers and additional arrivals and services. 
  # It then creates a QueueingModel object using these inputs and calculates the total cumulative profit 
  # of the system by subtracting the waiting costs, service costs and any improvement costs. 
  # The loop continues until either the total cumulative profit is positive or the iterations reach a certain number.
  # ===================================================================================================================
  calculateBE <- function(wcpu = input$waitingcost, lambda = input$lambda, nl = input$lambda + input$add_ar, scps = input$servicecost, mu = input$no_served, nm = input$no_served + input$add_sr, c = input$n_servers, nc = input$n_servers + input$add_c){
    costs <- c()
    rev <- c()
    cumprofit <- c()
    continue <- TRUE
    n <- 1
    i <- 1
    
    if(lambda >= nm*nc){
      showNotification("Error. Lambda can not be greater than mu.")
      break
    }
    
    while(continue){
      
      cat(' ',i)
      
      if(c == 1){
        input_m <- NewInput.MM1(lambda = lambda, mu = mu*c, n = 0)
      }else{
        if(c > 1){
          input_m <- NewInput.MMC(lambda = lambda, mu = mu*c, c = c, n = 0, method = 0)
        }
      }
      
      # Create queue class object
      output_m <- QueueingModel(input_m)
      
      L <- output_m$L
      Rev <- output_m$Throughput * input$revenue
      tmp <- wcpu * L + scps * c
      p <- Rev - tmp
      
      if(nc == 1){
        input_m <- NewInput.MM1(lambda = nl, mu = nm*nc, n = 0)
      }else{
        if(nc > 1){
          input_m <- NewInput.MMC(lambda = nl, mu = nm*nc, c = nc, n = 0, method = 0)
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
        cumprofit <- 0 - improvement_cost
      }else{
        np <- cumprofit[length(cumprofit)]+pn*420-p*420
        cumprofit <- c(cumprofit, np)
        if(cumprofit[length(cumprofit)] > 0){
          n <- n+1
        }
      }
      
      if(((n>1) & (i >= 20)) | (i == 40)){
        continue <- FALSE
      }
      i <- i+1
    }
    return(cumprofit)
  }
  
  output$arrivals_label <- renderUI({
    HTML("Arrival data of the queuing system
          <div class='tooltip'>
            <button class='button circle_blue'><b>?</b></button>
              <span class='tooltiptext smallerbox'>
                <p>
                  <b>Upload a .xlsx file with one row representing one arrival:</b><br>
                  <table>
                  <thead>
                    <tr>
                      <th>Column 1</th><th>Arrival time</th><th> ... </th><th> Column n </th>
                    </tr>
                    </thead>
                    <tbody>
                    <tr>
                      <td>1</td><td>09:24</td><td>...</td><td>...</td>
                    </tr>
                    <tr>
                      <td>2</td><td>09:24</td><td>...</td><td>...</td>
                    </tr>
                    <tr>
                      <td>3</td><td>09:26</td><td>...</td><td>...</td>
                    </tr>
                    </tbody>
                  </table>
                </p>
              </span>
          </div>")
  })
  
  output$column_label <- renderUI({
    HTML("Column of arrival time
          <div class='tooltip'>
            <button class='button circle_blue'><b>?</b></button>
              <span class='tooltiptext smallerbox'>
                <p>
                  <b>In wich column is the arrival time? (Counting from one)</b><br>
                </p>
              </span>
          </div>")
  })
  
  output$units_label <- renderUI({
    HTML("Total number of units in period
          <div class='tooltip'>
            <button class='button circle_blue'><b>?</b></button>
              <span class='tooltiptext smallerbox'>
                <p>
                  <b>How many periods are in the uploaded dataset? e.g. data covers one hour -> Input = 60<br>flexible unit time is not yet available, unit is one minute</b><br>
                </p>
              </span>
          </div>")
  })
  
  output$lambda_label <- renderUI({
    HTML("Avg. number of arrivals per unit time (lambda)
          <div class='tooltip'>
            <button class='button circle_blue'><b>?</b></button>
              <span class='tooltiptext smallerbox'>
                <p>
                  <b>How many customers are arriving per unit time on average?</b><br>
                  This input field is automatically updated when arrival data is uploaded and read into the system.
                </p>
              </span>
          </div>")
  })
  
  output$mu_label <- renderUI({
    HTML("Avg. number of customers served per server & unit time (mu * c)
          <div class='tooltip'>
            <button class='button circle_blue'><b>?</b></button>
              <span class='tooltiptext smallerbox'>
                <p>
                  <b>How many customers are served per unit time?</b><br>
                  Note that mu has to be greater than lambda.
                </p>
              </span>
          </div>")
  })
  
})
