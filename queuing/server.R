library(shiny)
library(queueing)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  inputs <- reactive({c(input$arrivals_put, input$no_served)})
  
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
    
    # Get queue model report
    y <- capture.output(Report(output_m))
    return(y)

    # Get queue model summary
    #summary(output_mm1)
  })
  
  output$dist <- renderPlot({
    lambda = input$no_arrivals
    mu = input$no_served
    if(input$system == "M/M/1"){
      input_mm1 <- NewInput.MM1(lambda = lambda, mu = mu, n = 0)
      curve(dpois(x, input_mm1$lambda),
            from = 0, 
            to = 2*input_mm1$lambda, 
            type = "b", 
            lwd = 2,
            xlab = "Number of customers",
            ylab = "Probability",
            main = "Poisson Distribution for Arrival Process",
            ylim = c(0, 0.25),
            n = input_mm1$lambda*2+1)
    }else{
      if(input$system == "M/M/c"){
        c = input$n_servers
        i_mmc <- NewInput.MMC(lambda=lambda, mu=mu, c=c, n=0, method=0)
        curve(dpois(x, input_mm1$lambda),
              from = 0, 
              to = 2*lambda, 
              type = "b", 
              lwd = 2,
              xlab = "Number of customers",
              ylab = "Probability",
              main = "Poisson Distribution for Arrival Process",
              ylim = c(0, 0.5),
              n = lambda*2+1)
      }
    }
  })
  

  # 
  # accumulate_by <- function(dat, var) {
  #   var <- lazyeval::f_eval(var, dat)
  #   lvls <- plotly:::getLevels(var)
  #   dats <- lapply(seq_along(lvls), function(x) {
  #     cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  #   })
  #   dplyr::bind_rows(dats)
  # }
  # 
  # 
  # 
  # 
  # output$animation <- renderPlotly({
  #   df <- txhousing 
  #   fig <- df %>%
  #     filter(year > 2005, city %in% c("Abilene", "Bay Area"))
  #   fig <- fig %>% accumulate_by(~date)
  #   
  #   fig <- fig %>%
  #     plot_ly(
  #       x = ~date, 
  #       y = ~median,
  #       split = ~city,
  #       frame = ~frame, 
  #       type = 'scatter',
  #       mode = 'lines', 
  #       line = list(simplyfy = F)
  #     )
  #   fig <- fig %>% layout(
  #     xaxis = list(
  #       title = "Date",
  #       zeroline = F
  #     ),
  #     yaxis = list(
  #       title = "Median",
  #       zeroline = F
  #     )
  #   ) 
  #   fig <- fig %>% animation_opts(
  #     frame = 100, 
  #     transition = 0, 
  #     redraw = FALSE
  #   )
  #   fig <- fig %>% animation_slider(
  #     hide = T
  #   )
  #   fig <- fig %>% animation_button(
  #     x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  #   )
  #   
  #   fig
  # })
    
  
})
