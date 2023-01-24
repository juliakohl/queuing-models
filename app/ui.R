library(shiny)
library(plotly)
library(flexdashboard)
library(shinydashboard)
#library(shinyjs)
#library(shinycssloaders)
library(shinyWidgets)
library(kableExtra)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
      tags$head(
        tags$style(HTML("body{
                      background-color: #ecf0f5;
                    }
                      .button {
                        padding: 0px 4.5px;
                        text-align: center;
                        text-decoration: none;
                        display: inline-block;
                        font-size: 10px;
                        margin: 0px 4px;
                        cursor: pointer;
                      }
                      .circle_blue {
                        border-radius: 50%;
                        background-color: #fff;
                        color: #504c4b;
                        border: none;
                      }
                  .tooltip {
                        position: relative;
                        display: inline-block;
                        #border-bottom: 1px dotted black;
                        opacity: 1;
                        z-index: initial!important;
                     }
                     .tooltip .tooltiptext {
                        visibility: hidden;
                        width: 700px;
                        background-color: #fff;
                        color: #504c4b;
                        text-align: left;
                        border-radius: 6px;
                        padding: 6px 6px 6px 6px;
                        /* Position the tooltip */
                        position: absolute;
                        z-index: 50001;
                        top: 20px;
                        left: 150%;
                        box-shadow: 2px 4px 4px 3px rgba(0, 0, 0, 0.2);
                      }
                      .tooltiptext.smallerbox {
                        width:400px;
                      }
                      .tooltiptext.smallerbox.tooltiptext_topleft {
                        top: -250px;
                        left: -450px;
                      }
                      .tooltip:hover .tooltiptext {
                        visibility: visible;
                      }
                      "
        )
        )
      ),
      tags$head(tags$style("table th{padding: 7px; background-color: #D8142C!important; color: white}", 
                           media="screen", type="text/css")),
      tags$head(tags$style("table td {padding: 7px;} table, th, td {border: 1px solid black; border-collapse: collapse;}",
                           media="screen", type="text/css")),
        dashboardPage(
            dashboardHeader(
                title = "Queuing models"
            ),
            dashboardSidebar(
                width = 400,
                column(width=11, 
                       fileInput("arrivals", 
                                 label = uiOutput("arrivals_label"),
                                 accept = c(
                                   "text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv",
                                   ".xlsx")
                                 ),
                       numericInput("column", label = uiOutput("column_label"), value = 2),
                       #numericInput("unittime", "One unit time", value = "0.1"),
                       numericInput('totalunits', label = uiOutput("units_label"), value = 420),
                       actionButton("read", "read data"),
                       #selectInput("system", "Queuing system", c("M/M/1", "M/M/c", "M/M/1/k"), "M/M/1")),
                #column(width = 11, h4('Arrivals')),
                       #selectInput('arrival_bhvr', 'Arrival distribution', c('Poisson', 'Normal', 'Constant'), 'Poisson')),
                #sliderInput('no_arrivals', 'number of arrivals per unit time', 0, 100, 3),
                #column(width=11, h4('Queue discipline')),
                #column(width=11, h4('Service facility')),
                #conditionalPanel("input.system == 'M/M/c'",
                        numericInput('lambda', label = uiOutput('lambda_label'), min = 0, max = 50000, value = 5),
                #),   
                        numericInput('no_served', 
                            label = uiOutput('mu_label'), 
                             min = 0, 
                             max = 100, 
                             value = 5.5),
                        sliderInput('n_servers', "Number of servers",1,10,1),
                        #numericInput('capacity', 'System capacity ( 0 = no limit )', min = 0, max = 50000, value=0),
                        #actionButton('simulate', 'Calculate system performance'),
                        actionButton('save', 'Start Calculation!')
                )          
              ),
          
            dashboardBody(
              tabsetPanel(type = "tabs",
                          tabPanel("Input data",
                            h2("Input data distribution"),
                            plotlyOutput('inputdist'),
                            h2('Model Report'),
                            textOutput("report"),
                            h2('Model Comparison'),
                            tableOutput("eval")
                          ),
                          tabPanel("Monetary analysis",
                            inputPanel(
                                   numericInput('waitingcost', 'Waiting cost per unit', 1),
                                   numericInput('servicecost', 'Service cost per server', 4),
                                   numericInput('revenue', 'Revenue per unit served', 8),
                                   numericInput('trainingcost', 'Cost to improve service rate by 1', 10),
                                   numericInput('marketingcost', 'Cost to increase arrival rate by 1', 5),
                                   numericInput('hiringcost', 'Cost to add another server', 20)
                            ),
                            h2('Costs'),
                            plotlyOutput('costplot'),
                            h2('Revenue'),
                            plotlyOutput('revplot'),
                            h2('Profit'),
                            plotlyOutput('proplot'),
                            h2('Break Even'),
                            inputPanel(
                              numericInput('add_sr', 'Increase service rate by ...', 0.5),
                              numericInput('add_ar', 'Increase arrival rate by ...', 0.5),
                              numericInput('add_c', 'Add ... servers', 1)
                            ),
                            plotlyOutput('breakeven'),
                            actionButton('compare', 'Save for comparison'),
                            tableOutput('results')
                          )
              )
            )
        )
    )
)
