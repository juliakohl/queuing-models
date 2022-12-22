library(shiny)
library(plotly)
library(flexdashboard)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(plotly)
library(kableExtra)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        dashboardPage(
            dashboardHeader(
                title = "Queuing models"
            ),
            dashboardSidebar(
                width = 400,
                column(width=11, 
                       fileInput("arrivals", 
                                 "Arrival data",
                                 accept = c(
                                   "text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv",
                                   ".xlsx")
                                 ),
                       numericInput("column", "Column of arrival time", value = 1),
                       numericInput("unittime", "One unit time", value = "0.1"),
                       numericInput('totalunits', 'Total number of units in period', value = 0),
                       actionButton("read", "read data"),
                       selectInput("system", "Queuing system", c("M/M/1", "M/M/c", "M/M/1/k"), "M/M/1")),
                column(width = 11, h4('Arrivals')),
                sliderInput('no_arrivals', 'number of arrivals per unit time', 0, 100, 3),
                column(width=11, h4('Queue discipline')),
                column(width=11, h4('Service facility')),
                conditionalPanel("input.system == 'M/M/c'",
                       sliderInput('n_servers', "NUmber of servers",1,10,2)
                ),
                sliderInput('no_served', 
                            'number of customers served per unit time', 
                             min = 0, 
                             max = 100, 
                             value = 4),
                actionButton('save', 'Save Results')
                                 
                ),
              

          
            dashboardBody(
                h2("Input data distribution"),
                plotlyOutput('inputdist'),
                h2('Probability Distribution of Arrivals'),
                #plotOutput("dist"),
                plotlyOutput('dist'),
                h2('Model Report'),
                textOutput("report"),
                #plotlyOutput("animation")
                h2('Model Comparison'),
                tableOutput("eval")
            )
        )
    )
)

# selectInput('population',
#             label = "Population",
#             choices = c('limited', 'unlimited'),
#             selected = 'unlimited',
#             multiple = FALSE,
# ),
# selectInput('arrivals_put',
#             label = "Arrivals per unit time",
#             choices = c('Poisson', 'Random'),
#             selected = 'Poisson',
#             multiple = FALSE,
# ),
# selectInput('cust_bhvr',
#             label = "Customer behavior",
#             choices = c('patient', 'balk and renegade'),
#             selected = 'patient',
#             multiple = FALSE,
# ),
#conditionalPanel("input.arrivals_put == 'Poisson'", 
