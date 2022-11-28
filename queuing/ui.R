library(shiny)
library(ggplot2)
library(plotly)
library(flexdashboard)
library(shinydashboard)
library(shinyjs)
library(formattable)
library(shinycssloaders)
library(shinyWidgets)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        dashboardPage(
            dashboardHeader(
                title = "Queuing models"
            ),
            dashboardSidebar(
                width = 300,
                h4('Arrivals'),
                selectInput('population',
                            label = "Population",
                            choices = c('limited', 'unlimited'),
                            selected = 'unlimited',
                            multiple = FALSE,
                ),
                selectInput('arrivals_put',
                            label = "Arrivals per unit time",
                            choices = c('Poisson', 'Random'),
                            selected = 'Poisson',
                            multiple = FALSE,
                ),
                selectInput('cust_bhvr',
                            label = "Customer behavior",
                            choices = c('patient', 'balk and renegade'),
                            selected = 'patient',
                            multiple = FALSE,
                ),
                h4('Queue discipline'),
                h4('Service facility')
                
            ),
            dashboardBody(
                
            )
        )
    )
)
