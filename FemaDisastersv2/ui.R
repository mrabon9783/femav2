#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
options(shiny.maxRequestSize = 10000*1024^2)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("F.E.M.A. Declared Presidential Major Disaster"),
  #mainPanel(
  #  plotOutput("distPlot")
  #),
  tabsetPanel(
    tabPanel("Disaster Mappings",
             sidebarLayout(
               sidebarPanel(
                 selectInput("sRegion",label = "Map Region",choices = c("US","Northeast","Southeast","Midwest","West")),
                 selectInput("sPeriod",label = "Disaster Past Years",choices = c(1,2,3)),
                 actionButton("abdl",label = "Save(disabled)"),
                 textOutput("sBarText")
                 , width = 2),
               
             mainPanel(plotOutput("femausplot"),
               DT::dataTableOutput("femausdatatest")#,
               #wellPanel(h1("this is the well")
               )
             )
             )
    )
  )
  )
  
  # Sidebar with a slider input for number of bins 
  # sidebarLayout(
  #   sidebarPanel(
  #      sliderInput("bins",
  #                  "Number of bins:",
  #                  min = 1,
  #                  max = 50,
  #                  value = 30)
  #   ),
    
    # Show a plot of the generated distribution
    # mainPanel(
    #    plotOutput("distPlot")
    # )
#  )
#)
#)
