
#written by Samuel Nelson and Daniel Hadley in 2016-2017
#
library(shiny)
library(scales)
library(plotly)
library(shinythemes)
# Define UI for random distribution application 



fluidPage(
  
  theme = shinytheme("flatly"),
  
  titlePanel("RecidiViz: Analyzing Reincarceration"),


  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  fluidRow(
    column(3,
      sliderInput("recid_rate", 
                  "5-yr Recidivism Rate:", 
                  value = 55,
                  min = 0, 
                  max = 100,
                  post = "%"),
      
      br(),
      
      sliderInput("prison_time_served", 
                  "Average Prison Sentence (months):", 
                  value = 37,
                  min = 1, 
                  max = 120),
      
      br(),
      
      sliderInput("cost_per_yr", 
                  "Cost / Prisoner / Yr:", 
                  value = 30000,
                  min = 0, 
                  max = 60000,
                  pre = "$"),
      
      
      
      br(),
      
      actionButton("goButton", "Re-Run the Model"),
      p("This simulates 1,000 prisoners over 60 months, so it may take a few seconds.")
    ),
    
      
    # of the generated distribution
    column(
      width = 8,
      fluidRow(htmlOutput("graph1"),
               plotlyOutput("plot")
      ),
      fluidRow(htmlOutput("graph2"),
               plotlyOutput("plot2")
      ),
      fluidRow(htmlOutput("graph3"),
               plotlyOutput("plot3")
      )
      )
    )
  )
