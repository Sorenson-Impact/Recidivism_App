library(shiny)
library(scales)
library(plotly)

# Define UI for random distribution application 
fluidPage(
  
  # Application title
  titlePanel("RecidiViz: Analyzing Reincarceration"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  fluidRow(
    column(3,
      sliderInput("recid_rate", 
                  "5-yr Recidivism Rate:", 
                  value = .55,
                  min = 0, 
                  max = 1),
      br(),
      
      sliderInput("prison_time_served", 
                  "Median Prison Sentence (months):", 
                  value = 16,
                  min = 1, 
                  max = 60),
      
      br(),
      
      sliderInput("cost_per_yr", 
                  "Cost / Prisoner / Yr:", 
                  value = 30000,
                  min = 0, 
                  max = 60000),
      
      br(),
      
      actionButton("goButton", "Re-Run the Model"),
      p("This simulates 1,000 prisoners over 60 months, so it may take a few seconds.")
    ),
    
      
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    column(8,
      tabsetPanel(type = "tabs", 
                  tabPanel("Plot", 
                           column(8,
                           plotlyOutput("plot"),
                           plotlyOutput("plot2"),
                           plotlyOutput("plot3")
                           ),
                           column(4,
                           htmlOutput("graph1"),
                           htmlOutput("graph2"),
                           htmlOutput("graph3")
)),
    
                  tabPanel("Summary", textOutput("summary")), 
                  tabPanel("Table", tableOutput("table")))
      )
    )
  )
