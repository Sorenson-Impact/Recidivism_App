library(shiny)
library(scales)
library(plotly)
library(shinydashboard)
# Define UI for random distribution application 

dashboardPage(
  dashboardHeader(title ="RecidiViz: Analyzing Reincarceration" ,
                  titleWidth = 350),
  dashboardSidebar(disable= TRUE),
dashboardBody (
  
  # Application title
  
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  fluidRow(
    column(3,
      h3("About this App"),
      box(width=12, p("Recidivism is one of society's most persistent, yet misunderstood, problems. Everyone from politicians to", a("Supreme Court Justices",     href= "https://www.themarshallproject.org/2014/12/04/the-misleading-math-of-recidivism#.AQSpHMFig"), "seem to get it wrong.")),
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
                  max = 60000,
                  pre = "$"),
      
      
      
      br(),
      
      actionButton("goButton", "Re-Run the Model"),
      p("This simulates 1,000 prisoners over 60 months, so it may take a few seconds.")
    ),
    
      
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    column(9,
      tabsetPanel(type = "tabs", 
                  tabPanel("Plot", 
                           fluidRow(box(width=7, plotlyOutput("plot")),
                                 box( width=5, htmlOutput("graph1"))),
                           fluidRow(box(plotlyOutput("plot2")),
                                    box(htmlOutput("graph2"))),
                           fluidRow(box(plotlyOutput("plot3")),
                                    box(htmlOutput("graph3")))),
                  tabPanel("Data", tableOutput("table")))
      )
    )
  ))
