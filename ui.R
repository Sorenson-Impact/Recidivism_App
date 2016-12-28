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
  sidebarLayout(
    sidebarPanel(
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
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Plot", 
                           plotlyOutput("plot"),
                           h2("About This App"),
                           p("Recidivism is one of society's most persistent, yet misunderstood, problems. Everyone from politicians to", a("Supreme Court Justices",     href= "https://www.themarshallproject.org/2014/12/04/the-misleading-math-of-recidivism#.AQSpHMFig"), "seem to get it wrong."),
                            plotlyOutput("plot2"),
                            h2("About This App"),
                            p("Recidivism is one of society's most persistent, yet misunderstood, problems. Everyone from politicians to", a("Supreme Court Justices",     href= "https://www.themarshallproject.org/2014/12/04/the-misleading-math-of-recidivism#.AQSpHMFig"), "seem to get it wrong."),
      plotlyOutput("plot3"),
      h2("About This App"),
      p("Recidivism is one of society's most persistent, yet misunderstood, problems. Everyone from politicians to", a("Supreme Court Justices",     href= "https://www.themarshallproject.org/2014/12/04/the-misleading-math-of-recidivism#.AQSpHMFig"), "seem to get it wrong.")),
    
                  tabPanel("Summary", verbatimTextOutput("summary")), 
                  tabPanel("Table", tableOutput("table")))
      )
    )
  )
