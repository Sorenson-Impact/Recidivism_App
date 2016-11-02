library(shiny)
library(scales)

# Define UI for random distribution application 
fluidPage(
  
  # Application title
  titlePanel("Tabsets"),
  
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
                  "Average Prison Sentence (months):", 
                  value = 31,
                  min = 1, 
                  max = 60),
      
      br(),
      
      sliderInput("cost_per_yr", 
                  "Cost / Prisoner / Yr:", 
                  value = 30000,
                  min = 0, 
                  max = 60000)
    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Plot", plotOutput("plot")), 
                  tabPanel("Summary", verbatimTextOutput("summary")), 
                  tabPanel("Table", tableOutput("table"))
      )
    )
  )
)