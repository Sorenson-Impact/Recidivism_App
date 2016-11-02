library(shiny)
library(expm)

increase_exponentially <- function(n){
  Oz %^% n
  
  return(Oz[1,1])
}

# Based on http://shiny.rstudio.com/gallery/tabsets.html

# Define server logic for random distribution application
function(input, output) {
  
  # Reactive expression to generate the requested distribution.
  # This is called whenever the inputs change. The output
  # functions defined below then all use the value computed from
  # this expression
  data <- reactive({
    pr_prison <- (1 - input$recid_rate)^(1/(12 * 5))
    pr_parole <- sign(.99) * abs(.99)^(1 / input$prison_time_served)
    Oz <- matrix(c(p, 1-p, 1-p_parole, p_parole), nrow=2, byrow=TRUE)
    
    x <- c(1:60)
    
    unlist(map(x, increase_exponentially))
  })
  
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$plot <- renderPlot({
    dist <- rnorm
    n <- input$prison_time_served
    
    hist(data(), 
         main=paste('r', dist, '(', n, ')', sep=''))
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(data())
  })
  
  # Generate an HTML table view of the data
  output$table <- renderTable({
    data.frame(x=data())
  })
  
}