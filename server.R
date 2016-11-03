library(shiny)
library(expm)
library(ggplot2)


build_markov_chain <- function(recid_rate, prison_time_served){
  
  # First we build a transition matrix
  pr_prison <- (1 - recid_rate)^(1/(12 * 5))
  pr_parole <- pr_parole <- (1 -.99)^(1/ prison_time_served)
  tm <- matrix(c
               (pr_prison, 1 - pr_prison, 1 - pr_parole, pr_parole), 
               nrow=2, byrow=TRUE)
  
  months <- c(1:60)
  on_parole <- vector("integer", length = length(months))
  for(month in months){
    y <- tm %^% month
    on_parole[month] <- y[1]
  }
  
  in_prison <- 1 - on_parole
  
  parolees <- data.frame(months, in_prison, on_parole)
  
  return(parolees)
}


# Based on http://shiny.rstudio.com/gallery/tabsets.html

# Define server logic for random distribution application
function(input, output) {
  
  # Reactive expression to generate the requested distribution.
  # This is called whenever the inputs change. The output
  # functions defined below then all use the value computed from
  # this expression

  data <- reactive({
    build_markov_chain(input$recid_rate, input$prison_time_served)
  })
  
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$plot <- renderPlot({
    
    ggplot(data(), aes(x = months, y = on_parole)) + geom_bar(stat = "identity")
    
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