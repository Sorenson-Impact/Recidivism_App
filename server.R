library(shiny)
library(expm)
library(ggplot2)

recid<-c(	0.076
          ,	0.041
          ,	0.041
          ,	0.04
          ,	0.038
          ,	0.031
          ,	0.031
          ,	0.027
          ,	0.024
          ,	0.022
          ,	0.019
          ,	0.02
          ,	0.018
          ,	0.016
          ,	0.015
          ,	0.015
          ,	0.012
          ,	0.013
          ,	0.011
          ,	0.011
          ,	0.011
          ,	0.009
          ,	0.01
          ,	0.009
          ,	0.008
          ,	0.008
          ,	0.008
          ,	0.008
          ,	0.007
          ,	0.006
          ,	0.006
          ,	0.006
          ,	0.005
          ,	0.006
          ,	0.004
          ,	0.006
          ,	0.004
          ,	0.005
          ,	0.004
          ,	0.005
          ,	0.003
          ,	0.004
          ,	0.005
          ,	0.004
          ,	0.003
          ,	0.004
          ,	0.003
          ,	0.003
          ,	0.003
          ,	0.003
          ,	0.003
          ,	0.004
          ,	0.003
          ,	0.003
          ,	0.002
          ,	0.003
          ,	0.003
          ,	0.002
          ,	0.002
          ,	0.003)
survival_rates<-matrix(c(recid,1-recid),ncol=2)


build_markov_chain <- function(recid_rate, prison_time_served){
  survival_rates <- matrix(c(recid*(recid_rate/.719), 1-(recid*(recid_rate/.7139))), ncol = 2)
  number_in_prison <- rep(0,60)
  prison_sample <- rlnorm(1000, log(prison_time_served))
  for (i in 1:1000){
    prison_time=0
    df <- data.frame(month=numeric(0),
                   months_free=numeric(0),
                   is_in_prison=numeric(0),
                   rearrested=numeric(0))
    for (month in 1:60){
      m.month<-month
      m.months_free<-calc_months_free(m.month,prison_time,tmp.months_free)
      m.rearrested<-calc_odds_of_being_rearrested(m.months_free,survival_rates)
      prison_time<-ifelse(m.rearrested==1,round(sample(prison_sample,1)),prison_time)
      m.is_in_prison<-say_if_in_prison(prison_time)
      prison_time<-ifelse(m.is_in_prison==1,prison_time-1,prison_time)
      tmp.months_free <- ifelse(m.rearrested == 1, 0, m.months_free)
      df[month,]<-c(m.month,m.months_free,m.is_in_prison,m.rearrested)
    }
    single_agent_prison_time<-df$is_in_prison
    number_in_prison<-number_in_prison+as.numeric(single_agent_prison_time)
  }
  parolees<-data.frame(months=c(1:60),on_parole=1000-number_in_prison,prisoners=number_in_prison)
  
  return(parolees)
}
# Simply for summing later and helping with cost functions below
say_if_in_prison <- function(m.prison_sentence) {
  ifelse(m.prison_sentence > 0, 1, 0)
}

# If the agent is free, how many months has he or she been free
calc_months_free <- function(m.month, m.prison_sentence, tmp.months_free) {
  ifelse(m.month == 1, 1,
         ifelse(m.prison_sentence > 1, 0,
                tmp.months_free + 1))
}

# Taking the survival analysis data from national trends, this will calculate the odds of being rearrested conditional on the number of months he or she has been free
calc_odds_of_being_rearrested <- function(m.months_free,survival_rates) {
  ifelse(m.months_free == 0, 0,
         sample(x = c(1, 0), 
                size = 1, 
                replace = FALSE, 
                prob = c(survival_rates[m.months_free,])))
}# Based on http://shiny.rstudio.com/gallery/tabsets.html

# Define server logic for random distribution application
function(input, output) {
  
  # Reactive expression to generate the requested distribution.
  # This is called whenever the inputs change. The output
  # functions defined below then all use the value computed from
  # this expression
  
  data <- eventReactive(input$goButton, {
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
