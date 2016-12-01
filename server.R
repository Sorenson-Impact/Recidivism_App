library(shiny)
library(expm)
library(ggplot2)

default_data <- read.csv("./default_data.csv")

# This is a vector of the probability of recidivating given months free (i)
# The values are calculated from national trends - see the "analyze_recidivism" script
recid<-c(0.023, 0.027, 0.035, 0.035, 0.038, 0.033, 0.031, 0.028, 0.031, 0.025, 0.025, 0.025, 0.025, 0.021, 0.019, 0.019, 0.018, 0.018, 0.015, 0.013, 0.014, 0.013, 0.014, 0.014, 0.013, 0.01, 0.013, 0.011, 0.01, 0.01, 0.01, 0.009, 0.008, 0.008, 0.008, 0.009, 0.008, 0.007, 0.006, 0.005, 0.005, 0.006, 0.005, 0.004, 0.005, 0.005, 0.004, 0.005, 0.004, 0.005, 0.004, 0.005, 0.005, 0.003, 0.003, 0.004, 0.005, 0.003, 0.004, 0.004)
# survival_rates<-matrix(c(recid,1-recid),ncol=2)


build_model <- function(recid_rate, prison_time_served){
  # This is the matrix for calculating the prob of recidivating 
  # We adjust by dividing the rate selected by the 5-year rate used for this data:
  survival_rates <- matrix(c(recid*(recid_rate/.551), 1-(recid*(recid_rate/.551))), ncol = 2)
  
  number_in_prison <- rep(0,60)
  
  # Made this a log normal becuse the long tail seems to capture the gap between
  # Mean and Median in actual BJS data
  # The log of the mean = the median for the actual data generated
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
  parolees<-data.frame(months=c(1:60),on_parole=1000-number_in_prison,prisoners=number_in_prison, survival_rates = survival_rates)
  
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
}


# Based on http://shiny.rstudio.com/gallery/tabsets.html

# Define server logic for random distribution application
function(input, output) {
  
  beginning <- Sys.time()
  
  # Reactive expression to generate the requested distribution.
  # This is called whenever the inputs change. The output
  # functions defined below then all use the value computed from
  # this expression
  
  rv<-reactiveValues(data=default_data)
  
  # data <- eventReactive(input$goButton, {
  #   build_model(input$recid_rate, input$prison_time_served)
  # })
  
  observeEvent(input$goButton, {
    rv$data <- build_model(input$recid_rate, input$prison_time_served)
  })
  
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  
  end <- Sys.time()
  print(end - beginning)
  
  output$plot <- renderPlot({
    
    ggplot(rv$data, aes(x = months, y = on_parole)) + geom_bar(stat = "identity")
    
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
