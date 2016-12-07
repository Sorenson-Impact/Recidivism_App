library(shiny)
library(expm)
library(ggplot2)
library(riverplot)
library(RColorBrewer)

default_data <- read.csv("./default_data.csv")

# This is a vector of the probability of recidivating given months free (i)
# The values are calculated from national trends - see the "analyze_recidivism" script
recid<-c(0.023, 0.027, 0.035, 0.035, 0.038, 0.033, 0.031, 0.028, 0.031, 0.025, 0.025, 0.025, 0.025, 0.021, 0.019, 0.019, 0.018, 0.018, 0.015, 0.013, 0.014, 0.013, 0.014, 0.014, 0.013, 0.01, 0.013, 0.011, 0.01, 0.01, 0.01, 0.009, 0.008, 0.008, 0.008, 0.009, 0.008, 0.007, 0.006, 0.005, 0.005, 0.006, 0.005, 0.004, 0.005, 0.005, 0.004, 0.005, 0.004, 0.005, 0.004, 0.005, 0.005, 0.003, 0.003, 0.004, 0.005, 0.003, 0.004, 0.004)
# survival_rates<-matrix(c(recid,1-recid),ncol=2)


build_model <- function(recid_rate, prison_time_served, updateProgress = NULL){
  # This is the matrix for calculating the prob of recidivating 
  # We adjust by dividing the rate selected by the 5-year rate used for this data:
  survival_rates <- matrix(c(recid*(recid_rate/.551), 1-(recid*(recid_rate/.551))), ncol = 2)
  
  number_in_prison <- rep(0,60)
  number_released<-rep(0,60)
  number_rearrested<-rep(0,60)
  
  # Made this a log normal becuse the long tail seems to capture the gap between
  # Mean and Median in actual BJS data
  # The log of the mean = the median for the actual data generated
  prison_sample <- rlnorm(1000, log(prison_time_served))
  numberOfArrests<-c(rep.int(0,times = 1000))
  for (i in 1:1000){
    prison_time=0
    df <- data.frame(month=numeric(0),
                   months_free=numeric(0),
                   is_in_prison=numeric(0),
                   rearrested=numeric(0),
                   released=numeric(0))
    for (month in 1:60){
      m.month<-month
      m.months_free<-calc_months_free(m.month,prison_time,tmp.months_free)
      m.rearrested<-calc_odds_of_being_rearrested(m.months_free,survival_rates)
      numberOfArrests[i]<-ifelse(m.rearrested==1,numberOfArrests[i]+1,numberOfArrests[i])
      prison_time<-ifelse(m.rearrested==1,round(sample(prison_sample,1)),prison_time)
      m.released<-ifelse(prison_time==1,1,0)
      m.is_in_prison<-say_if_in_prison(prison_time)
      prison_time<-ifelse(m.is_in_prison==1,prison_time-1,prison_time)
      tmp.months_free <- ifelse(m.rearrested == 1, 0, m.months_free)
      df[month,]<-c(m.month,m.months_free,m.is_in_prison,m.rearrested,m.released)
      
    }
    single_agent_prison_time<-df$is_in_prison
    released_by_month<-df$released
    rearrested_by_month<-df$rearrested
    number_in_prison<-number_in_prison+as.numeric(single_agent_prison_time)
    number_released<-number_released+as.numeric(released_by_month)
    number_rearrested <-number_rearrested+as.numeric(rearrested_by_month)
    
    # If we were passed a progress update function, call it
    if (is.function(updateProgress)) {
      text <- paste0("Simulation:", i, "/1,000")
      updateProgress(detail = text)
    }
    
  }
  parolees<-data.frame(months=c(1:60),on_parole=1000-number_in_prison,prisoners=number_in_prison, survival_rates = survival_rates, released=number_released,rearrested=number_rearrested)
  returns<-list(parolees=parolees,arrested=numberOfArrests)
  return(returns)
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
  
  # Loads default data
  rv<-reactiveValues(data=default_data)
  
  # Updates the data if they push the goButton
  # and shows progress on a progress bar
  observeEvent(input$goButton, {
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Re-running Model", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    # Create a callback function to update progress.
    # Each time this is called:
    # - If `value` is NULL, it will move the progress bar 1/1000 of the remaining
    #   distance. If non-NULL, it will set the progress to that value.
    # - It also accepts optional detail text.
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 1000
      }
      progress$set(value = value, detail = detail)
    }
    
    # Compute the new data, and pass in the updateProgress function so
    # that it can update the progress indicator.
    rv$data <- build_model(input$recid_rate, input$prison_time_served, updateProgress)
  })

   #Generate a plot of the data. Also uses the inputs to build
  output$plot <- renderPlot({
    data<-rv$data
    ggplot(data$parolees, aes(x = months, y = on_parole)) + geom_bar(stat = "identity")
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    data<-rv$data
    summary(data$parolees)
    
  })
  
  # Generate an HTML table view of the data
  output$table <- renderTable({
    data<-rv$data
    #data.frame(x=data$parolees)
    data.frame(x=data$arrested)
  })

  
  output$river<-renderPlot({ 
    data<-rv$data
    data<-data$parolees
    stayp<-data.frame(Value=data$on_parole)
    staypr<-data.frame(Value=data$prisoners)
    stayp<- c(as.numeric(stayp[c(12,24,36,48,60),]))
    staypr<- c(as.numeric(staypr[c(12,24,36,48,60),]))
    stay<-rbind(data.frame(Value=stayp),data.frame(Value=staypr))
    
    leavep<-data.frame(Value=data$rearrested)
    leavepr<-data.frame(Value=data$released)
    leavep<- c(sum(as.numeric(leavep[1:12,])),sum(as.numeric(leavep[13:24,])),sum(as.numeric(leavep[25:36,])),sum(as.numeric(leavep[37:48,])),sum(as.numeric(leavep[49:60,])))
    leavepr<- c(sum(as.numeric(leavepr[1:12,])),sum(as.numeric(leavepr[13:24,])),sum(as.numeric(leavepr[25:36,])),sum(as.numeric(leavepr[37:48,])),sum(as.numeric(leavepr[49:60,])))
    
    leave<-rbind(data.frame(Value=leavep),data.frame(Value=leavepr))
    
    
    
    edges<-data.frame(N1 = paste0(rep(0:4, each = 1), rep(LETTERS[1:2], each = 5)),N2 = paste0(rep(1:5, each = 1), rep(LETTERS[1:2], each = 5)),Value=stay)
    edges2<-data.frame(N1 = paste0(rep(0:4, each = 1), rep(LETTERS[1:2], each = 5)),N2 = paste0(rep(1:5, each = 1), rep(LETTERS[2:1], each = 5)),Value=leave)
    edgesf<-rbind(edges,edges2)
    
    nodes<-data.frame(ID = paste0(rep(0:5, each = 1), rep(LETTERS[1:2], each = 6)),x = rep(0:5, each = 1),y=rep(2:1, each = 6))
    rownames(nodes) = nodes$ID 
    
    palette = paste0(brewer.pal(4, "Set1"), "60")
    styles = lapply(nodes$y, function(n) {list(col = palette[n+1], lty = 0, textcol = "black")})
    names(styles) = nodes$ID
    
    river<-makeRiver(nodes,edgesf,edge_styles = styles)
    
    
    plot(river)})
  
  
  
}
