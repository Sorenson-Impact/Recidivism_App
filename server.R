
#written by Samuel Nelson and Daniel Hadley in 2016-2017
#
library(shiny)
library(expm)
library(ggplot2)
library(RColorBrewer)
library(plotly)
library(tibble)
library(plyr)
library(dplyr)
library(tidyr)

default_data <- read.csv("./default_data.csv")
default_arrests<-read.csv("./default_arrests.csv")
default_recid<-read.csv("./default_recid_rates.csv")

# This is a vector of the probability of recidivating given months free (i)
#values used from rpts05p0510f01.csv which has national recidivism trends
cumu_prob_recid<-c(0.0231,	0.0493,	0.0827,	0.1147,	0.1479,	0.1764,	0.2019,	0.2245,	0.2487,	0.2678,	0.2859,	0.3041,	0.3214,	0.3358,	0.3484,	0.361,	0.3723,	0.3835,	0.3928,	0.4009,	0.4092,	0.4169,	0.4248,	0.4328,	0.4403,	0.446,	0.453,	0.4588,	0.4642,	0.4694,	0.4745,	0.4794,	0.4836,	0.4878,	0.4921,	0.4966,	0.5004,	0.5037,	0.5069,	0.5096,	0.5121,	0.5149,	0.5173,	0.5194,	0.522,	0.5242,	0.5263,	0.5288,	0.5307,	0.533,	0.535,	0.5371,	0.5394,	0.541,	0.5425,	0.5443,	0.5465,	0.5479,	0.5495,	0.5511)

# survival_rates<-matrix(c(recid,1-recid),ncol=2)


build_model <- function(recid_rate, prison_time_served, updateProgress = NULL){
  # This is the matrix for calculating the prob of recidivating 
  # We adjust by dividing the rate selected by the 5-year rate used for this data:
  cumu_prob_recid<-cumu_prob_recid*(recid_rate/.551)
  #the next four lines replicate the procdure used in analyze_recidivism 
  #to replicate recidivism trends based on national trends
 recid_rates<-data.frame(cumu_prob_not_recid=1-cumu_prob_recid) 
 recid_rates<-recid_rates%>%mutate(recid=1-cumu_prob_not_recid/lag(cumu_prob_not_recid))
 recid_rates$recid[1]=1-recid_rates$cumu_prob_not_recid[1]
 prob_rec<-round(recid_rates$recid,digits = 3)
  
  survival_rates <- matrix(c(prob_rec, 1-prob_rec), ncol = 2)

  
  # Prison time
  #prison_time_served <- prison_time # This is an input on the final model
  prison_sample <- rlnorm(1000, log(prison_time_served))
  
  
  
  
  # Initalize the outputs of the simulation for loop
  simulations <- vector("list", 1000)
  numberOfArrests <- vector("double", 1000)
  
  
  for(sim in 1:length(simulations)){
    # Initalize the outputs of the single simulation
    tmp.months_free<-0;
    months_free_vector <- vector("double", 60)

    for (month in 1:length(months_free_vector)){
     
      months_free <- calc_months_free(month, tmp.months_free)
      # p(arrest | months free) based on national data
      rearrested <- calc_odds_of_being_rearrested(months_free,survival_rates)
      # IF arrested, choose a random prison sentence from the prison_sample distribution
      tmp.months_free <- ifelse(rearrested == 1, 
                                 round((sample(prison_sample, 1)) * -1), months_free)
      # Now add the months free and arrests to the right vectors
      months_free_vector[[month]] <- tmp.months_free
      numberOfArrests[sim]<-ifelse(rearrested==1, numberOfArrests[sim] + 1, 
                                   numberOfArrests[sim])
      
      # m.month<-month
      #m.months_free<-calc_months_free(m.month,prison_time,tmp.months_free)
      #m.rearrested<-calc_odds_of_being_rearrested(m.months_free,survival_rates)
      #numberOfArrests[i]<-ifelse(m.rearrested==1,numberOfArrests[i]+1,numberOfArrests[i])
      #first_arrests[month]<-ifelse(m.rearrested==1 & numberOfArrests[i]==1,first_arrests[month]+1,first_arrests[month])
      #prison_time<-ifelse(m.rearrested==1,round(sample(prison_sample,1)),prison_time)
      #m.released<-ifelse(prison_time==1,1,0)
      #m.is_in_prison<-say_if_in_prison(prison_time)
      #prison_time<-ifelse(m.is_in_prison==1,prison_time-1,prison_time)
      #tmp.months_free <- ifelse(m.rearrested == 1, 0, m.months_free)
      #df[month,]<-c(m.month,m.months_free,m.is_in_prison,m.rearrested,m.released)
      
    }
    
    # Add each simulation to a list   
    simulations[[sim]] <- months_free_vector

    
    # If we were passed a progress update function, call it
    if (is.function(updateProgress)) {
      text <- paste0("Simulation:", sim, "/1,000")
      updateProgress(detail = text, value = sim/1000)
    }
    
    
  }
  
  # Now use these variables to create a tidy tibble
  months_free <- unlist(simulations)
  months <- rep(seq(1:60), 1000)
  id <- rep(1:1000, each=60)
  free <- tibble(months_free, months, id)
  
  # This is the data we share and base the in/out chart on
  parolees <- free %>% 
    group_by(months) %>% 
    count(on_parole = sum(months_free > 0)) %>% 
    mutate(prisoners = n - on_parole) %>% 
    select(-n)
  
  # The outputs
  returns <- list(parolees = parolees, arrested = numberOfArrests,rates=cumu_prob_recid)
  
#  parolees<-data.frame(months=c(1:60),on_parole=1000-number_in_prison,prisoners=number_in_prison, survival_rates = survival_rates, released=number_released,rearrested=number_rearrested)
 # returns<-list(parolees=parolees,arrested=numberOfArrests,rates=first_arrests)
  #return(returns)
}




# Simply for summing later and helping with cost functions below
say_if_in_prison <- function(m.prison_sentence) {
  ifelse(m.prison_sentence > 0, 1, 0)
}

# If the agent is free, how many months has he or she been free
calc_months_free <- function(month, tmp.months_free){
  ifelse(month == 1, 1, tmp.months_free + 1)
}

# p(arrest | months free)
calc_odds_of_being_rearrested <- function(months_free,survival_rates){
  ifelse(months_free <= 0, 0,
         sample(x = c(1, 0), 
                size = 1, 
                replace = FALSE, 
                prob = c(survival_rates[months_free,])))
}


# Based on http://shiny.rstudio.com/gallery/tabsets.html

# Define server logic for random distribution application
function(input, output) {
  default_list<-list(parolees=default_data,arrested=default_arrests,rates=default_recid)
  # Loads default data
  rv<-reactiveValues(data=default_list)
  
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
  output$plot <- renderPlotly({
    returned_data<-rv$data
    returned_stack<-returned_data$parolees
    stack<-data.frame(values=c( returned_stack$on_parole,returned_stack$prisoners),status=rep(c("Parolees","Prisoners"),each=60),months=rep.int(1:60,2))
    p<-ggplot(stack, aes(x = months, y = values,fill=status, text = paste("Count:", values,"<br>Month:",months,"<br>Status:",status)))+ 
      geom_bar(stat = "identity",position = "stack")+
      labs(x="Months",y="Count")
    ggplotly(p,tooltip = "text")
    
  })
  
  output$plot2<- renderPlotly({
    returned_data<-rv$data
    returned_rates<-data.frame(Recidivated=(returned_data$rates),Month=rep.int(1:60,2))
    p<-ggplot(returned_rates,aes(x=Month))+
    geom_line(aes(y=Recidivated))+scale_y_continuous(labels=percent)+
    labs(x="Months",y="% Recidivated", colour="Status")
   # print(returned_data$rates)
    ggplotly(p)
  })
  
  output$plot3<-renderPlotly({
    returned_data<-rv$data
    returned_arrests<-data.frame(Arrests=returned_data$arrested)
    p<-ggplot(returned_arrests,aes(x= Arrests))+geom_histogram(binwidth = 1)+
    labs(x="Number of Arrests in 60 months",y="Count")
    ggplotly(p)
  })
  
  # Generate a summary of the data
  output$table <- renderTable({
    returned_data<-rv$data
    data.frame(x=returned_data$parolees)
  })

  output$graph1<-renderUI({
    returned_data<-rv$data
    returned_stack<-returned_data$parolees
    stack<-data.frame(values=c(on_parole= returned_stack$on_parole,imprisoned=returned_stack$prisoners),status=rep(c("On Parole","Prisoners"),each=60),months=rep.int(1:60,2))
    HTML(paste(h3("Number in Parole/Prison"),p("It is sometimes assumed that because of high recidivism rates, that over the next few years the porportion number of prisoners on parole would grow smaller and smaller. However in reality it levels off after an initial drop resulting in about ",
                                               percent( returned_stack$on_parole[60]/1000),
                                               "on parole after 60 months. This can be explained by a low average number of rearrests. ",
                                               "The cost per Parolee per Year is ",
                                               dollar((input$cost_per_yr*(1000-returned_stack$on_parole[1]))/1000),
                                                "in the first month and ",
                                                dollar((input$cost_per_yr*(1000-returned_stack$on_parole[60]))/1000),
                                                "in the last month. "))
    )
  })
  
  output$graph2<-renderUI({
    returned_data<-rv$data
    returned_rates<-data.frame(Recidivated=returned_data$rates,months=rep.int(1:60,2))
    HTML(paste(h3("Recidivism Rates"), p(paste("Most released prisoners that recidivate will return within the first few years, after which the likely hood they recidivate drops significantly.The percentage of released prisoners that have recidivated within 36 months is"),percent( returned_rates$Recidivated[36]),". The cumulative percentage in the 60th month may be slightly off from the given 5 Year Recidivism Rate due to our simulation and the relativly low sample size."))
    )
  })
  
  output$graph3<-renderUI({
    returned_data<-rv$data
    returned_arrests<-data.frame(Arrests=returned_data$arrested)
    HTML(paste(h3("Number of Arrests per Person"), p(paste("Usually parolees are not rearrested more than twice with"),percent(length(which(returned_arrests>0 & returned_arrests<3))/length(which(returned_arrests>0))),"only getting rearrested 1 or 2 times.This may only be in the 60 months however the chance for recidivism clearly lowers the longer one has been out of prison so these averages should hold for longer time periods. "))
    )
  })
}

