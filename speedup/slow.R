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
  
  number_in_prison <- rep(0,60)
  number_released<-rep(0,60)
  number_rearrested<-rep(0,60)
  
  # Made this a log normal becuse the long tail seems to capture the gap between
  # Mean and Median in actual BJS data
  # The log of the mean = the median for the actual data generated
  prison_sample <- rlnorm(1000, log(prison_time_served))
  numberOfArrests<-c(rep.int(0,times = 1000))
  first_arrests<-c(rep.int(0,times = 60))
  
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
      first_arrests[month]<-ifelse(m.rearrested==1 & numberOfArrests[i]==1,first_arrests[month]+1,first_arrests[month])
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
      updateProgress(detail = text, value = i/1000)
    }
    
  }
  parolees<-data.frame(months=c(1:60),on_parole=1000-number_in_prison,prisoners=number_in_prison, survival_rates = survival_rates, released=number_released,rearrested=number_rearrested)
  returns<-list(parolees=parolees,arrested=numberOfArrests,rates=first_arrests)
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


system.time(build_model(recid_rate = .55, prison_time_served = 32))
