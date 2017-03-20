cumu_prob_recid<-cumu_prob_recid*(recid_rate/.551)
#the next four lines replicate the procdure used in analyze_recidivism 
#to replicate recidivism trends based on national trends
recid_rates<-data.frame(cumu_prob_not_recid=1-cumu_prob_recid) 
recid_rates<-recid_rates%>%mutate(recid=1-cumu_prob_not_recid/lag(cumu_prob_not_recid))
recid_rates$recid[1]=1-recid_rates$cumu_prob_not_recid[1]
prob_rec<-round(recid_rates$recid,digits = 3)

survival_rates <- matrix(c(prob_rec, 1-prob_rec), ncol = 2)


# If the agent is free, how many months has he or she been free
calc_months_free <- function(m.month, m.prison_sentence, tmp.months_free) {
  ifelse(m.month == 1, 1,
         ifelse(m.prison_sentence > 1, 0,
                tmp.months_free + 1))
}


for (month in 1:60){
  prison_time <- 0
  m.month<-month
  m.months_free<-calc_months_free(m.month,prison_time,tmp.months_free)
  m.rearrested<-calc_odds_of_being_rearrested(m.months_free,survival_rates)
  prison_time<-ifelse(m.rearrested==1,round(sample(prison_sample,1)),prison_time)
  m.is_in_prison<-say_if_in_prison(prison_time)
  prison_time<-ifelse(m.is_in_prison==1,prison_time-1,prison_time)
  tmp.months_free <- ifelse(m.rearrested == 1, 0, m.months_free)
  df[month,]<-c(m.month,m.months_free,m.is_in_prison,m.rearrested,m.released)
  
}

output <- vector("double", 60)

for(month in 1:60){
  months_free <- if_else(month == 1, 1, 1)
  rearrested <- sample(x = c(1, 0), 
                       size = 1, 
                       replace = FALSE, 
                       prob = c(survival_rates[months_free,]))
  months_free <- if_else(month == 1, 1, )
  
  output[[i]] <- if_else()
}