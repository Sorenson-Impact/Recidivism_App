# This is a vector of the probability of recidivating given months free (i)
#values used from rpts05p0510f01.csv which has national recidivism trends
cumu_prob_recid<-c(0.0231,	0.0493,	0.0827,	0.1147,	0.1479,	0.1764,	0.2019,	0.2245,	0.2487,	0.2678,	0.2859,	0.3041,	0.3214,	0.3358,	0.3484,	0.361,	0.3723,	0.3835,	0.3928,	0.4009,	0.4092,	0.4169,	0.4248,	0.4328,	0.4403,	0.446,	0.453,	0.4588,	0.4642,	0.4694,	0.4745,	0.4794,	0.4836,	0.4878,	0.4921,	0.4966,	0.5004,	0.5037,	0.5069,	0.5096,	0.5121,	0.5149,	0.5173,	0.5194,	0.522,	0.5242,	0.5263,	0.5288,	0.5307,	0.533,	0.535,	0.5371,	0.5394,	0.541,	0.5425,	0.5443,	0.5465,	0.5479,	0.5495,	0.5511)

# survival_rates<-matrix(c(recid,1-recid),ncol=2)

cumu_prob_recid<-cumu_prob_recid*(recid_rate/.551)
#the next four lines replicate the procdure used in analyze_recidivism 
#to replicate recidivism trends based on national trends
recid_rates<-data.frame(cumu_prob_not_recid=1-cumu_prob_recid) 
recid_rates<-recid_rates%>%mutate(recid=1-cumu_prob_not_recid/lag(cumu_prob_not_recid))
recid_rates$recid[1]=1-recid_rates$cumu_prob_not_recid[1]
prob_rec<-round(recid_rates$recid,digits = 3)

survival_rates <- matrix(c(prob_rec, 1-prob_rec), ncol = 2)


# Prison time
prison_time_served <- 10
prison_sample <- rlnorm(1000, log(prison_time_served))



# If the agent is free, how many months has he or she been free
calc_months_free <- function(month, tmp.months_free){
  ifelse(month == 1, 1, tmp.months_free + 1)
}


calc_odds_of_being_rearrested <- function(months_free){
  ifelse(months_free <= 0, 0,
         sample(x = c(1, 0), 
                size = 1, 
                replace = FALSE, 
                prob = c(survival_rates[months_free,])))
}


output <- vector("double", 60)

for(month in 1:60){
  months_free <- calc_months_free(month, tmp.months_free)
  rearrested <- calc_odds_of_being_rearrested(months_free)
  tmp.months_free <- if_else(rearrested == 1, round((sample(prison_sample, 1)) * -1), months_free) 
  output[[month]] <- tmp.months_free
}






out <- vector("list", 1000)

for (sim in 1:1000) {
  output <- vector("double", 60)
  for(month in 1:60){
    months_free <- calc_months_free(month, tmp.months_free)
    rearrested <- calc_odds_of_being_rearrested(months_free)
    tmp.months_free <- if_else(rearrested == 1, round((sample(prison_sample, 1)) * -1), months_free) 
    output[[month]] <- tmp.months_free
  }
  
  out[[sim]] <- output
 
}

test <- unlist(out)
months <- rep(seq(1:60), 1000)
id <- rep(1:1000, each=60)

final <- tibble(test, months, id)




ggplot(data=final,
       aes(x=months, y=test, colour=id)) +
  geom_line()












sim_agent <- function(variables) {
  output <- vector("double", 60)
  for(month in 1:60){
    months_free <- calc_months_free(month, tmp.months_free)
    rearrested <- calc_odds_of_being_rearrested(months_free)
    tmp.months_free <- if_else(rearrested == 1, round((sample(prison_sample, 1)) * -1), months_free) 
    output[[month]] <- tmp.months_free
  }
  return(output)
}

test <- sim_agent()

walks <- replicate(1000, sim_agent())

walkst <- t(walks)
