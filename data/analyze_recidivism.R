# Created By Daniel Hadley Wed Nov 30 16:22:39 MST 2016 #
setwd("/Users/danielhadley/Github/Recidivism_App/data/")
#



recidivism <- read.csv("./rprts05p0510f01.csv", 
                           stringsAsFactors = F) %>% 
  filter(X != "",
         X != "All releases") %>%
  mutate(return_to_prison = as.numeric(X.2)) %>% 
  mutate(cumulative_percent = return_to_prison / 100,
         cumulative_prob_did_not = 1 - cumulative_percent,
         prob_recidivate = 1 - cumulative_prob_did_not / lag(cumulative_prob_did_not)) %>% filter(return_to_prison != 0)


vec <- round(recidivism$prob_recidivate, digits = 3)
paste(vec, collapse=", ") 
