
library(EpiEstim)
library(readr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(readxl)
library(tidyr)
library(deSolve)


###################
# COVID Case Data #
###################

StateCases = read_csv("~/Dropbox (University of Michigan)/SPH-COVID Response/Data - MDHHS/21-11-12/Micuminc_conf_prob_onset.csv",col_types = cols())
StateCases = head(StateCases, -7)

StateIncidenceData = data.frame(tail(StateCases$Date,-1), diff(StateCases$Total) )
names(StateIncidenceData) = c("dates", "I")


####################################
# COVID Serial Interval Parameters #
####################################

# Roughly based on meta analysis: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7448781/
# But big SD because the above is really mostly estimates from China, not sure how contact patterns would change the interval

# mean_SI = 5.3
# std_SI = 4

# Based on Montana estimates: https://wwwnc.cdc.gov/eid/article/27/5/20-4663_article
mean_SI = 5.68
std_SI = 4.77


################################
# Simple fixed serial interval #
################################

simpleRt = StateIncidenceData$I/lag(StateIncidenceData$I, round(mean_SI))
# simpleRt = StateIncidenceData$I/lag(StateIncidenceData$I, 5)

# Plot daiily cases
ggplot() + 
  geom_point(aes(x = StateIncidenceData$dates, y = StateIncidenceData$I), alpha = 0.2) +
  geom_line(aes(x = StateIncidenceData$dates, y = zoo::rollmean(StateIncidenceData$I,7,na.pad = T) ), size = 1) + 
  labs(x="", y="Daily Cases in Michigan", color="") +
  theme_light(,base_size = 16)


# Plot Rt
ggplot() + 
  geom_point(aes(x = StateIncidenceData$dates, y = simpleRt), alpha = 0.2) +
  geom_line(aes(x = StateIncidenceData$dates, y = zoo::rollmean(simpleRt,7,na.pad = T) ), size = 1) + 
  geom_hline(yintercept = 1, linetype = "dashed") + 
  labs(x="", y="Rt", color="") +
  theme_light(,base_size = 16)



############
# EpiEstim #
############

knownSIconfig = list(mean_si = mean_SI, std_si = std_SI)
uncertainSIconfig = list(mean_si = mean_SI, std_mean_si = 4,
                         min_mean_si = 3, max_mean_si = 7,
                         std_si = std_SI, std_std_si = 4,
                         min_std_si = 1, max_std_si = 7)


niceRt = estimate_R(StateIncidenceData, method="parametric_si", config = make_config(knownSIconfig) )
plot(niceRt, legend = FALSE)

niceRt2 = estimate_R(StateIncidenceData, method="uncertain_si", config = make_config(uncertainSIconfig) )
plot(niceRt2, legend = FALSE)





















