library(EpiEstim)
library(readr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(readxl)
library(tidyr)
library(deSolve)

# Variations to try as a class: 
# - endemic disease! (including oscillations on the way there maybe)
# - reporting rate
# - asymptomatic infections
# - make a version that has sustained oscillations & more interesting dynamics
# - delay if measuring deaths without accounting for it
# - two patches with spatial patterning
# - Covid version with realistic params




###### SIR Model Example ######

# Model simulation
SIR.ode = function(t, x, params){
  S = x[1]
  I = x[2]
  R = x[3]
  beta = params[1]
  gamma = params[2]
  
  dSdt = - beta*S*I   # Susceptible
  dIdt = beta*S*I - gamma*I  # Infectious
  dRdt = gamma*I # Recovered
  dCdt = beta*S*I  # Cumulative infected (so we can calculate daily cases by differencing the cumulative cases each day)
  
  list(c(dSdt, dIdt, dRdt, dCdt))
}


IC = c(9999, 1, 0, 0)
# IC = c(0.9999, 0.0001, 0, 0)
params = c(1/10000, 0.2)


initSim = ode(IC, 0:50, SIR.ode, params)
matplot(initSim[,1], initSim[,2:5])

dailyCases = c(NA, diff(initSim[,5]))


# True Rt
SIR.Rt = function(params,S){params[1]*S/params[2]}
SIR.Rt(params,sum(IC))

plot(initSim[,1], SIR.Rt(params, initSim[,2]), xlab = "Days", ylab = "Estimated Rt")

# Final Size Equation

# To solve the final size equation, we need a function which is zero when R_inf = 1 - e^(-R0 * R_inf) 
# (i.e. move it all to one side of the equation)
# note it has to be a function of one variable for it to work with uniroot
Rinf = tail(initSim,1)[5]
R0FinalSize = function(R0){1 - exp(-R0*(Rinf/sum(IC))) - (Rinf/sum(IC))}

R0FinalSize(5)
uniroot(R0FinalSize, interval = c(0,100))


# Simple Rt

# Note the actual generation time will change over time (because the rate of contact with infected people will change over time)
# The 'intrinsic' generation time should be 1/gamma for SIR, though this won't work so well for the basic SIR (discuss why)
# Compare the intrinsic generation time for the SIR with and without loss of immunity (endemic disease)

simpleRt = dailyCases/lag(dailyCases, round(1/params[2]))
simpleRt = dailyCases/lag(dailyCases, 2)
simpleRt = c( (dailyCases/lag(dailyCases, 2))[1:15], (dailyCases/lag(dailyCases, 5))[16:20], (dailyCases/lag(dailyCases, 10))[21:51] )

plot(initSim[,1], SIR.Rt(params, initSim[,2]), xlab = "Days", ylab = "Estimated Rt")
lines(initSim[,1], simpleRt, col=2)


# EpiEstim

knownSIconfig = list(mean_si = 3.8, std_si = 3.8)
data = tail(data.frame(dates = initSim[,1], I = dailyCases), -1)
niceRt = estimate_R(data, method="parametric_si", config = make_config(knownSIconfig) )
plot(niceRt, legend = FALSE)

plot(tail(niceRt$dates,-7),niceRt$R$`Mean(R)`, xlab = "Days", ylab = "Estimated Rt")
lines(initSim[,1], SIR.Rt(params, initSim[,2]))
lines(initSim[,1], simpleRt, lty=2)










