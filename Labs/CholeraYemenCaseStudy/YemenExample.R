#### Yemen Data Fits ####

library(deSolve)
library(plotly)


Yemen<-read.csv('/Users/marisaeisenberg/Desktop/YemenCholeraOutbreak.csv')
#Make sure the dates are formatted correctly
Yemen$date<-as.Date(Yemen$Date)#, format='%m/%d/%y')
#Sort by date
Yemen<-Yemen[order(Yemen$date),]
data<-Yemen$Deaths
#Convert dates to times (first observation is time 0)
times<-as.numeric(Yemen$date-as.Date('5/22/2017', format='%m/%d/%Y'))

SIWRode <- function(t, x, params){
  S = x[1]
  I = x[2]
  W = x[3]
  D = x[4]
  
  
  bI = params[1]
  bW = params[2]
  g = params[3]
  s = params[4]
  xi = params[5]
  
  dSdt = -bI*S*I - bW*S*W
  dIdt = bI*S*I + bW*S*W - g*I - s*I
  dWdt = xi*(I-W)
  dDdt = s*I
  
  list(c(dSdt, dIdt, dWdt, dDdt))
}


params = c('bI'=0.25,'bW'=0.25,'gamma'=0.25, 's'=0.1, 'xi'=0.1, 'k'=1/10000)

x0fun = function(cases,params) {
  x0 = c(0.99, 0.01, 0, data[1]*params[6])
  names(x0) = c('S0','I0','W0')
  x0}

yfun = function(odeSim, params){odeSim[,5]/params[6]} 

xinit = ode(x0fun(cases,params), times, SIWRode, params, method='ode45')
plot(times, yfun(xinit,params), type='l')
points(times, data)

SIRML=function(params,times,data){
  params = abs(params)
  # Simulate model
  xcurr = ode(x0fun(data,params), times, SIWRode, params, method='ode45')
  
  # Measurement equation
  y = yfun(xcurr,params)
  
  # Negative Log Likelihood (NLL)
  NLL =  sum(y) - sum(data*log(y)) # Poisson ML
  # note this is a slightly shortened version--there's an additive constant term missing but it 
  # makes calculation faster and won't alter the threshold. Alternatively, can do:
  # NLL = -sum(log(dpois(round(data),round(y)))) # the round is b/c Poisson is for (integer) count data
  # this can also barf if data and y are too far apart because the dpois will be ~0, which makes the log angry
  
  # ML using normally distributed measurement error (least squares)
  # NLL = -sum(log(dnorm(data,y,0.1*mean(data)))) # example WLS assuming sigma = 0.1*mean(data)
  # NLL = sum((y - data)^2)  # alternatively can do OLS but note this will mess with the thresholds 
  #                             for the profile! This version of OLS is off by a scaling factor from
  #                             actual LL units.
  
  # return(NLL) 
}

res = optim(params,fn=SIRML,times=times,data=data)#,method='Nelder-Mead')
paramests = res$par

xest = ode(x0fun(data,paramests), times, SIWRode, paramests, method='ode45')
plot(times, yfun(xest,paramests), type='l', xlab='Time (days)', ylab='Cumulative Deaths')
points(times,data)




# Make a contour plot!
betaWrange = seq(1e-2,6e-2,5e-3)
xirange = seq(1e-2,6e-2,5e-3)
likevals = matrix(NA,nrow=length(betaWrange),ncol=length(xirange))
tempparams = paramests

# Go through each point on the contour plot and calculate the likelihood value at those coordinates
for (i in 1:length(betaWrange)){
  for(j in 1:length(xirange)){
    tempparams[2] = betaWrange[i]
    tempparams[5] = betaWrange[j]
    likevals[i,j] = SIRML(tempparams,times,data)
  }
}

plot_ly(x = betaWrange, y = xirange, z = likevals, type = "contour") %>%
  layout(xaxis = list(title = "betaW"), yaxis = list(title = "xi")) %>%
  colorbar(title = "-LL")
