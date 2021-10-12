library(BayesianTools)
library(deSolve)
library(ggplot2)

set.seed(7)

data = rnorm(50,0,30)
hist(data)

loglike = function(par){
  sum(dnorm(data, par, 30, log = T))
}

setup = createBayesianSetup(likelihood = loglike, lower = -20, upper = 20)
settings = list(iterations = 10000, nrChains = 5, message=F)

res = runMCMC(bayesianSetup = setup, settings = settings)
plot(res)

marginalPlot(res)

settings = list(iterations = 10000, adapt = F, DRlevels = 1, gibbsProbabilities = NULL, temperingFunction = NULL, optimize = F,  message = FALSE)
res = runMCMC(bayesianSetup = setup, sampler = "Metropolis", settings = settings)
plot(res) 

# 2 parameter version!
loglike = function(par){
  sum(dnorm(data, par[1] + par[2], 30, log = T))
}

setup = createBayesianSetup(likelihood = loglike, lower = c(-20,0), upper = c(20,100))
settings = list(iterations = 10000, nrChains = 5, message=F)

res = runMCMC(bayesianSetup = setup, settings = settings)
plot(res)
summary(res)
marginalPlot(res)





#### SI model ####
SIode = function(t, x, params){
  S = x[1]
  I = x[2]
  
  b = params[1]
  g = params[2]
  
  dS = -b*S*I + g*I
  dI = b*S*I - g*I
  
  list(c(dS, dI))
}

#### Measurement equation ####
yfun = function(x,params){x[,3]*10000*params[3]}

times = seq(0,100,1)
trueparams = c(0.25,0.1,0.15)
xsimdata = ode(c(0.99,0.01), times, SIode, trueparams, method='ode45')

data = rnorm(length(times),yfun(xsimdata,trueparams),0.2*yfun(xsimdata,trueparams))
plot(times,data)

loglike = function(par){
  xtemp = ode(c(0.99,0.01), times, SIode, par, method='ode45')
  return(sum(dnorm(data, yfun(xtemp,par), 0.2*yfun(xtemp,par), log = T)))
}

setup = createBayesianSetup(likelihood = loglike, lower = c(0,0,0), upper = c(1,1,1))
settings = list(iterations = 10000, message=F)

res = runMCMC(bayesianSetup = setup, settings = settings)
summary(res)
plot(res)

marginalPlot(res, start=1000)
correlationPlot(res)

mapests = MAP(res) # MAP generates a list with the MAP parameter estimates, and the associated posterior, likelihood, and prior.

xmap = ode(c(0.99,0.01), times, SIode, mapests$parametersMAP)
plot(times,data)
lines(xmap[,1],yfun(xmap,mapests$parametersMAP),type='l')

# Pull a sample of 1000 parameter sets from our Markov chain
paramsample = getSample(res, parametersOnly = F, numSamples = 1000)

ysample = matrix(0,length(times),length(paramsample[,1]))
for(i in 1:length(paramsample[,1])){
  xtemp = ode(c(0.99,0.01), times, SIode, paramsample[i,1:3])
  ysample[,i] = yfun(xtemp,paramsample[i,1:3])
}

# Now let's plot!
matplot(times,ysample,type='l',col="grey") # sample trajectories
points(times,data) # data
lines(xmap[,1],yfun(xmap,mapests$parametersMAP),type='l',col='red') # MAP estimate

quantiles = apply(ysample, 1, quantile, probs=c(0.0275,0.25,0.75, 0.975))

ggplot() + 
  geom_point(aes(x = times, y = data)) + 
  geom_line(aes(x = xmap[,1],y = yfun(xmap,mapests$parametersMAP))) + 
  geom_ribbon(aes(x = xmap[,1], ymin = quantiles[1,], ymax = quantiles[4,]), alpha = 0.2) + 
  geom_ribbon(aes(x = xmap[,1], ymin = quantiles[2,], ymax = quantiles[3,]), alpha = 0.2) + 
  theme_classic(base_size = 15)


newprior = createTruncatedNormalPrior(mean = rep(0.5,3), sd = rep(5,3), lower = rep(0,3), upper = c(2,2,1))
bayesianSetup = createBayesianSetup(likelihood = loglike, prior = newprior)







