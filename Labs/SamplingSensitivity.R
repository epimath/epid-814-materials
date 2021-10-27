library(lhs)
library(SobolSequence)
library(deSolve)
library(scatterplot3d)
library(plotly)
library(ppcor)

set.seed(2)
numsamples = 1000
numparams = 2
paramsampleRand = matrix(runif(numsamples*numparams),ncol=numparams)
paramsampleLHS = randomLHS(numsamples,numparams)
paramsampleSobol = sobolSequence.points(dimR=numparams,count=numsamples)

plot(paramsampleRand[,1], paramsampleRand[,2])
plot(paramsampleLHS[,1], paramsampleLHS[,2])
plot(paramsampleSobol[,1], paramsampleSobol[,2])

params = data.frame("param" = c("Beta", "Gamma"), "lower" = c(0.0001,0.1), "upper" = c(0.001, 1), "default" = c(0.0008, 0.5))


SIRode = function(t, x, params){
  S = x[1]
  I = x[2]
  R = x[3]
  
  beta = params[1]
  gamma = params[2]
  
  dSdt = -beta*S*I
  dIdt = beta*S*I - gamma*I
  dRdt = gamma*I
  
  output = c(dSdt, dIdt, dRdt)
  list(output)
}

initCond = c(999,1,0)

# Simulate the model using our initial paramter guesses
initSim = ode(initCond, 0:100, SIRode, params$default, method='ode45')
matplot(initSim[,1], initSim[,2:4])

paramsample = paramsampleLHS

peakprev = numeric(numsamples)
paramsample.scaled = matrix(NA,numsamples, numparams)

for(i in 1:numsamples){
  print(i)
  
  # We need to re-scale the parameters to be in the upper and lower bounds above
  paramsample.scaled[i,] = params$lower + (params$upper-params$lower)*paramsample[i,]
  
  # Run with estimated parameters
  xtemp = ode(y=initCond,0:100,SIRode,paramsample.scaled[i,])#,method='lsode',atol=1e-15)
  
  peakprev[i] = max(xtemp[,3])
}

plot(paramsample.scaled[,1], peakprev)
plot(paramsample.scaled[,2], peakprev)


# Non-interactive version
# scatterplot3d(paramsample.scaled[,1],paramsample.scaled[,2],peakprev, pch = 16, color="steelblue",type="h",grid=TRUE, box=TRUE, angle=150)

# Interactive version (easier to see the shape)
plotdata = data.frame("beta" = paramsample.scaled[,1], "gamma" = paramsample.scaled[,2], "peakprev" = peakprev)
plot = plot_ly(plotdata, x = ~beta, y = ~gamma, z = ~peakprev, marker = list(color = ~peakprev, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>% 
  add_markers() 

plot


###### Calculate some correlation coefficients to summarize the above ######

# Compute a standard (Pearson) correlation coefficient
cor(plotdata$beta, plotdata$peakprev, method = "pearson") #method = c("pearson", "kendall", "spearman")
cor(plotdata$gamma, plotdata$peakprev, method = "pearson") #method = c("pearson", "kendall", "spearman")

cor.test(plotdata$beta, plotdata$peakprev, method = "pearson") #method = c("pearson", "kendall", "spearman")
cor.test(plotdata$gamma, plotdata$peakprev, method = "pearson") #method = c("pearson", "kendall", "spearman")

# Compute a rank-based (Spearman) correlation coefficient
cor(plotdata$beta, plotdata$peakprev, method = "spearman") #method = c("pearson", "kendall", "spearman")
cor(plotdata$gamma, plotdata$peakprev, method = "spearman") #method = c("pearson", "kendall", "spearman")

cor.test(plotdata$beta, plotdata$peakprev, method = "spearman") #method = c("pearson", "kendall", "spearman")
cor.test(plotdata$gamma, plotdata$peakprev, method = "spearman") #method = c("pearson", "kendall", "spearman")
# note the ranking is a little problematic for spearman because you can make the same 
# peak prevalance a lot of different ways (e.g. could be a small fast epidemic or a long slow burn)

# Maybe easier to just have it make the whole correlation matrix, even though we don't really need all the entries
cor(plotdata, method = "pearson")
cor(plotdata, method = "spearman")


# Partial correlation coefficients using the ppcor package
pcor(plotdata, method = "pearson")
pcor(plotdata, method = "spearman") # This one is the PRCC! (partial rank (i.e. spearman) correlation coefficient)




##### Let's set up an example that has more parameters #####

SIRode = function(t, x, params){
  S = x[1]
  I = x[2]
  R = x[3]
  
  beta = params[1]
  gamma = params[2]
  mu = params[3]
  N = 1000
  
  dSdt = mu*N -beta*S*I - mu*S
  dIdt = beta*S*I - gamma*I - mu*I
  dRdt = gamma*I - mu*R
  dCdt = beta*S*I # so we can track cumulative cases
  
  output = c(dSdt, dIdt, dRdt, dCdt)
  list(output)
}

# 4 parameters - the model parameters above, plus a reporting fraction k
params = data.frame("param" = c("Beta", "Gamma", "Mu", "k"), 
                    "lower" = c(0.0001,0.1, 0, 0), 
                    "upper" = c(0.001, 1, 1/50, 1), 
                    "default" = c(0.0008, 0.5, 1/80, 0.2))

# Initial conditions
initCond = c(999,1,0, 0)

numsamples = 1000
numparams = 4
paramsampleRand = matrix(runif(numsamples*numparams),ncol=numparams)
paramsampleLHS = randomLHS(numsamples,numparams)
paramsampleSobol = sobolSequence.points(dimR=numparams,count=numsamples)


# Simulate the model using our initial paramter guesses
initSim = ode(initCond, 0:500, SIRode, params$default, method='ode45')
matplot(initSim[,1], initSim[,2:4])

paramsample = paramsampleLHS

outputsample = numeric(numsamples)
paramsample.scaled = matrix(NA,numsamples, numparams)

for(i in 1:numsamples){
  print(i)
  
  # We need to re-scale the parameters to be in the upper and lower bounds above
  paramsample.scaled[i,] = params$lower + (params$upper-params$lower)*paramsample[i,]
  
  # Run with estimated parameters
  xtemp = ode(y=initCond,0:500,SIRode,paramsample.scaled[i,])#,method='lsode',atol=1e-15)
  
  outputsample[i] = xtemp[500,5]*paramsample.scaled[i,4]
}

plot(paramsample.scaled[,1], outputsample)
plot(paramsample.scaled[,2], outputsample)
plot(paramsample.scaled[,3], outputsample)
plot(paramsample.scaled[,4], outputsample)

# 3D plot
plotdata = data.frame("beta" = paramsample.scaled[,1], "gamma" = paramsample.scaled[,2], "mu" = paramsample.scaled[,3], "k" = paramsample.scaled[,4], "output" = outputsample)
plot = plot_ly(plotdata, x = ~beta, y = ~gamma, z = ~output, marker = list(color = ~output, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>% 
  add_markers() 
# plot = plot_ly(plotdata, x = ~gamma, y = ~mu, z = ~output, marker = list(color = ~output, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>% 
#   add_markers() 
# plot = plot_ly(plotdata, x = ~beta, y = ~k, z = ~output, marker = list(color = ~output, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>% 
#   add_markers() 

# Just to illustrate the parameters themselves are randomly sampled, plus it shows the 3d interactions
plot = plot_ly(plotdata, x = ~beta, y = ~gamma, z = ~mu, marker = list(color = ~output, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>% 
  add_markers() 

plot


# Correlation coefficients
cor(plotdata, method = "pearson")
cor(plotdata, method = "spearman")


# Partial correlation coefficients using the ppcor package
pcor(plotdata, method = "pearson")
pcor(plotdata, method = "spearman") # This one is the PRCC! (partial rank (i.e. spearman) correlation coefficient)


