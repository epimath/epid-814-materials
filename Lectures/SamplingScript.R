library(lhs)
library(SobolSequence)
library(deSolve)
library(scatterplot3d)
library(plotly)

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


SIRode <- function(t, x, params){
  S <- x[1]
  I <- x[2]
  R <- x[3]
  
  B <- params[1]
  g <- params[2]
  
  dS <- -B*S*I
  dI <- B*S*I - g*I
  dR <- g*I
  
  output <- c(dS, dI, dR)
  list(output)
}

initCond <- c(999,1,0)

# Simulate the model using our initial paramter guesses
initSim <- ode(initCond, 0:100, SIRode, params$default, method='ode45')
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




