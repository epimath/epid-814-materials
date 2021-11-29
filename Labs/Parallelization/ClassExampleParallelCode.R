# Example for Parallel Processing in R
# MCE 10-30-2018

library(foreach)
library(doParallel)
library(lhs)
library(tictoc)

#### Setup parallel backend ####
cores = detectCores()

mycluster = makeCluster(cores-2)
# leaving 2 cores out so I can run other things and not overwhelm the 
# computer, you could leave just one out and use more cores for this

registerDoParallel(mycluster)


#### Make up some example to run ####

# Suppose we have 10 parameters and we're going to draw 100 parameter sets
# and run our 'model' for each parameter set
nsamples = 1000000
nparams = 10

# Generate uniform random samples as our parameter sets
samples = randomLHS(nsamples, nparams) # generates nsamples rows of parameter sets


#### Run the parallel loop ####
tic()
results = foreach(i=1:nsamples) %dopar% {
  
  # Suppose our 'model'/process spits out the i, mean, median, and product as output
  curresults = c(i,mean(samples[i,]),median(samples[i,]),prod(samples[i,]))
  
  # note that like a function in R, whatever you evaluate last is treated 
  # like the output, so if you do a bunch of calculations, be sure to 
  # spit out your curresults again at the end.
}
toc()



#### Try it out non-parallel to compare ####
# %dopar% to %do% (or convert to a regular for loop)

tic()
results = foreach(i=1:nsamples) %do% {
  
  # Suppose our 'model'/process spits out the i, mean, median, and product as output
  curresults = c(i,mean(samples[i,]),median(samples[i,]),prod(samples[i,]))
  
  # note that like a function in R, whatever you evaluate last is treated 
  # like the output, so if you do a bunch of calculations, be sure to 
  # spit out your curresults again at the end.
}
toc()


#### Plot things! ####

# matplot(1:4, t(results)) # derpy plot of all the indices, means, medians, 
# and products from all the samples


#### Stop the cluster once done! ####
stopCluster(mycluster)
