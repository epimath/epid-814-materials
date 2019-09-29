#Pull a sample of 1000 parameter sets from Markov chain
paramsample = getSample(res, parametersOnly = F, numSamples = 1000)
paramsample.dat<-as.data.frame(paramsample)
quantile(paramsample.dat$Llikelihod, probs=c(0.025, 0.975))
#Range: -609.0366 - -587.015
Best95Percent<-paramsample.dat[(which(paramsample.dat$Llikelihod>= -609.0366 & paramsample.dat$Llikelihod <=-587.015)),]

ysample=matrix(0, length(times), length(paramsample.dat[,1]))
for(i in 1:length(paramsample.dat[,1])){
  xtemp = ode(c(0.99,0.01), times, SIode, paramsample[i,1:3])
  ysample[,i] = yfun(xtemp,paramsample[i,1:3])
}

#Need to summarize to calculate the median by day
day.median<-apply(ysample, 1, median)
dat.min<-apply(ysample, 1, min)
dat.max<-apply(ysample, 1, max)
time<-seq(from=1, to=101, by=1)
summaries<-as.data.frame(cbind(time, day.median, dat.min, dat.max))
ggplot(data=summaries, aes(x=time, y=day.median))+geom_line()+
  geom_line(aes(x=time, y=dat.min), linetype=2)+geom_line(aes(x=time, y=dat.max), linetype=2)

#can use this to do the ribbon
#geom_ribbon(aes(min=dat.min, max=dat.max, fill="grey70"))