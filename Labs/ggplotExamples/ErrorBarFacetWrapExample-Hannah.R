# Error bars with facet wrap
# Author: Hannah Maier
# Date: 10-5-17

library(ggplot2)


data<-data.frame(Exposure=c(rep('Kids',2),rep('Teens',2)),
                 Outcome=rep(c('Disease','Very Bad Disease'),2),
                 IRR=c(.7,.5,1,3),
                 L=c(.5,.4,.5,2.1),
                 U=c(.9,.6,2,4))
data


plot<-ggplot(data,
             aes(x=Outcome,y=IRR,  ymin=L, ymax=U  )) +  # aesthetics    
  
  geom_pointrange(size=0.8, position=position_dodge(width = .5))+ # for error bars
  geom_hline(aes(yintercept = 1)) +  # horizontal line
  scale_y_log10(breaks=c(.5,1,2,4)) +  # log scale
  
  labs(x=NULL, y="IRR (95% CI) log scale",  caption="very nonsense data")  +
  ggtitle("Is mysterious EXPOSURE associated \nwith disease in kids and teens?")+
  
  facet_wrap(~ Exposure )
plot



# change theme
plot +  theme_classic(base_size = 14) +
  theme( strip.background = element_blank(), # remove box around facet wraps
         strip.text = element_text(size=20),  # facet text bigger 
         plot.title = element_text(hjust = 0.5) # centers title
  ) 