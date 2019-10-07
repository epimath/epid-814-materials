# Making error bar plot
# Author: Alicia Kraay
# Date: 10-4-17

require(ggplot2)

#Defining function 'multiplot' for multiple panel figures
#Code taken from online
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#Manually inputting data frame
df<-data.frame(Age=c(rep('Age <2', 3), rep(' Age ≥2', 3)), Doses=rep(c('Zero', 'One', 'Two'), 2), 
               PointEstimate=c(0, -0.2, -0.8, -1.2, -1.5, -1.0001),
               SE=c(0, 0.2, 0.13, 0.37, 0.76, 0.15))
df$group<-paste(df$Age, df$Doses, sep=', ')
head(df)
df$Doses<-as.factor(df$Doses)
print(levels(df$Doses))
df$Doses<-factor(df$Doses,levels(df$Doses)[c(3, 1, 2)])
df$group<-factor(df$group,levels(df$group)[c(6, 4, 5, 3, 1, 2)])

p1<-ggplot(subset(df, df$Age=='Age <2'), aes(x=Doses, y=exp(PointEstimate)))+geom_point()+
  geom_errorbar(aes(ymin=exp(PointEstimate-1.96*SE),ymax=exp(PointEstimate+1.96*SE)), width=0.2)+theme_classic()+
  labs(y='IRR (95% CI)', x='Doses')+ylim(0, 1.3)+geom_abline(slope=0, intercept=1, linetype=2)+ggtitle('Age <2')
p1

p2<-ggplot(subset(df, df$Age==' Age ≥2'), aes(x=Doses, y=exp(PointEstimate)))+geom_point()+
  geom_errorbar(aes(ymin=exp(PointEstimate-1.96*SE),ymax=exp(PointEstimate+1.96*SE)), width=0.2)+theme_classic()+
  labs(y='IRR (95% CI)', x='Doses')+geom_abline(slope=0, intercept=1, linetype=2)+ggtitle('Age ≥2')+ylim(0, 1.3)
p2

multiplot(p1, p2, cols=2)