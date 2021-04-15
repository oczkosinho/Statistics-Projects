##install.packages("rmarkdown")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("tidyverse")
install.packages("reshape2")
library(ggplot2)
library(ggpubr)
library(reshape2)
install.packages("tinytex")

p9 <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = dt, args = list(df = 1))
p9
tv<-c(.100, .050, .025, .010, .005)


df<-c(1:30,40,60,120,Inf)
a<-qt(1-.1, df)
b<-qt(1-.05, df)
c<-qt(1-.025, df)
d<-qt(1-.010, df)
e<-qt(1-.005, df)
xd<-data.frame(DF=df,t.1 =a, t.05 =b, t.025 =c, t.010 =d, t.005 =e)


t.values<-seq(-4,4,0.1)
t.frame = data.frame(t.values, y=dnorm(t.values))

t.frame.melt <- melt(t.frame,id="t.values")

colnames(t.frame.melt)= c("t","df","density")

x.axis.values <- seq(-4,4,2)

g<- ggplot(t.frame, aes(x=t.values,y=density)) +
  geom_line() + geom_vline(aes(xintercept=2), linetype="solid") + 
  geom_rect(aes(xmin = 2, xmax = 4, ymin = 0, ymax = 0.0539909665),fill = "pink", alpha = 0.01)+
  geom_area(aes(x=t.values,y = density),fill="lightblue")
g + theme_classic() 


g<- ggplot(t.frame, aes(x=t.values,y=density)) +
  geom_line()
g + theme_classic() + geom_area(aes(x=t.values,y = density),fill="lightblue") + 
  geom_ribbon(aes(x=t.values,ymin=0,ymax = 0.0539909665,fill="green"))






p<-qplot(x=t.frame$t.values,y=t.frame$density,geom="line") 
p

#First subst the data and add the coordinates to make it shade to y = 0
shade <- rbind(c(1,0), subset(t.frame, t.values >= 1), c(t.frame[nrow(t.frame), "t.values"], 0))    

#Then use this new data.frame with geom_polygon
p + geom_segment(aes(x=1,y=0,xend=1,yend=ytop)) +
  geom_polygon(data = shade, aes(x=t.frame$t.values,y=t.frame$density))







ytop=0.2419707
g<- ggplot(t.frame, aes(x=t.values,y=density)) +
  geom_line()
g + theme_classic() + geom_segment(aes(x=1,y=0,xend=1,yend=ytop)) + 
  geom_polygon(data = shade, aes(x,y=density))

#First subst the data and add the coordinates to make it shade to y = 0
shade <- rbind(c(1,0), subset(t.frame, t.values > 1), c(t.frame[nrow(t.frame), "t.values"], 0))

#Then use this new data.frame with geom_polygon
g + geom_segment(aes(x=2,y=0,xend=4,yend=0.0539909665)) +
  geom_polygon(data = shade, aes(x, y))



t.values<-seq(-4,4,0.1)
txd<-seq(2,4,0.1)
t.frame = data.frame(t.values, density=dnorm(t.values))
txd.frame = data.frame(txd,dens=dnorm(txd))
t.frame.melt <- melt(t.frame,id="t.values")
txd.frame.melt <- melt(txd.frame, id="txd")
colnames(t.frame.melt)= c("t","df","density")

x.axis.values <- seq(-4,4,2)



g<- ggplot(t.frame, aes(x=t.values,y=density)) +
  geom_line()
g + theme_classic() + geom_area(aes(x=txd,y = dens),fill="lightblue")


g<- ggplot(t.frame, aes(x=t.values,y=density)) +
  geom_line() + geom_vline(aes(xintercept=2), linetype="solid")
g + theme_classic() 

rt<- ggplot(txd.frame, aes(x=txd,y=dens)) +
  geom_line() + geom_area(aes(x=txd,y = dens),fill="lightblue")
rt + theme_classic() 


    
g<- ggplot(t.frame, aes(x=t.values,y=density)) +
  geom_line()
g + theme_classic() + geom_area(aes(x=t.values,y = density),fill="lightblue")