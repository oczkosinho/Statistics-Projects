load("C:/Users/kponu/Desktop/data/gospodarstwa.rda")
load("C:/Users/kponu/Desktop/data/gospodarstwaDICT.rda")
load("C:/Users/kponu/Desktop/data/osoby.rda")
load("C:/Users/kponu/Desktop/data/osobyDICT.rda")

install.packages("weights")
library(weights)
sdpop<-function(x,y){sqrt(mean((x-wtd.mean(x,y))^2))}

#Exercise 1
#Chosen data: lowest net income of a household which allows it to operate 

#extraction of the needed data
income<-gospodarstwa[,c("waga_gd_2000","ae5")]
income<-na.omit(income)
attach(income)

#histogram
wtd.hist(ae5, weight = waga_gd_2000, breaks = 100, xlim = c(0,7000),
         main = paste("Lowest net income of a household which allows it to operate"), xlab = "Net income")

#mean
income.mean<-wtd.mean(ae5, waga_gd_2000)
#sdpop
income.std<-sdpop(ae5, waga_gd_2000)
#quartiles
income.quartiles<-c(wtd.quantile(ae5, waga_gd_2000,0.25),wtd.quantile(ae5, waga_gd_2000,0.50),
                    wtd.quantile(ae5, waga_gd_2000,0.75))
#5th and 95th percentile
income.5th<-wtd.quantile(ae5, waga_gd_2000, 0.05)
income.95th<-wtd.quantile(ae5, waga_gd_2000, 0.95)
#kurtosis
my_kurtosis<-wtd.mean((ae5-income.mean)^4, weights = waga_gd_2000)/(income.std)^4-3
#skewness
my_skewness<-wtd.mean((ae5-income.mean)^3, weights = waga_gd_2000)/(income.std)^3
detach(income)

#Ex2
#Chosen data: Area of household vs financial help recived by a household
area<-gospodarstwa[,c("waga_gd_2000","ah8","ah21")]
area<-na.omit(area)
attach(area)
#scatter plot
plot(ah21, ah8, ylab = "Area of a household", xlab = "Received financial help")
#SD line
area.mean<-wtd.mean(ah8,waga_gd_2000)
help.mean<-wtd.mean(ah21,waga_gd_2000)
area<-sdpop(ah8, waga_gd_2000)
help<-sdpop(ah21,waga_gd_2000)
abline(area.mean - help.mean * area / help, area /help, col='red')
#correlation
area.cor<-wtd.cor(ah8,ah21,waga_gd_2000)

#Ex3
plot(ah21, ah8, ylab = "Area of a household", xlab = "Received financial help")
#regression models
lm1<- lm(ah21~ah8, weights = waga_gd_2000)
a1<-lm1$coefficients[1]
b1<-lm2$coefficients[2]
lm2<- lm(ah8~ah21, weights = waga_gd_2000)
abline(a=-a1/b1,b=1/b1, col='blue')
abline(lm2, col='green')
#chart of resuiduals
plot(ah21, resid(lm2), xlab = "Received financial help", ylab = "Residuals")
summary(lm2)
summary(lm1)
resid(lm2)
detach(area)

#Ex4
#chosen data: Did you participate in the therapy
span<-c(2000,2003,2005,2007,2009,2011)
psych<-osoby[,c("waga_2000_ind","waga_2003_ind","waga_2005_ind","waga_2007_ind",
                  "waga_2009_ind","waga_2011_ind", "ap84","bp71","cp78","dp75","ep70","fp72")]
psych<-na.omit(psych)
attach(psych)
y0<-wpct(ap84, waga_2000_ind)[1]
y1<-wpct(bp71, waga_2003_ind)[1]
y2<-wpct(cp78, waga_2005_ind)[1]
y3<-wpct(dp75, waga_2007_ind)[1]
y4<-wpct(ep70, waga_2009_ind)[1]
y5<-wpct(fp72, waga_2011_ind)[1]
yes<-c(y0, y1, y2, y3, y4, y5)
plot(span, yes, ylab="Percentage of people who visited psychologist")
lines(span,yes)

#CAGR
mult<-(yes[length(yes)]/yes[1])^(1/11)
mult
lines(c(2000:2011), yes[1]*mult^(0:11),col="red")
#fixed index
fixed<-yes/yes[1]*100
plot(span[1:length(yes)], fixed, xlab = "years", ylab = "fixed index")
lines(span[1:length(yes)], fixed)
#chain index
chain<-yes[2:length(yes)]/yes[1:length(yes)-1]*100
plot(span[2:length(yes)],chain, xlab = "span")
lines(span[2:length(yes)],chain)
detach(psych)
