#PREPARATION###########################
install.packages("devtools")
library(devtools)
install_github("pbiecek/Diagnoza") 
library(Diagnoza)
data("gospodarstwa")
data("gospodarstwaDict")
data("osoby")
data("osobyDict")

View(colnames(gospodarstwa))
View(gospodarstwaDict)
View(colnames(osoby))
View(osobyDict)

install.packages("weights")
library(weights)


#First Task############################

tmp<-osoby[,c('waga_2015_ind', 'hp114')]
tmp<-na.omit(tmp)
library(weights)
wpct(tmp$hp114, tmp$waga_2015_ind)[1]


wtd.hist(tmp$hp114 ,weight = tmp$waga_2015_ind,xlab="Godziny pracy w tyg")


wtd.mean(tmp$hp114 ,weight = tmp$waga_2015_ind)


sdpop<-function(x){sqrt(wtd.mean((x-wtd.mean(x))^2))}
sdpop(tmp$hp114)


my_skewness<-function(x){
  wtd.mean((x-wtd.mean(x))^3)/(sdpop(x))^3}

my_skewness(tmp$hp114)

my_kurtosis<-function(x){
  wtd.mean((x-wtd.mean(x))^4)/(sdpop(x))^4-3}
my_kurtosis(tmp$hp114)

wtd.quantile(tmp$hp114,weight = tmp$waga_2015_ind, 0.50)
wtd.quantile(tmp$hp114,weight = tmp$waga_2015_ind, 0.25)
wtd.quantile(tmp$hp114,weight = tmp$waga_2015_ind, 0.75)
wtd.quantile(tmp$hp114,weight = tmp$waga_2015_ind, 0.05)
wtd.quantile(tmp$hp114,weight = tmp$waga_2015_ind, 0.95)


#Second Task###########################
tmp2<-osoby[,c('f2009','waga_2009_ind', 'ep125', 'ep39')]
tmp2<-na.omit(tmp2)
library(weights)
wpct(tmp2$ep125, tmp2$waga_2009_ind,)[1]
wpct(tmp2$ep39, tmp2$waga_2009_ind,)[1]
library(ggplot2)


plocik<-ggplot(tmp2, aes(x=tmp2$ep39,y=tmp2$ep125,size=waga_2009_ind)) + geom_point() + 
  labs(x="Nabożeństwa kościelne",y="Godziny w internecie",size="Weight") + 
  geom_abline(intercept = wtd.mean(tmp2$ep39,weight=tmp2$waga_2009_ind )-wtd.mean(tmp2$ep125,weight=tmp2$waga_2009_ind)*
  sdpop(tmp2$ep39)/sdpop(tmp2$ep125), slope = sdpop(tmp2$ep39)/sdpop(tmp2$ep125))
plocik

wtd.cor(tmp2$ep125,tmp2$ep39, weight = tmp$waga_2009_ind )

#Third task###########################

my_lm<-lm(tmp2$ep39~tmp2$ep125,weights=tmp2$waga_2009_ind)
a<-my_lm$coefficients[1]
b<-my_lm$coefficients[2]
plocik2<-ggplot(tmp2, aes(x=tmp2$ep39,y=tmp2$ep125,size=waga_2009_ind))+ geom_point() +
  scale_fill_continuous(low = "plum1", high = "purple4")+
  labs(x = "Nabożeństwa kościelne", y ="Godziny w internecie", size="Weight" ) + 
  stat_smooth(method = "lm", col = "red")+
  geom_abline(intercept=-a/b, slope=1/b, col='red')
plocik2
summary(lm(tmp2$ep39~tmp2$ep125))
summary(-a/b,1/b)
summary(lm(tmp2$ep125~tmp2$ep39))
my_lm2<-lm(tmp2$ep125~tmp2$ep39,weights=tmp2$waga_2009_ind)
residual<-ggplot(tmp2, aes(x=tmp2$ep39,y=resid(my_lm2), size=tmp2$waga_2009_ind))+ geom_point()+
  ggtitle("Residuals") +
  labs(x = "Nabożeństwa kościelne", y ="Residual" , size="Weight")
residual

#Fourth task########################

alko2000<-osoby[,c('waga_2000_ind', 'ap85')]
alko2000<-na.omit(alko2000)
alko2003<-osoby[,c('waga_2003_ind', 'bp72')]
alko2003<-na.omit(alko2003)
alko2005<-osoby[,c('waga_2005_ind', 'cp79')]
alko2005<-na.omit(alko2005)
alko2007<-osoby[,c('waga_2007_ind', 'dp76')]
alko2007<-na.omit(alko2007)
alko2009<-osoby[,c('waga_2009_ind', 'ep71')]
alko2009<-na.omit(alko2009)
alko2011<-osoby[,c('waga_2011_ind', 'gp72')]
alko2011<-na.omit(alko2011)

alko2000<-wtd.mean(alko2000$ap85,weights = alko2003$waga_2000_ind)
alko2003<-wtd.mean(alko2003$bp72,weights = alko2003$waga_2003_ind)
alko2005<-wtd.mean(alko2005$cp79,weights = alko2005$waga_2005_ind)
alko2007<-wtd.mean(alko2007$dp76,weights = alko2007$waga_2007_ind)
alko2009<-wtd.mean(alko2009$ep71,weights = alko2009$waga_2009_ind)
alko2011<-wtd.mean(alko2011$gp72,weights = alko2011$waga_2011_ind)

alkoholicy<-c(alko2000,alko2003,alko2005,alko2007,alko2009,alko2011)
time<-c(2000,2003,2005,2007,2009,2011)

alkomafia<-as.data.frame(cbind(time,alkoholicy))

alkoplot<-ggplot(alkomafia,aes(x=time,y=alkoholicy))+ geom_point() + geom_line(col='green')+
                   ggtitle("Alkoholicy na przestrzeni lat :(") +
                   labs(x = "Lata", y ="Alkoholicy" )
alkoplot

#CARG
mult2<-(alkoholicy[length(alkoholicy)]/alkoholicy[1])^(1/5)
mult2

plot(time,alkoholicy)
lines(time,alkoholicy)
alkoholicy[1]*mult2^(0:5)
lines(time,alkoholicy[1]*mult2^(0:5),col='red')

chain_index<-alkoholicy[2:length(alkoholicy)]/alkoholicy[1:length(alkoholicy)-1]*100
alkomafia<-as.data.frame(cbind(time[2:length(alkoholicy)], chain_index))
plotinho<-ggplot(alkomafia,aes(x=time[2:length(alkoholicy)],y=chain_index))+ geom_point()+geom_line(col='green')+
  ggtitle("Chain index") +
  labs(x = "Year", y ="Index")
plotinho
fixed_index<-alkoholicy/alkoholicy[1]*100
alkomafia<-as.data.frame(cbind(time, fixed_index))
plotto<-ggplot(alkomafia, aes(x=time[1:length(alkoholicy)],y=fixed_index))+ geom_point()+geom_line(col='pink')+
  ggtitle("Fixed index") +
  labs(x = "Year", y ="Index")
plotto

