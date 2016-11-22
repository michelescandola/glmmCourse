rm(list=ls())

library(lme4)
library(car)
library(ggplot2)


## simulation of fixed data
b0 <- 9.9
b1 <- 2
# repeat measure times for 15 people
n <- c(13, 14, 14, 15, 12, 13, 7, 15, 16, 17, 14, 13, 11, 20, 11)
npeople <- length(n)
set.seed(1)
# generate x(fixed effect)
x <- matrix(rep(0, length=max(n) * npeople),ncol = npeople)
xx=NULL
peopleall <- NULL
for (i in 1:npeople){
  x[1:n[i], i] <- runif(n[i], min = 1, max = 5)
  x[1:n[i], i] <- sort(x[1:n[i], i])
  xx=c(xx,x[1:n[i],i])
  people <- rep(i, length = n[i])
  peopleall <- c(peopleall, people)
}
epsilon<- rnorm(n=length(xx),mean=0,sd=1)
dati.simulate = data.frame(y=b0+b1*xx+epsilon,id=as.factor(peopleall),xx)

mdl.fixed = lm(y~xx,data=dati.simulate)
summary(mdl.fixed)

cbind(estimated=coef(mdl.fixed),actual=c(b0,b1))

ggplot(dati.simulate,aes(y=y,x=xx))+
  geom_smooth(method="lm")+
  geom_point(aes(colour=id))+
  theme_bw()

ggplot(dati.simulate,aes(y=y,x=xx))+
  geom_point()+
  theme_bw()+
  coord_cartesian(ylim=c(-30,60))

# random intercept
intercept <- rnorm(npeople, mean = 0, sd = 10)
yall <- NULL
for (i in 1:npeople){
  # generate y
  ytmp <- rep(b0 + intercept[i], length = n[i]) +
    b1 * x[1:n[i],i]
  yall <- c(yall, ytmp) # combine y
}
# final dataset
data1 <- data.frame(y=yall+epsilon,id=as.factor(peopleall),x=xx)

mdl.Rintercept=lmer(y~x+(1|id),data=data1)

summary(mdl.Rintercept)

cbind(estimated=ranef(mdl.Rintercept)$id,actual=intercept)

ggplot(data1,aes(y=y,x=x,colour=id))+
  geom_smooth(method="lm")+
  geom_point()+
  theme_bw( )

ggplot(data1,aes(y=y,x=xx))+
  geom_point()+
  theme_bw()+
  coord_cartesian(ylim=c(-30,60))

# random slope
slope <- rnorm(npeople,sd=5) # random slope

yall <- NULL
for (i in 1:npeople){
  # generate y
  ytmp <- rep(b0 + intercept[i], length = n[i]) +
    (b1+slope[i]) * x[1:n[i],i]
  yall <- c(yall, ytmp) # combine y
}
# final dataset
data2 <- data.frame(y=yall+epsilon,id=as.factor(peopleall),x=xx)

mdl.Rslope=lmer(y~x+(x|id),data=data2)

summary(mdl.Rslope)

cbind(estimated=ranef(mdl.Rslope)$id[,1],actual=intercept)
cbind(estimated=ranef(mdl.Rslope)$id[,2],actual=slope)

ggplot(data2,aes(y=y,x=x,colour=id))+
  geom_smooth(method="lm")+
  geom_point()+
  theme_bw( )

ggplot(data2,aes(y=y,x=xx))+
  geom_point()+
  theme_bw()+
  coord_cartesian(ylim=c(-30,60))

#########################################

## example with random intercepts

#let's get a look into the Dyestuff dataset
#The Dyestuff data frame provides the yield of dyestuff (Naphthalene Black 12B)
#from 5 different preparations from each of 6 different batchs of an intermediate product (H-acid). 
str(Dyestuff)

head(Dyestuff)

summary(Dyestuff)

tail(Dyestuff)

table(Dyestuff$Batch)

ggplot(Dyestuff,aes(y=Yield,x=Batch))+
  geom_boxplot()+
  geom_jitter()+
  theme_bw()

fm1 = lmer(Yield~1+(1|Batch),data=Dyestuff)

summary(fm1)

fe=fixef(fm1)

re=ranef(fm1)$Batch

medie=aggregate(Yield~Batch,data=Dyestuff,FUN=mean)$Yield

ggplot(Dyestuff,aes(y=Yield,x=Batch))+
  geom_boxplot()+
  geom_jitter()+
  geom_label(x=1,y=fe+re[1,1],label="estimated")+
  geom_label(x=2,y=fe+re[2,1],label="estimated")+
  geom_label(x=3,y=fe+re[3,1],label="estimated")+
  geom_label(x=4,y=fe+re[4,1],label="estimated")+
  geom_label(x=5,y=fe+re[5,1],label="estimated")+
  geom_label(x=6,y=fe+re[6,1],label="estimated")+
  
  geom_point(x=1,y=medie[1],colour="red")+
  geom_point(x=2,y=medie[2],colour="red")+
  geom_point(x=3,y=medie[3],colour="red")+
  geom_point(x=4,y=medie[4],colour="red")+
  geom_point(x=5,y=medie[5],colour="red")+
  geom_point(x=6,y=medie[6],colour="red")+
  theme_bw()

##########################################################

fm2 = lmer(Yield~1+Batch+(1|Batch),data=Dyestuff)

summary(fm2)

fe2=fixef(fm2)

re2=ranef(fm2)$Batch

medie=aggregate(Yield~Batch,data=Dyestuff,FUN=mean)$Yield

ggplot(Dyestuff,aes(y=Yield,x=Batch))+
  geom_boxplot()+
  geom_jitter()+
  geom_label(x=1,y=fe2[1]+re2[1,1],label="estimated")+
  geom_label(x=2,y=fe2[1]+fe2[2]+re2[2,1],label="estimated")+
  geom_label(x=3,y=fe2[1]+fe2[3]+re2[3,1],label="estimated")+
  geom_label(x=4,y=fe2[1]+fe2[4]+re2[4,1],label="estimated")+
  geom_label(x=5,y=fe2[1]+fe2[5]+re2[5,1],label="estimated")+
  geom_label(x=6,y=fe2[1]+fe2[6]+re2[6,1],label="estimated")+
  
  geom_point(x=1,y=medie[1],colour="red")+
  geom_point(x=2,y=medie[2],colour="red")+
  geom_point(x=3,y=medie[3],colour="red")+
  geom_point(x=4,y=medie[4],colour="red")+
  geom_point(x=5,y=medie[5],colour="red")+
  geom_point(x=6,y=medie[6],colour="red")+
  theme_bw()

################################

ggplot(Dyestuff,aes(y=Yield,x=Batch))+
  geom_boxplot()+
  geom_jitter()+
  
  geom_label(x=1,y=fe+re[1,1],label="estimated")+
  geom_label(x=2,y=fe+re[2,1],label="estimated")+
  geom_label(x=3,y=fe+re[3,1],label="estimated")+
  geom_label(x=4,y=fe+re[4,1],label="estimated")+
  geom_label(x=5,y=fe+re[5,1],label="estimated")+
  geom_label(x=6,y=fe+re[6,1],label="estimated")+
  
  geom_label(x=1,y=fe2[1]+re2[1,1],label="estimated2")+
  geom_label(x=2,y=fe2[1]+fe2[2]+re2[2,1],label="estimated2")+
  geom_label(x=3,y=fe2[1]+fe2[3]+re2[3,1],label="estimated2")+
  geom_label(x=4,y=fe2[1]+fe2[4]+re2[4,1],label="estimated2")+
  geom_label(x=5,y=fe2[1]+fe2[5]+re2[5,1],label="estimated2")+
  geom_label(x=6,y=fe2[1]+fe2[6]+re2[6,1],label="estimated2")+
  
  geom_point(x=1,y=medie[1],colour="red")+
  geom_point(x=2,y=medie[2],colour="red")+
  geom_point(x=3,y=medie[3],colour="red")+
  geom_point(x=4,y=medie[4],colour="red")+
  geom_point(x=5,y=medie[5],colour="red")+
  geom_point(x=6,y=medie[6],colour="red")+
  theme_bw()

anova(fm1,fm2)

plot(fitted(fm1),Dyestuff$Yield)

#########################################

## example with random intercepts and slope

#Sleepstudy
# The average reaction time per day for subjects in a sleep deprivation study.
# On day 0 the subjects had their normal amount of sleep. Starting that night
# they were restricted to 3 hours of sleep per night. The observations represent
# the average reaction time on a series of tests given each day to each subject.
str(sleepstudy)

head(sleepstudy)

summary(sleepstudy)

tail(sleepstudy)

table(sleepstudy$Days,sleepstudy$Subject)

ggplot(sleepstudy,aes(y=Reaction,x=Days))+
  geom_smooth(method="lm")+
  geom_point()+
  theme_bw()

ggplot(sleepstudy,aes(y=Reaction,x=Days))+
  geom_smooth(method="lm")+
  geom_point()+
  facet_wrap(~Subject,ncol=5)+
  theme_bw()

fm1 = lmer(Reaction~Days+(1|Subject),data=sleepstudy)

summary(fm1)

fe=fixef(fm1)

re=ranef(fm1)$Subject

ggplot(sleepstudy,aes(y=Reaction,x=Days))+
  geom_smooth(method="lm")+
  geom_point()+
  geom_abline(intercept=fe[1],slope=fe[2],colour="red",size=2)+
  theme_bw()