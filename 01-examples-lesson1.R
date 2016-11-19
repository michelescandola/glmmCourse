# We have data from an energy bar manafacturer.
# These data consists in the number of sells, price, and money spent in promotional activities per shop.
# We want to build the best linear model which fit the number of sells data, considering them as a dependent variable,
# while the price and the money spent are independent variables.

library(ggplot2)
library(gridExtra)

dati <- data.frame(
  idStore = 1:34,
  sells   = c(4141,3842,3056,3519,4226,4630,3507,3754,5000,5120,4011,5015,1916,675,3636,3224,2295,2730,2618,
              4421,4113,3746,3532,3825,1096,761,2088,820,2114,1882,2159,1602,3354,2927),
  price   = c(59,59,59,59,59,59,59,59,59,59,59,59,79,79,79,79,79,79,79,79,79,79,79,79,99,99,99,99,99,99,99,99,99,99),
  prom    = c(200,200,200,200,400,400,400,400,600,600,600,600,200,200,200,200,400,400,400,400,600,
              600,600,600,200,200,200,200,400,400,400,400,600,600)
)

## taking into account just 1 beta, the intercept
(mdlIntercept = lm(sells~1,data=dati))
mean((dati$sells-mdlIntercept$fitted.values)^2)
#1532167

(g1=ggplot(dati,aes(y=sells,x=idStore))+geom_point()+theme_bw())
(g2=g1+geom_abline(intercept=mdlIntercept$coefficients,slope=0,colour="red",size=2))


## taking into account 2 betas, the intercept and the slope for the price
(mdlPrice = lm(sells~price,data=dati))
mean((dati$sells-mdlPrice$fitted.values)^2)
#704123

(g1a=ggplot(dati,aes(y=sells,x=price))+geom_point()+theme_bw())
(g2a=g1a+geom_abline(intercept=mdlPrice$coefficients[1],
                     slope=mdlPrice$coefficients[2],colour="red",size=2))


## taking into account 3 beta, the intercept and the slopes
## for the price and promotion
(mdlPriceProm = lm(sells~price+prom,data=dati))
mean((dati$sells-mdlPriceProm$fitted.values)^2)
#371204

(grid.arrange(ggplot(dati,aes(y=sells,x=price))+geom_point()+theme_bw()+
                coord_cartesian(ylim=c(500,10000),xlim=c(0,110)),
              ggplot(dati,aes(y=sells,x=prom))+geom_point()+theme_bw()+
                coord_cartesian(ylim=c(500,10000),xlim=c(0,700))))

(grid.arrange(ggplot(dati,aes(y=sells,x=price))+geom_point()+theme_bw()+
                geom_abline(intercept=mdlPriceProm$coefficients[1],slope=mdlPriceProm$coefficients[2],
                            colour="red",size=2)+
                coord_cartesian(ylim=c(500,10000),xlim=c(0,110)),
              ggplot(dati,aes(y=sells,x=prom))+geom_point()+theme_bw()+
                geom_abline(intercept=mdlPriceProm$coefficients[1],slope=mdlPriceProm$coefficients[3],
                            colour="red",size=2)+
                coord_cartesian(ylim=c(500,10000),xlim=c(0,700))))

################################################

## R functions for the "usual" linear model output

# model with just the intercept
summary(mdlIntercept)
anova(mdlIntercept)

# model with the intercept and 1 slope
summary(mdlPrice)## the t-value for price regressor is -6.134
anova(mdlPrice)## the F-value for price regressor is 37.632

# the relation between the t-value and the F value
summary(mdlPrice)$coefficients[2,3]# the t value
anova(mdlPrice)$"F value"[1]# the F value

summary(mdlPrice)$coefficients[2,3]^2==anova(mdlPrice)$"F value"[1]

summary(mdlPrice)$coefficients[2,3]^2# the squared t value
anova(mdlPrice)$"F value"[1]#the F-value

all.equal(summary(mdlPrice)$coefficients[2,3]^2,anova(mdlPrice)$"F value"[1])

# model with the intercept and other 2 slopes
summary(mdlPriceProm)
anova(mdlPriceProm)#first type ANOVA
drop1(mdlPriceProm,test="F")#third type ANOVA

###############################################

## simple linear model to see what happens with different contrast matrices
## with a 2-levels-factor

males   <- c(19, 30, 20, 19, 29, 25, 21, 24)
females <- c(25, 21, 17, 15, 14, 14, 22, 17)

y = c(males,females)
gender = factor(c(rep("M",8),rep("F",8)))
gender2 = c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0)# dummy coding or treatment coding
gender3 = c(1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1)# effect coding
gender4 = c(0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1)# dummy coding or treatment coding

dati = data.frame(y=y,
                  gender,
                  gender2,
                  gender3,
                  gender4)

ggplot(dati,aes(y=y,x=gender))+
  geom_boxplot(fatten=3)+theme_bw()+
  geom_jitter(width=0.1,colour=as.numeric(dati$gender)+1)

mdlG1 = lm(y~gender)
summary(mdlG1)

mdlG2<-lm(y~gender2)
summary(mdlG2)
mean(dati$y)
medie=tapply(dati$y,dati$gender,mean)

ggplot(dati,aes(y=y,x=gender2))+
  geom_point()+
  geom_abline(intercept=mdlG2$coefficients[1],
              slope=mdlG2$coefficients[2],
              colour="red",size=2)+
  geom_text(label="General Mean",x = 0, y=mean(dati$y))+
  geom_text(label="Males",x = 0, y=medie[1])+
  geom_text(label="Females",x = 0, y=medie[2])+
  theme_bw()+
  coord_cartesian(xlim=c(-1.1,1.1))+
  ggtitle("Treatment Coding")

mdlG3<-lm(y~gender3)
summary(mdlG3)
ggplot(dati,aes(y=y,x=gender3))+
  geom_point()+
  geom_abline(intercept=mdlG3$coefficients[1],
              slope=mdlG3$coefficients[2],
              colour="red",size=2)+
  geom_text(label="General Mean",x = 0, y=mean(dati$y))+
  geom_text(label="Males",x = 0, y=medie[1])+
  geom_text(label="Females",x = 0, y=medie[2])+
  theme_bw()+
  coord_cartesian(xlim=c(-1.1,1.1))+
  ggtitle("Effect Coding")

mdlG4<-lm(y~gender4)
summary(mdlG4)
ggplot(dati,aes(y=y,x=gender4))+
  geom_point()+
  geom_abline(intercept=mdlG4$coefficients[1],
              slope=mdlG4$coefficients[2],
              colour="red",size=2)+
  geom_text(label="General Mean",x = 0, y=mean(dati$y))+
  geom_text(label="Males",x = 0, y=medie[1])+
  geom_text(label="Females",x = 0, y=medie[2])+
  theme_bw()+
  coord_cartesian(xlim=c(-1.1,1.1))+
  ggtitle("Treatment Coding")

anova(mdlG1)
anova(mdlG2)
anova(mdlG3)
anova(mdlG4)

###################################################

## example with a 3-levels factor
# we want to assess if taxi drivers and/or bus drivers run faster than
# normal people. We have the nomber of meters they run in 30 seconds
set.seed(1)
dati.run = data.frame(
  y = c(rnorm(10,mean=7,sd=5),rnorm(10,mean=15,sd=3),rnorm(10,mean=11,sd=5))+rnorm(30),
  group = gl(n=3,k=10,labels=c("normalPeople","taxiDrivers","busDrivers"))
)

ggplot(dati.run,aes(y=y,x=group))+
  geom_boxplot(fatten=3)+theme_bw()+
  geom_jitter(width=0.1,colour=as.numeric(dati.run$group)+1)

medie=aggregate(y~group,data=dati.run,FUN=mean)

contrasts(dati.run$group)

# dummy contrasts
contrasts(dati.run$group) = contr.treatment(n=3,base=1)
mdl.run.CT = lm(y~group,data=dati.run)
summary(mdl.run.CT)
anova(mdl.run.CT)

dati.graph1 = subset(dati.run,group!="busDrivers")
dati.graph1$x = ifelse(dati.graph1$group=="normalPeople",0,1)
dati.graph2 = subset(dati.run,group!="taxiDrivers")
dati.graph2$x = ifelse(dati.graph2$group=="normalPeople",0,1)

grid.arrange(
  ggplot(dati.graph1,aes(y=y,x=x))+
    geom_point()+
    geom_abline(intercept=mdl.run.CT$coefficients[1],
                slope=mdl.run.CT$coefficients[2],
                colour="red",size=2)+
    geom_text(label="General Mean",x = 0, y=mean(dati.run$y))+
    geom_text(label="Normal People",x = 0, y=medie$y[1])+
    geom_text(label="Taxi Drivers",x = 0, y=medie$y[2])+
    geom_text(label="Bus Drivers",x = 0, y=medie$y[3])+
    theme_bw()+
    coord_cartesian(xlim=c(-1.1,1.1))+
    ggtitle("Treatment Coding: (Intercept) and group2 - normal people and taxi drivers"),
  ggplot(dati.graph2,aes(y=y,x=x))+
    geom_point()+
    geom_abline(intercept=mdl.run.CT$coefficients[1],
                slope=mdl.run.CT$coefficients[3],
                colour="red",size=2)+
    geom_text(label="General Mean",x = 0, y=mean(dati.run$y))+
    geom_text(label="Normal People",x = 0, y=medie$y[1])+
    geom_text(label="Taxi Drivers",x = 0, y=medie$y[2])+
    geom_text(label="Bus Drivers",x = 0, y=medie$y[3])+
    theme_bw()+
    coord_cartesian(xlim=c(-1.1,1.1))+
    ggtitle("Treatment Coding: (Intercept) and group3 - normal people and bus drivers")
  )

# dummy contrasts 2, taxi drivers as baseline
contrasts(dati.run$group) = contr.treatment(n=3,base=2)
mdl.run.CT2 = lm(y~group,data=dati.run)
summary(mdl.run.CT2)
anova(mdl.run.CT2)

# effect coding
contrasts(dati.run$group) = contr.sum(n=3)
mdl.run.EC = lm(y~group,data=dati.run)
summary(mdl.run.EC)
anova(mdl.run.EC)

tmp = subset(dati.run,group=="normalPeople")$y
dati.graph1 = data.frame(y=tmp,x=rep(1,10))

tmp = subset(dati.run,group=="taxiDrivers")$y
dati.graph2 = data.frame(y=tmp,x=rep(1,10))

grid.arrange(
  ggplot(dati.graph1,aes(y=y,x=x))+
    geom_point()+
    geom_abline(intercept=mdl.run.EC$coefficients[1],
                slope=mdl.run.EC$coefficients[2],
                colour="red",size=2)+
    geom_text(label="General Mean",x = 0, y=mean(dati.run$y))+
    geom_text(label="Normal People",x = 0, y=medie$y[1])+
    geom_text(label="Taxi Drivers",x = 0, y=medie$y[2])+
    geom_text(label="Bus Drivers",x = 0, y=medie$y[3])+
    theme_bw()+
    coord_cartesian(xlim=c(-1.1,1.1))+
    ggtitle("Treatment Coding: (Intercept) and group1 - normal people against general mean"),
  ggplot(dati.graph2,aes(y=y,x=x))+
    geom_point()+
    geom_abline(intercept=mdl.run.EC$coefficients[1],
                slope=mdl.run.EC$coefficients[3],
                colour="red",size=2)+
    geom_text(label="General Mean",x = 0, y=mean(dati.run$y))+
    geom_text(label="Normal People",x = 0, y=medie$y[1])+
    geom_text(label="Taxi Drivers",x = 0, y=medie$y[2])+
    geom_text(label="Bus Drivers",x = 0, y=medie$y[3])+
    theme_bw()+
    coord_cartesian(xlim=c(-1.1,1.1))+
    ggtitle("Treatment Coding: (Intercept) and group3 - normal people against general mean")
)

#############################################
## within-subjects

set.seed(1)
dati.within = data.frame(
  y = c(rnorm(100,mean=7,sd=5),rnorm(100,mean=15,sd=3),rnorm(100,mean=11,sd=5))+rnorm(30),
  group = gl(n=3,k=100,labels=c("baseline","pre","post")),
  subject = c(rep(1:10,10),rep(1:10,10),rep(1:10,10))
)

dat=aggregate(y~group*subject,data=dati.within,FUN=mean)

mdlBetween = lm(y~group,data=dati.within)
mdlWithin  = aov(y~group+Error(subject/group),data=dati.within)
coefficients(mdlBetween)
coefficients(mdlWithin)

mdlBetween = lm(y~group,data=dat)
mdlWithin  = aov(y~group+Error(subject/group),data=dat)
coefficients(mdlBetween)
coefficients(mdlWithin)

contrasts(dat$group)

dati.graph1 = subset(dat,group!="post")
dati.graph1$x = ifelse(dati.graph1$group=="baseline",0,1)
dati.graph2 = subset(dat,group!="pre")
dati.graph2$x = ifelse(dati.graph2$group=="baseline",0,1)

grid.arrange(
  ggplot(dati.graph1,aes(y=y,x=x))+
    geom_point()+
    geom_abline(intercept=mdlBetween$coefficients[1],
                slope=mdlBetween$coefficients[2],
                colour="red",size=2)+
    geom_abline(intercept=coefficients(mdlWithin)[[1]],
                slope=coefficients(mdlWithin)[[4]][1],
                colour="blue",size=2)+
    theme_bw()+
    coord_cartesian(xlim=c(-1.1,10),ylim=c(0,50))+
    ggtitle("Baseline vs. Pre"),
  ggplot(dati.graph2,aes(y=y,x=x))+
    geom_point()+
    geom_abline(intercept=mdlBetween$coefficients[1],
                slope=mdlBetween$coefficients[3],
                colour="red",size=2)+
    geom_abline(intercept=coefficients(mdlWithin)[[1]],
                slope=coefficients(mdlWithin)[[4]][2],
                colour="blue",size=2)+
    theme_bw()+
    coord_cartesian(xlim=c(-1.1,10),ylim=c(0,50))+
    ggtitle("Baseline vs. Post")+
    xlab("blue: within subjects, red: between subjects")
)
