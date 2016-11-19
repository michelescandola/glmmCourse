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

(mdlIntercept = lm(sells~1,data=dati))
mean((dati$sells-mdlIntercept$fitted.values)^2)
#1532167

(g1=ggplot(dati,aes(y=sells,x=idStore))+geom_point()+theme_bw())
(g2=g1+geom_abline(intercept=mdlIntercept$coefficients,slope=0,colour="red",size=2))

(mdlPrice = lm(sells~price,data=dati))
mean((dati$sells-mdlPrice$fitted.values)^2)
#704123

(g1a=ggplot(dati,aes(y=sells,x=price))+geom_point()+theme_bw())
(g2a=g1a+geom_abline(intercept=mdlPrice$coefficients[1],
                     slope=mdlPrice$coefficients[2],colour="red",size=2))

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
