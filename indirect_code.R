setwd("")
library(foreign)
library(coefplot)
library(Zelig)
library(nnet)
library(MASS)
library(xtable)
library(lme4)
library(lattice)

#read in data
ir1<-read.dta("ir1.dta")

###############List of variables

##Dependent variables
##rule2 - support for indirect rule 1, support for direct rule 0
##rule - ordinal indicator of support for indirect rule, 4 point increasing scale

# noputin  - President's Disapproval
# norep - Governor's Disapproval
# hardlife  - Hard life (life harder in your republic than in other places in Russia?)
# trust - Trust toward strangers
# migrate - Migrate, how long resided in current location on decreasing scale 
# income 
# income sq
# ed - education
# edsq - education squared
# native (Native to the North Caucasus 1, otherwise 0)
# tension - perception of ethnic tensions in society
# network - have friends outside his/her ethnic group 1, otherwise 0



###Logit

####controls

log.1<-glm(rule2~noputin+norep+hardlife+trust+migrate, data=ir1, 
           family=binomial(link="logit"))
summary(log.1)
xtable(log.1)

####explanatory

log.2<-glm(rule2~income+incomesq+ed+edsq+native+tension+
               network, data=ir1, family=binomial(link="logit"))
summary(log.2)
xtable(log.2)

####full

log.3<-glm(rule2~noputin+norep+hardlife+trust+migrate+income+incomesq+ed+edsq+native+
               tension+network, data=ir1, family=binomial(link="logit"))
summary(log.3)
xtable(log.3)

if ( .Platform$OS.type != "windows" ) { 
  windows <- function( ... ) X11( ... ) 
}

windows(10, 10)


b<-multiplot(log.1, log.2, log.3, intercept=F, decreasing=T,
             title="Support for Indirect Rule", 
             xlab="Coefficient Estimates", ylab="",
             cex=1.5, names=c("Control",
          "Explanatory", "Full"), 
          newNames=c(noputin="President Disapproval",
          norep="Governor Disapproval", 
          hardlife="Hard Life", migrate="Migrate", 
          income="Income", incomesq="Income Sq", 
          ed="Education", edsq="Education Sq", 
          native="Native", tension="Tension", 
          network="Network", trust="Trust"))

b+ theme(panel.background = element_blank())

ggsave(file="r.eps")
b + geom_point() +
scale_shape_manual(values=c(0,5,6))


##Multinomial models
ir1$rule3<-factor(ir1$rule)
ir1$rule4<-relevel(ir1$rule3, ref="1")

m1<-multinom(rule4~noputin+norep+hardlife+trust+migrate, data=ir1)
summary(m1)

m2<-multinom(rule4~income+incomesq+ed+edsq+native+tension+
               network, data=ir1)
summary(m2)

m3<-multinom(rule2~noputin+norep+hardlife+trust+migrate+income+incomesq+ed+edsq+native+
               tension+network, data=ir1)
summary(m3)


###ordinal models

ord1<-polr(as.factor(rule)~noputin+norep+hardlife+trust+migrate, data=ir1)
summary(ord1)

ord2<-polr(as.factor(rule)~income+incomesq+ed+edsq+native+tension+
               network, data=ir1)
summary(ord2)

ord3<-polr(as.factor(rule)~noputin+norep+hardlife+trust+migrate+income+incomesq+ed+edsq+
native+tension+network, data=ir1)
summary(ord3)


##hierarchical models
####null model

glm.0<-lmer(rule2~1|reg, data=ir1, method='REML')
summary(glm.0)

#ICC
0.019232/( 0.019232+0.220426)

#### controls

glm.1<-glmer(rule2~noputin+norep+hardlife+trust+migrate+(1|reg), family=binomial, 
             data=ir1)

summary(glm.1)

#### explanatory

glm.2<-glmer(rule2~income+incomesq+ed+edsq+native+tension+
               network+(1|reg), family=binomial,
             data=ir1)
             
summary(glm.2)


#### full

glm.3<-glmer(rule2~noputin+norep+hardlife+trust+migrate+income+incomesq+ed+edsq+native+
               tension+network+(1|reg), family=binomial,
             data=ir1)

summary(glm.3)




