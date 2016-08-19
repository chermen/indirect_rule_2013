setwd("~")
library(foreign)
library(coefplot)
library(Zelig)
library(nnet)
library(MASS)
library(xtable)

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

###alternative forest plot for black and white publications

Alphas <- seq(1, 99, 2) / 100

Multiplier <- qnorm(1 - Alphas / 2)
zzTransparency <<- 1/(length(Multiplier)/4)
CoefficientTables <- lapply(models, function(x){summary(x)$coef})
TableRows <- unlist(lapply(CoefficientTables, nrow))

if(modelnames[1] == ""){
  ModelNameLabels <- rep(paste("Model", 1:length(TableRows)), TableRows)
} else {
  ModelNameLabels <- rep(modelnames, TableRows)
}

MatrixofModels <- cbind(do.call(rbind, CoefficientTables), ModelNameLabels)
if(removeintercept == TRUE){
  MatrixofModels <- MatrixofModels[!rownames(MatrixofModels) == "(Intercept)", ]
}
MatrixofModels <- data.frame(cbind(rownames(MatrixofModels), MatrixofModels))

MatrixofModels <- data.frame(cbind(MatrixofModels, rep(Multiplier, each = nrow(MatrixofModels))))

colnames(MatrixofModels) <- c("IV", "Estimate", "StandardError", "TValue", "PValue", "ModelName", "Scalar")
MatrixofModels$IV <- factor(MatrixofModels$IV, levels = MatrixofModels$IV)
MatrixofModels[, -c(1, 6)] <- apply(MatrixofModels[, -c(1, 6)], 2, function(x){as.numeric(as.character(x))})
MatrixofModels$Emphasis <- by(1 - seq(0, 1, length = length(Multiplier) + 1)[-1], as.character(round(Multiplier, 5)), mean)[as.character(round(MatrixofModels$Scalar, 5))]

OutputPlot <- qplot(data = MatrixofModels, x = IV, y = Estimate,
                    ymin = Estimate - Scalar * StandardError, ymax = Estimate + Scalar * StandardError,
                    ylab = NULL, xlab = NULL, alpha = I(zzTransparency), colour = I(gray(0)), geom = "blank")
OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
OutputPlot <- OutputPlot + geom_linerange(data = MatrixofModels, aes(size = 1/Emphasis), alpha = I(zzTransparency), colour = I(gray(0)))
OutputPlot <- OutputPlot + scale_size_continuous(legend = FALSE)
OutputPlot <- OutputPlot + facet_grid(~ ModelName) + coord_flip() + geom_point(aes(x = IV, y = Estimate), colour = I(gray(0))) + theme_bw()
return(OutputPlot)
}

models<-list(controls, response, full)

SmoothCoefficientPlot(models, removeintercept=TRUE)

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

glm.1<-zelig(rule2~noputin+norep+hardlife+trust+migrate+tag(1|reg), data=ir1, model="logit.mixed")

summary(glm.1)

#### explanatory

glm.2<-zelig(rule2~income+incomesq+ed+edsq+native+tension+
               network+tag(1|reg), 
             data=ir1, model="logit.mixed")
             
summary(glm.2)


#### full

glm.3<-zelig(rule2~noputin+norep+hardlife+trust+migrate+income+incomesq+ed+edsq+native+
               tension+network+tag(1|reg), data=ir1, model="logit.mixed")

summary(glm.3)




