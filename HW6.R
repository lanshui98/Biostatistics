##HW6_514
##Q1
library(readr)
FEV<-read.csv("Downloads/fev.csv")
#b
str(FEV) #654 obs.
many.corrhatstar <- rep(NA, 10000)
set.seed(4)
for(i in 1:10000){
  resample <- sample(1:654, replace=TRUE)
  boot.data <- FEV[resample,]
  many.corrhatstar[i] <- cor(boot.data$age,boot.data$fev)
}
c1<-quantile(many.corrhatstar, c(0.025, 0.975)) #95% CI
c2<-quantile(many.corrhatstar, c(0.05, 0.95)) #90% CI
cor(FEV$age,FEV$fev)
#c
set.seed(5)
for(i in 1:10000){
  resample <- sample(1:654, replace=TRUE)
  boot.data <- FEV[resample,]
  many.corrhatstar[i] <- cor(boot.data$age,boot.data$fev)
}
c3<-quantile(many.corrhatstar, c(0.025, 0.975)) #95% CI
c4<-quantile(many.corrhatstar, c(0.05, 0.95)) #90% CI

set.seed(6)
for(i in 1:10000){
  resample <- sample(1:654, replace=TRUE)
  boot.data <- FEV[resample,]
  many.corrhatstar[i] <- cor(boot.data$age,boot.data$fev)
}
c5<-quantile(many.corrhatstar, c(0.025, 0.975)) #95% CI
c6<-quantile(many.corrhatstar, c(0.05, 0.95)) #90% CI

set.seed(7)
for(i in 1:10000){
  resample <- sample(1:654, replace=TRUE)
  boot.data <- FEV[resample,]
  many.corrhatstar[i] <- cor(boot.data$age,boot.data$fev)
}
c7<-quantile(many.corrhatstar, c(0.025, 0.975)) #95% CI
c8<-quantile(many.corrhatstar, c(0.05, 0.95)) #90% CI

set.seed(8)
for(i in 1:10000){
  resample <- sample(1:654, replace=TRUE)
  boot.data <- FEV[resample,]
  many.corrhatstar[i] <- cor(boot.data$age,boot.data$fev)
}
c9<-quantile(many.corrhatstar, c(0.025, 0.975)) #95% CI
c10<-quantile(many.corrhatstar, c(0.05, 0.95)) #90% CI

set.seed(9)
for(i in 1:10000){
  resample <- sample(1:654, replace=TRUE)
  boot.data <- FEV[resample,]
  many.corrhatstar[i] <- cor(boot.data$age,boot.data$fev)
}
c11<-quantile(many.corrhatstar, c(0.025, 0.975)) #95% CI
c12<-quantile(many.corrhatstar, c(0.05, 0.95)) #90% CI

##Q2
psa<-read.csv("Downloads/psa.csv")
#b
time<-psa$obstime
event<-as.factor(psa$inrem)
event<-!(as.numeric(event)-1)
library(survival)
y<-Surv(time,event)
#c
str(psa)
psa$relapse24 <- psa$inrem=="no" & psa$obstime<24
table(psa$relapse24) # 22 TRUE and 28 FALSE
set.seed(4)
many.meandiffhatstar <- rep(NA, 10000)
for(i in 1:10000){
  resample <- sample(1:50, replace=TRUE)
  boot.data <- psa[resample,]
  mean1 <- with(subset(boot.data,relapse24=="TRUE"),mean(nadirpsa))
  mean2 <- with(subset(boot.data,relapse24=="FALSE"),mean(nadirpsa))
  many.meandiffhatstar[i] <- mean1-mean2
}
quantile(many.meandiffhatstar, c(0.025, 0.975))
meandiff<-with(subset(psa,relapse24=="TRUE"),mean(nadirpsa))-
  with(subset(psa,relapse24=="FALSE"),mean(nadirpsa))
#d
set.seed(4)
many.geomean.ratiohatstar <- rep(NA, 10000)
for(i in 1:10000){
  resample <- sample(1:50, replace=TRUE)
  boot.data <- psa[resample,]
  geo.mean1 <- with(subset(boot.data,relapse24=="TRUE"),exp(mean(log(nadirpsa))))
  geo.mean2 <- with(subset(boot.data,relapse24=="FALSE"),exp(mean(log(nadirpsa))))
  many.geomean.ratiohatstar[i] <- geo.mean1/geo.mean2
}
quantile(many.geomean.ratiohatstar, c(0.025, 0.975))
geomean.ratio<-with(subset(psa,relapse24=="TRUE"),exp(mean(log(nadirpsa))))/
  with(subset(psa,relapse24=="FALSE"),exp(mean(log(nadirpsa))))
#e
psa$lowpsa <- psa$nadirpsa<1.5
table(psa$lowpsa) # 22 FALSE and 28 TRUE
set.seed(4)
many.prodiffhatstar <- rep(NA, 10000)
for(i in 1:10000){
  resample <- sample(1:50, replace=TRUE)
  boot.data <- psa[resample,]
  pro1 <- with(subset(boot.data,lowpsa=="TRUE"),mean(as.numeric(relapse24)))
  pro2 <- with(subset(boot.data,lowpsa=="FALSE"),mean(as.numeric(relapse24)))
  many.prodiffhatstar[i] <- pro1-pro2
}
quantile(many.prodiffhatstar, c(0.025, 0.975))
prodiff<-with(subset(psa,lowpsa=="TRUE"),mean(as.numeric(relapse24))-
                with(subset(psa,lowpsa=="FALSE"),mean(as.numeric(relapse24))))

##Q3
#a
many.beta1hatstar <- rep(NA, 10000)
set.seed(4)
for(i in 1:10000){
  resample <- sample(1:654, replace=TRUE)
  boot.data <- FEV[resample,]
  boot.lm <- lm(fev~age, data=boot.data)
  many.beta1hatstar[i] <- coef(boot.lm)[2]
}
c.beta1<-quantile(many.beta1hatstar, c(0.025, 0.975)) #95% CI for beta1
coef(lm(fev~age, data=FEV))

#b
many.beta1hatstar <- rep(NA, 10000)
set.seed(4)
for(i in 1:10000){
  resample <- sample(1:50, replace=TRUE)
  boot.data <- psa[resample,]
  boot.lm <- lm(nadirpsa~relapse24, data=boot.data)
  many.beta1hatstar[i] <- coef(boot.lm)[2]
}
c.beta1<-quantile(many.beta1hatstar, c(0.025, 0.975)) #95% CI for beta1
coef(lm(nadirpsa~relapse24, data=psa))

#c
many.beta1hatstar <- rep(NA, 10000)
set.seed(4)
for(i in 1:10000){
  resample <- sample(1:50, replace=TRUE)
  boot.data <- psa[resample,]
  boot.lm <- lm(log(nadirpsa)~relapse24, data=boot.data)
  many.beta1hatstar[i] <- coef(boot.lm)[2]
}
c.beta1<-quantile(many.beta1hatstar, c(0.025, 0.975)) #95% CI for beta1
beta<-coef(lm(log(nadirpsa)~relapse24, data=psa))
exp.slope<-exp(beta[2])

#d
many.beta1hatstar <- rep(NA, 10000)
set.seed(4)
for(i in 1:10000){
  resample <- sample(1:50, replace=TRUE)
  boot.data <- psa[resample,]
  boot.lm <- lm(relapse24~lowpsa, data=boot.data)
  many.beta1hatstar[i] <- coef(boot.lm)[2]
}
c.beta1<-quantile(many.beta1hatstar, c(0.025, 0.975)) #95% CI for beta1
beta<-coef(lm(relapse24~lowpsa, data=psa))