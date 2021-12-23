##HW7_514
##Q1
#a
library(readr)
FEV<-read.csv("Downloads/fev.csv")
str(FEV) #654 obs.
many.corrhatstar <- rep(NA, 10000)
set.seed(4)
for(i in 1:10000){
  resample <- sample(1:654, replace=TRUE)
  boot.data <- FEV[resample,]
  many.corrhatstar[i] <- cor(boot.data$age,boot.data$fev)
}
mean.hatstar<-mean(many.corrhatstar)
sd.hatstar<-sd(many.corrhatstar)
CI<-mean.hatstar+c(-1,1)*1.96*sd.hatstar

#b
standard.errors.wide<-2*(1-mean.hatstar)/sd.hatstar
pnorm(standard.errors.wide/2)

##Q2
#a
many.beta1hatstar <- rep(NA, 10000)
set.seed(4)
for(i in 1:10000){
  resample <- sample(1:654, replace=TRUE)
  boot.data <- FEV[resample,]
  boot.lm <- lm(fev~age, data=boot.data)
  many.beta1hatstar[i] <- coef(boot.lm)[2]
}
mean.hatstar<-mean(many.beta1hatstar)
sd.hatstar<-sd(many.beta1hatstar)
CI<-mean.hatstar+c(-1,1)*1.96*sd.hatstar

qqnorm(many.beta1hatstar)
abline(a = mean.hatstar,b = sd.hatstar,lty=2)

library("rigr")
r1 <- regress("mean", fev~age, data=FEV)
r1

#b
#first recode smoke variable (smoker as 1, non-smoker as 0)
FEV$smokenew<-2-FEV$smoke
data9<-subset(FEV,age <= 9)
data1011<-subset(FEV,age == 10 | age == 11)
data1213<-subset(FEV,age == 12 | age == 13)
data14<-subset(FEV,age >= 14)

r2 <- regress("mean", fev~smokenew, data=data9)
r2
many.beta1hatstar <- rep(NA, 10000)
str(data9) #100 obs
set.seed(4)
for(i in 1:10000){
  resample <- sample(1:654, replace=TRUE)
  boot.data <- data9[resample,]
  boot.lm <- lm(fev~smokenew, data=boot.data)
  many.beta1hatstar[i] <- coef(boot.lm)[2]
}
mean.hatstar<-mean(many.beta1hatstar)
sd.hatstar<-sd(many.beta1hatstar)
CI1<-mean.hatstar+c(-1,1)*1.96*sd.hatstar
table(data9$smokenew)

r3 <- regress("mean", fev~smokenew, data=data1011)
r3
many.beta1hatstar <- rep(NA, 10000)
str(data1011) #100 obs
set.seed(4)
for(i in 1:10000){
  resample <- sample(1:171, replace=TRUE)
  boot.data <- data1011[resample,]
  boot.lm <- lm(fev~smokenew, data=boot.data)
  many.beta1hatstar[i] <- coef(boot.lm)[2]
}
mean.hatstar<-mean(many.beta1hatstar)
sd.hatstar<-sd(many.beta1hatstar)
CI2<-mean.hatstar+c(-1,1)*1.96*sd.hatstar

r4 <- regress("mean", fev~smokenew, data=data1213)
r4
many.beta1hatstar <- rep(NA, 10000)
str(data1213) #100 obs
set.seed(4)
for(i in 1:10000){
  resample <- sample(1:100, replace=TRUE)
  boot.data <- data1213[resample,]
  boot.lm <- lm(fev~smokenew, data=boot.data)
  many.beta1hatstar[i] <- coef(boot.lm)[2]
}
mean.hatstar<-mean(many.beta1hatstar)
sd.hatstar<-sd(many.beta1hatstar)
CI3<-mean.hatstar+c(-1,1)*1.96*sd.hatstar

r5 <- regress("mean", fev~smokenew, data=data14)
r5
many.beta1hatstar <- rep(NA, 10000)
str(data14) #74 obs
set.seed(4)
for(i in 1:10000){
  resample <- sample(1:74, replace=TRUE)
  boot.data <- data14[resample,]
  boot.lm <- lm(fev~smokenew, data=boot.data)
  many.beta1hatstar[i] <- coef(boot.lm)[2]
}
mean.hatstar<-mean(many.beta1hatstar)
sd.hatstar<-sd(many.beta1hatstar)
CI4<-mean.hatstar+c(-1,1)*1.96*sd.hatstar
