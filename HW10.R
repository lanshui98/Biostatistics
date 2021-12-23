##HW10_514
##Q1
#a
library(readr)
plasma<-read.csv("Downloads/hw10plasma.csv")
plasma$ESR_new<-ifelse(plasma$ESR=='ESR < 20',0,1)
library("rigr")
r<-regress("odds", ESR_new~fibrinogen, data=plasma)
#b
y=exp(-6.845+1.827*x)/(1+(exp(-6.845+1.827*x)))
plot(x=plasma$fibrinogen,y=plasma$ESR_new,xlab = 'fibrinogen',ylab = 'ESR')
abline(h=0.5,col='red')
lines(x,y,'l',xlim = c(2,5),ylim = c(0,1),xlab = 'fibrinogen',ylab = 'ESR')

##Q2
#a
laryng<-read.csv("Downloads/hw10laryng.csv")
table(laryng$status,laryng$stage)
library("survival")
y <- Surv(laryng$time,laryng$status)
eS <- survfit(y~1,data = laryng)
plot(eS, xlab="time", ylab="Survival", conf.int=TRUE)
summary(eS, time=6.0) # when the time is 6.0

#b
eS2 <- survfit(y~stage,data = laryng)
plot(eS2,col=c("black","red","green","blue"))
legend("topright",lty=c(1,1,1,1),col=c("black","red","green","blue"),
       legend=c("stage1","stage2","stage3","stage4"))

#c
#relable for stage 1or2 as group being 1 and stage 3or4 as group being 2
laryng$group<-ifelse(laryng$stage==1|laryng$stage==2,1,2)
eS3<-survdiff(y~group, data=laryng)
eS3