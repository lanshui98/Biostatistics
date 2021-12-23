##HW8_514
##Q2
#a
library(readr)
NHANES <- read.csv("http://faculty.washington.edu/kenrice/heartgraphs/nhaneslarge.csv",na.strings=".")
#BPXSAR>140 is hypertension (hypertension is 1)
NHANES$hypertension <- as.numeric(NHANES$BPXSAR>140)
library('rigr')
r1<-regress("mean", hypertension~ RIDAGEYR, data=NHANES)
#b
precision.naive<-1/(r1$augCoefficients["RIDAGEYR",c("Naive SE")])^2
precision.robust<-1/(r1$augCoefficients["RIDAGEYR",c("Robust SE")])^2
(r1$augCoefficients["RIDAGEYR",c("Robust SE")]/r1$augCoefficients["RIDAGEYR",c("Naive SE")])
(r1$augCoefficients["RIDAGEYR",c("Robust SE")]/r1$augCoefficients["RIDAGEYR",c("Naive SE")])^2
#c
plot(hypertension~RIDAGEYR,data=NHANES,xlab="Age (years)",ylab="hypertension",
     pch=19, col= adjustcolor("black", alpha=0.2))
abline(lm(hypertension ~ RIDAGEYR,data=NHANES),col="magenta")

##Q3
#a
FEV<-read.csv("Downloads/fev.csv")
reg.a<-regress("mean", fev~ age, data=FEV)
pvalue.a<-reg.a$augCoefficients[2,8]
#b
psa<-read.csv("Downloads/psa.csv")
psa$relapse24 <- psa$inrem=="no" & psa$obstime<24
pvalue.b<-t.test(nadirpsa~relapse24, data=psa)$p.value
#c
pvalue.c<-t.test(log(nadirpsa)~relapse24, data=psa)$p.value
#d
psa$psanadirlow <- psa$nadirpsa < 1.5
pvalue.d<-t.test(relapse24~psanadirlow, data=psa)$p.value

##Q4
power.t.test(delta=10, sd=20, sig.level=0.05, power=0.8)
power.t.test(delta=10, sd=40, sig.level=0.05, power=0.8)
