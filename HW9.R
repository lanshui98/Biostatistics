##HW9_514
##Q2
#b
library(readr)
FEV<-read.csv("Downloads/fev.csv")
# recoding smoke variable (smoker as 1, non-smoker as 0)
FEV$smokenew <- 2-FEV$smoke
strata.r1 <- data.frame(Estimate=NA, Robust.SE=NA, CI95.L=NA, CI95.H=NA)
strata.r1[1:4,] <- NA
mylist<-c('age<=9','age==10|age==11','age==12|age==13','age>=14')
library('rigr')
fev.1<-subset(FEV,age<=9)
fev.2<-subset(FEV,age==10|age==11)
fev.3<-subset(FEV,age==12|age==13)
fev.4<-subset(FEV,age>=14)
r1 <- regress("mean", fev~smokenew, data=fev.1)
strata.r1[1,] <- r1$augCoefficients[2, c(1,3,4,5)]
r2 <- regress("mean", fev~smokenew, data=fev.2)
strata.r1[2,] <- r2$augCoefficients[2, c(1,3,4,5)]
r3 <- regress("mean", fev~smokenew, data=fev.3)
strata.r1[3,] <- r3$augCoefficients[2, c(1,3,4,5)]
r4 <- regress("mean", fev~smokenew, data=fev.4)
strata.r1[4,] <- r4$augCoefficients[2, c(1,3,4,5)]
row.names(strata.r1) <- c("age<=9","age=10or11","age=12or13","age=>14")
round(strata.r1,2)

#meta-analysis
library('metafor')
rma1 <- rma( yi=Estimate, sei= Robust.SE , data=strata.r1, method="FE")
forest(rma1, slab=c("age<=9","age=10or11","age=12or13","age=>14"), 
       digits=3, showweights=TRUE,
       xlab="Difference in fev (smoke-nonsmoke)")
##Q3
#a
FEV$agecat <- cut(FEV$age, quantile(FEV$age, seq(0,1,l=5)),
                  include.lowest=TRUE)
levels(FEV$agecat) #[3,8],(8,10],(10,12],(12,19]
strata.r2 <- data.frame(Estimate=NA, Robust.SE=NA, CI95.L=NA, CI95.H=NA)
strata.r2[1:4,] <- NA
fevnew.1<-subset(FEV,agecat=='[3,8]')
fevnew.2<-subset(FEV,agecat=='(8,10]')
fevnew.3<-subset(FEV,agecat=='(10,12]')
fevnew.4<-subset(FEV,agecat=='(12,19]')
rnew1 <- regress("mean", fev~smokenew, data=fevnew.1)
strata.r2[1,] <- rnew1$augCoefficients[2, c(1,3,4,5)]
rnew2 <- regress("mean", fev~smokenew, data=fevnew.2)
strata.r2[2,] <- rnew2$augCoefficients[2, c(1,3,4,5)]
rnew3 <- regress("mean", fev~smokenew, data=fevnew.3)
strata.r2[3,] <- rnew3$augCoefficients[2, c(1,3,4,5)]
rnew4 <- regress("mean", fev~smokenew, data=fevnew.4)
strata.r2[4,] <- rnew4$augCoefficients[2, c(1,3,4,5)]
row.names(strata.r2) <- c("[3,8]","(8,10]","(10,12]","(12,19]")
round(strata.r2,2) # agecat=='[3,8]' has no smoker, omit it

#meta-analysis
rma2 <- rma( yi=Estimate, sei= Robust.SE , data=strata.r2[2:4,], method="FE")
rma2

#b
result<-c(1,1,1,1)
for (i in 2:8){
  FEV$agecat <- cut(FEV$age, quantile(FEV$age, seq(0,1,l=i+1)),
                    include.lowest=TRUE)
  strata.r <- data.frame(Estimate=NA, Robust.SE=NA, CI95.L=NA, CI95.H=NA)
  strata.r[1:i,] <- NA
  for (j in 1:i){
    data.age<-subset(FEV,agecat==levels(FEV$agecat)[j])
    if(all(data.age$smokenew==0)){
      strata.r[j,] <- c(NA,NA,NA,NA)
      # if the entire strata are non-smokers, can’t do regression 
    }
    else{
      r <- regress("mean", fev~smokenew, data=data.age)
      strata.r[j,] <- r$augCoefficients[2, c(1,3,4,5)]
    }
  }
  result<-rbind.data.frame(result,strata.r)
}

#2 equal strata
rma.2strata <- rma( yi=Estimate, sei= Robust.SE , data=result[2:3,], method="FE")
rma.2strata

#3 equal strata
rma.3strata <- rma( yi=Estimate, sei= Robust.SE , data=result[4:6,], method="FE")
rma.3strata

#4 equal strata
rma.4strata <- rma( yi=Estimate, sei= Robust.SE , data=result[8:10,], method="FE")
rma.4strata

#5 equal strata
rma.5strata <- rma( yi=Estimate, sei= Robust.SE , data=result[12:15,], method="FE")
rma.5strata

#6 equal strata
rma.6strata <- rma( yi=Estimate, sei= Robust.SE , data=result[17:21,], method="FE")
rma.6strata

#7 equal strata
rma.7strata <- rma( yi=Estimate, sei= Robust.SE , data=result[24:28,], method="FE")
rma.7strata

#8 equal strata
rma.8strata <- rma( yi=Estimate, sei= Robust.SE , data=result[31:36,], method="FE")
rma.8strata
