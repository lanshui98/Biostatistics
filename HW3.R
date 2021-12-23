##HW3_Q3
library(readr)
salary <- read_csv("Downloads/salary.csv")

#a(i)
male<-subset(salary,gender=="M")
female<-subset(salary,gender=="F")

summary(male$salary)
var(male$salary,na.rm = TRUE)
summary(female$salary)
var(female$salary,na.rm = TRUE)

#a(ii)
Assistant<-subset(salary,rank=="Assist")
Associate<-subset(salary,rank=="Assoc")
Full<-subset(salary,rank=="Full")

summary(Assistant$salary)
var(Assistant$salary,na.rm = TRUE)
summary(Associate$salary)
var(Associate$salary,na.rm = TRUE)
summary(Full$salary)
var(Full$salary,na.rm = TRUE)

#a(iii)
male_assistant<-subset(Assistant,gender=="M")
summary(male_assistant$salary)
var(male_assistant$salary,na.rm = TRUE)

male_associate<-subset(Associate,gender=="M")
summary(male_associate$salary)
var(male_associate$salary,na.rm = TRUE)

male_full<-subset(Full,gender=="M")
summary(male_full$salary)
var(male_full$salary,na.rm = TRUE)

female_assistant<-subset(Assistant,gender=="F")
summary(female_assistant$salary)
var(female_assistant$salary,na.rm = TRUE)

female_associate<-subset(Associate,gender=="F")
summary(female_associate$salary)
var(female_associate$salary,na.rm = TRUE)

female_full<-subset(Full,gender=="F")
summary(female_full$salary)
var(female_full$salary,na.rm = TRUE)

#boxplot
boxplot(salary~gender,data=salary,main="Salary Data i"
        ,xlab="Gender", ylab="Salary",varwidth=TRUE)
boxplot(salary~rank,data=salary,main="Salary Data ii"
        ,xlab="Rank", ylab="Salary",varwidth=TRUE)
boxplot(salary~gender*rank,data=salary,main="Salary Data iii"
        ,xlab="Combinations of gender and rank", ylab="Salary",
        col=(c("gold","darkgreen")),varwidth=TRUE)

##HW3_Q4
#(1)
QRS <- read_csv("Downloads/hw3q4.csv")
ivt<-subset(QRS,ivt==1)
no_ivt<-subset(QRS,ivt==0)
summary(ivt$qrs)
var(ivt$qrs,na.rm = TRUE)
summary(no_ivt$qrs)
var(no_ivt$qrs,na.rm = TRUE)
#(2)
library("beeswarm")
beeswarm(qrs~ivt,data=QRS,main="beeswarm plot of qrs data",
         xlab="ivt status(0 denotes without ivt)", ylab="qrs time")
#(3)
ivt_abnormal_qrs<-subset(QRS,qrs>120&ivt=="1")
ivt_normal_qrs<-subset(QRS,qrs<=120&ivt=="1")
summary(ivt_abnormal_qrs$qrs)
var(ivt_abnormal_qrs$qrs,na.rm = TRUE)
summary(ivt_normal_qrs$qrs)
var(ivt_normal_qrs$qrs,na.rm = TRUE)

noivt_abnormal_qrs<-subset(QRS,qrs>120&ivt=="0")
noivt_normal_qrs<-subset(QRS,qrs<=120&ivt=="0")
summary(noivt_abnormal_qrs$qrs)
var(noivt_abnormal_qrs$qrs,na.rm = TRUE)
summary(noivt_normal_qrs$qrs)
var(noivt_normal_qrs$qrs,na.rm = TRUE)

##HW3_Q5
library(survival)
survival <- read_csv("Downloads/hw3q5.csv")
y<-Surv(survival$time,survival$event)
kmtrtfit<-survfit(y~treatment,data=survival)
summary(kmtrtfit,times = c(2*(0:7)))
plot(kmtrtfit,xlab = "time",ylab = "Survival",conf.int = FALSE
     ,col=(c("gold","darkgreen")),main="K-M fit")
legend("topright",legend=c("new drug","control drug"),
       col=c("gold","darkgreen"),lty=1,lwd=2)