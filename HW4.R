##HW4_514
##Q1
#a
library(readr)
salary <- read_csv("Downloads/salary.csv")
with(subset(salary,year==95),cor(startyr,salary,use = 'pairwise.complete.obs'))
with(subset(salary,year==95&gender=='F'),cor(startyr,salary,use = 'pairwise.complete.obs'))
with(subset(salary,year==95&gender=='M'),cor(startyr,salary,use = 'pairwise.complete.obs'))
#b
data<-subset(salary[,3:11],year==95)
data<-data[,c(-4,-6,-7)]
data$gender<-as.factor(data$gender)
data$gender<-as.numeric(data$gender)

data$deg<-as.factor(data$deg)
data$deg<-as.numeric(data$deg)

data<-as.data.frame(data)
round(cor(data,use = 'pairwise.complete.obs'),3)
#c
round(cov(data,use = 'pairwise.complete.obs'),3)

##Q2
NHANES <- read.csv("http://faculty.washington.edu/kenrice/heartgraphs/nhaneslarge.csv",
                   na.strings=".")
#a
lm(DR1TFOLA~BMXBMI,data=NHANES)
#b
plot(DR1TFOLA~BMXBMI,data=NHANES,xlab="BMI (kg/m2)",ylab="folate intake (μg/day)",
     pch=19, col= adjustcolor("black", alpha=0.2))
title('plot of folate intake versus BMI')
nhanes_no_na <- subset(NHANES, !is.na(DR1TFOLA)&!is.na(BMXBMI))
data_smooth <- with(nhanes_no_na, lowess(BMXBMI, DR1TFOLA, iter=0))
lines(data_smooth, col="tomato", lwd=2)
abline(a=443.798, b=-1.269,col="blue",lwd=2)
legend("topright",pch=c(19,NA,NA), lty=c(NA,1,1), 
       col=c(adjustcolor("black",alpha=0.2),"tomato","blue"),
       legend=c("data points","lowess smoother","regression"))

##Q3
#a
q3<-read.csv("Downloads/hw4q3.csv")
range0<-round(with(subset(q3,x==0),var(y)),2)
range1<-round(with(subset(q3,x==1),var(y)),2)
range2<-round(with(subset(q3,x==2),var(y)),2)

##Q5
#a
FEV<-read.csv("Downloads/fev.csv")
FEV$smoke<-FEV$smoke-1
lm(fev~smoke,data=FEV)
#b
FEV$age_level<-cut(FEV$age,c(min(FEV$age)-1,9,11,13,max(FEV$age)+1)
                   ,labels = FALSE)
lm(fev~smoke,data=subset(FEV,age_level==1))
lm(fev~smoke,data=subset(FEV,age_level==2))
lm(fev~smoke,data=subset(FEV,age_level==3))
lm(fev~smoke,data=subset(FEV,age_level==4))

##Q6
#a
library("quantreg")
rq(fev~smoke,data=FEV)
#b
rq(fev~smoke,data=subset(FEV,age_level==1),tau = 0.5)
rq(fev~smoke,data=subset(FEV,age_level==2),tau = 0.5)
rq(fev~smoke,data=subset(FEV,age_level==3),tau = 0.5)
rq(fev~smoke,data=subset(FEV,age_level==4),tau = 0.5)
plot(fev~smoke,data=subset(FEV,age_level==1),
     xlab="smoking habits, self-reported (0 = smoker, 1 = non-smoker)"
     ,ylab="measured FEV (liters per second)",
     pch=19, col= adjustcolor("black", alpha=0.2))
title('plot for age group: <=9')

plot(fev~smoke,data=subset(FEV,age_level==3),
     xlab="smoking habits, self-reported (0 = smoker, 1 = non-smoker)"
     ,ylab="measured FEV (liters per second)",
     pch=19, col= adjustcolor("black", alpha=0.2))
title('plot for age group: 12-13')

boxplot(fev~smoke,data=subset(FEV,age_level==4),
        xlab="smoking habits, self-reported (0 = smoker, 1 = non-smoker)"
        ,ylab="measured FEV (liters per second)")
title('boxplot for age group: >=14')
