#HW2_Q4
library(readr)
bodyfat <- read_csv("Downloads/bodyfat.csv") 
View(bodyfat)
#a
boxplot(x = bodyfat$weight,horizontal = TRUE,
        main = 'men’s body weights (in pounds)') 
outliers <- subset(bodyfat,weight>250)
#b
boxplot(x = bodyfat$height,horizontal = TRUE,
        main = 'men’s heights (in inches)') 
outliers2 <- subset(bodyfat,height<40)
#c
weight<-bodyfat$weight[c(-39)] 
height<-bodyfat$height[c(-42)] 
mean1<-mean(weight) 
mean2<-mean(height) 
median1<-median(weight) 
median2<-median(height) 
geomean1<-exp(mean(log(weight))) 
geomean2<-exp(mean(log(height)))