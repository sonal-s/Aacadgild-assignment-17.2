gmdata<-read.csv("GMdata.csv",header = T)
View(gmdata)
class(gmdata)
names(gmdata)
# MODEL 1, MLR for price forecast
gmmod1<-lm(Price~.,data=gmdata)
gmmod1
summary(gmmod1)
gmmod1$residuals
par(mfrow=c(2,2))
plot(gmmod1)
gm1res<-data.frame(gmdata,fittedval=fitted(gmmod1),resi=resid(gmmod1))
gm1res
head(gm1res)
library(MASS)
boxcox(lm(Price~.,data=gmdata),lambda=seq(-2,2,by=.1))
plot(gm1res$Mileage,gm1res$resi)
plot(gm1res$Cylinder,gm1res$resi)
plot(gm1res$Liter,gm1res$resi)


#1/sqrt(Price) lambda when -0.5
gmmod2<-lm((1/sqrt(Price))~.,data=gmdata)
gmmod2
summary(gmmod2)
gmmod2$residuals
par(mfrow=c(2,2))
plot(gmmod2)
gm2res<-data.frame(gmdata,fittedval=fitted(gmmod2),resi=resid(gmmod2))
gm2res
head(gm2res)
plot(gm2res$Mileage,gm2res$resi)
plot(gm2res$Cylinder,gm2res$resi)
plot(gm2res$Liter,gm2res$resi)




# MODEL 3
##log(Price)
gmmod3<-lm(log(Price)~.,data=gmdata)
gmmod3
summary(gmmod3)
gmmod3$residuals
par(mfrow=c(2,2))
plot(gmmod3)
gm3res<-data.frame(gmdata,fittedval=fitted(gmmod2),resi=resid(gmmod2))
gm3res
head(gm3res)
plot(gm3res$Mileage,gm3res$resi)
plot(gm3res$Cylinder,gm3res$resi)
plot(gm3res$Liter,gm3res$resi)


# MODEL 4
gmdata[62,]
boxplot(gmdata$Price)
gmdata31<-gmdata[-62,]
gmmod4<-lm(Price~.,data=gmdata31)
summary(gmmod4)
par(mfrow=c(2,2))
plot(gmmod4)


