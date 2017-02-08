library(splines)
car<-read.csv("/Users/William/STAT GRAD/Cars.csv")
car$cc[81]<-1600


#### K-fold CV  ####
library(boot)

### Miles ###
set.seed(38)
cv.error.10<-rep(0,10)
for(i in 1:10){
  glm.fit<-glm(Price~ns(Miles,df=i+1),data=car)
  cv.error.10[i]<-cv.glm(car,glm.fit,K=20)$delta[1]
}
cv.error.10
plot(1:10,cv.error.10,type="b",xlab="Number of Knots",ylab ="Cross Validation Error",main="Cross Validation to Determine Number of Knots")
points(2,cv.error.10[2],col="red",pch=4,cex=2)

### Age ###
set.seed(38)
cv.error.10.p2<-rep(0,10)
for(i in 1:10){
  glm.fit2<-glm(Price~ns(Age_08_04,df=i+1),data=car)
  cv.error.10.p2[i]<-cv.glm(car,glm.fit2,K=30)$delta[1]
}
cv.error.10.p2
plot(1:10,cv.error.10.p2,type="b",xlab="Number of Knots",ylab="Cross Validation Error",main="Cross Validation to Determine Number of Knots")



### Splines for Miles  ####
Mileslims<-range(car$Miles)
Miles.grid<-seq(from=Mileslims[1],to=Mileslims[2])

fit<-lm(Price~bs(Miles,knots = c(26720,39380,54070)),data=car)
pred<-predict(fit,newdata = list(Miles=Miles.grid),se=T)
plot(car$Miles,car$Price,col="gray")
lines(Miles.grid,pred$fit,lwd=2)
lines(Miles.grid,pred$fit+2*pred$se.fit,lwd=2,lty="dashed",col="red")
lines(Miles.grid,pred$fit-2*pred$se.fit,lwd=2,lty="dashed",col="red")

summary(fit)
plot(fit)


fit2<-lm(Price~ns(Miles,df=3),data=car)
pred2<-predict(fit2,newdata = list(Miles=Miles.grid),se=T)

plot(car$Miles,car$Price,col="gray",xlab="Miles",ylab="Price",main="Natural Spline of Miles with 2 Knots")
lines(Miles.grid,pred2$fit,lwd=2)
lines(Miles.grid,pred2$fit+2*pred2$se.fit,lwd=2,lty=3,col="red")
lines(Miles.grid,pred2$fit-2*pred2$se.fit,lwd=2,lty=3,col="red")
abline(v=31052,col="black",lty="dashed")
abline(v=48096,col="black",lty="dashed")

summary(fit2)
plot(fit2)

### Splines for Age  ####  Don't worry about it 
Agelims<-range(car$Age_08_04)
Age.grid<-seq(from=Agelims[1],to=Agelims[2])

fit3<-lm(Price~bs(Age_08_04,knots = c(44,61,70)),data=car)
pred3<-predict(fit3,newdata = list(Age_08_04=Age.grid),se=T)
plot(car$Age_08_04,car$Price,col="gray")
lines(Age.grid,pred3$fit,lwd=2)
lines(Age.grid,pred3$fit+2*pred3$se.fit,lwd=2,lty="dashed",col="red")
lines(Age.grid,pred3$fit-2*pred3$se.fit,lwd=2,lty="dashed",col="red")

fit4<-lm(Price~Age_08_04,data=car)
pred4<-predict(fit4,newdata = list(Age_08_04=Age.grid),se=T)
abline(fit4)


fit5<-lm(Price~ns(Age_08_04,df=9 ),data=car)
pred5<-predict(fit5,newdata = list(Age_08_04=Age.grid),se=T)
plot(car$Age_08_04,car$Price,col="gray")
lines(Age.grid,pred5$fit,lwd=2)
lines(Age.grid,pred5$fit+2*pred5$se.fit,lwd=2,lty="dashed",col="red")
lines(Age.grid,pred5$fit-2*pred5$se.fit,lwd=2,lty="dashed",col="red")



#### GAM  ###
## Use natural spline for miles, linear for weight and others as category.  Use year instead of age and don't use cylinders. 


library(mgcv)
gam.car1<-lm(Price~ns(Miles,df=3)+ns(Age_08_04,df=9),data=car)
gam.car2<-lm(Price~ns(Miles,df=3)+ns(Age_08_04,df=9)+Weight,data=car)
gam.car3<-lm(Price~ns(Miles,df=3)+ns(Age_08_04,df=9)+Weight+Automatic_airco,data=car)

# Prediction
par(mfrow=c(1,1))
set.seed(38)
train<-sample(nrow(car),nrow(car)*0.95)
fit.car<-lm(Price~ ...   ,data=car,subset=train)
test <- data.frame(car[-train,])
prd <- predict.lm(fit.car,test,interval=c("predict"))
plot(1:72,predict(fit.car,test),ylim=c(2000,35000),main="Prediction Performance of Model",xlab="Observation number",ylab="Predicted balance")
segments(1:72,prd[1:72,2],1:72,prd[1:72,3],col="red")
points(1:72,test$Price,col="blue",pch=4)
legend("topright",legend=c("Predicted Balance","Actual Balance","Prediction Interval"),col=c("black","blue","red"),pch=c(1,4,NA),lty=c(0,0,1))


estim<-summary(mod)$coef[,1]
std<-summary(mod)$coef[,2]
ub<-estim+(1.96*std)
lb<-estim+(-1.96*std)
