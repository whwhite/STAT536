library(leaps)
library(car)

credit<-read.csv("/Users/William/Downloads/Credit.csv")
credit2<-credit[,c(-1,-3)]

set.seed(38)
train.credit<-sample(c(TRUE,FALSE),nrow(credit),rep=TRUE)
test.credit<-(!train.credit)

regfit.best<-regsubsets(Balance~.,data=credit[train.credit,],nvmax=11)
test.mat<-model.matrix(Balance~.,data=credit[test.credit,])

val.errors<-rep(NA,11)
for(i in 1:11){
  coefi<-coef(regfit.best,id=i)
  pred<-test.mat[,names(coefi)]%*%coefi
  val.errors[i]<-mean((credit$Balance[test.credit]-pred)^2)
}

which.min(val.errors)
coef(regfit.best,4)


predict.regsubsets<-function(object,newdata,id,...){
  form<-as.formula(object$call[[2]])
  mat<-model.matrix(form,newdata)
  coefi<-coef(object,id=id)
  xvars<-names(coefi)
  mat[,xvars]%*%coefi
}


### K-folds
k<-10
set.seed(38)
folds<-sample(1:k,nrow(credit),replace=TRUE)
cv.errors<-matrix(NA,k,11,dimnames=list(NULL,paste(1:11)))

for(j in 1:k){
  best.fit<-regsubsets(Balance~.,data=credit[folds!=j,],nvmax=11)
  for(i in 1:11){
    pred<-predict.regsubsets(best.fit,credit[folds==j,],id=i)
    cv.errors[j,i]<-mean( (credit$Balance[folds==j]-pred)^2)
  }
}

mean.cv.errors<-apply(cv.errors,2,mean)

plot(mean.cv.errors,type='b')
reg.best<-regsubsets(Balance~.,data=credit,nvmax=11)
coef(reg.best,4)


############################
###### Without Limit #######
############################
set.seed(38)
train.credit2<-sample(c(TRUE,FALSE),nrow(credit2),rep=TRUE)
test.credit2<-(!train.credit2)

regfit.best<-regsubsets(Balance~.,data=credit2[train.credit2,],nvmax=10)
test.mat<-model.matrix(Balance~.,data=credit2[test.credit2,])

val.errors<-rep(NA,10)
for(i in 1:10){
  coefi<-coef(regfit.best,id=i)
  pred<-test.mat[,names(coefi)]%*%coefi
  val.errors[i]<-mean((credit2$Balance[test.credit2]-pred)^2)
}

which.min(val.errors)
coef(regfit.best,4)


predict.regsubsets<-function(object,newdata,id,...){
  form<-as.formula(object$call[[2]])
  mat<-model.matrix(form,newdata)
  coefi<-coef(object,id=id)
  xvars<-names(coefi)
  mat[,xvars]%*%coefi
}

k<-10
set.seed(38)
folds<-sample(1:k,nrow(credit2),replace=TRUE)
cv.errors<-matrix(NA,k,10,dimnames=list(NULL,paste(1:10)))

for(j in 1:k){
  best.fit<-regsubsets(Balance~.,data=credit2[folds!=j,],nvmax=10)
  for(i in 1:10){
    pred<-predict.regsubsets(best.fit,credit2[folds==j,],id=i)
    cv.errors[j,i]<-mean( (credit2$Balance[folds==j]-pred)^2)
  }
}

mean.cv.errors<-apply(cv.errors,2,mean)
which.min(val.errors)
coef(regfit.best,3)



################################################
###### Without Limit and no zero balance #######
################################################

credit3<-credit2[credit2$Balance>0,]
set.seed(38)
train.credit3<-sample(c(TRUE,FALSE),nrow(credit3),rep=TRUE)
test.credit3<-(!train.credit3)

regfit.best<-regsubsets(Balance~.,data=credit3[train.credit3,],nvmax=10)
test.mat<-model.matrix(Balance~.,data=credit3[test.credit3,])

val.errors<-rep(NA,10)
for(i in 1:10){
  coefi<-coef(regfit.best,id=i)
  pred<-test.mat[,names(coefi)]%*%coefi
  val.errors[i]<-mean((credit3$Balance[test.credit3]-pred)^2)
}

which.min(val.errors)
coef(regfit.best,4)


predict.regsubsets<-function(object,newdata,id,...){
  form<-as.formula(object$call[[2]])
  mat<-model.matrix(form,newdata)
  coefi<-coef(object,id=id)
  xvars<-names(coefi)
  mat[,xvars]%*%coefi
}

k<-10
set.seed(38)
folds<-sample(1:k,nrow(credit3),replace=TRUE)
cv.errors<-matrix(NA,k,10,dimnames=list(NULL,paste(1:10)))

for(j in 1:k){
  best.fit<-regsubsets(Balance~.,data=credit3[folds!=j,],nvmax=10)
  for(i in 1:10){
    pred<-predict.regsubsets(best.fit,credit3[folds==j,],id=i)
    cv.errors[j,i]<-mean( (credit3$Balance[folds==j]-pred)^2)
  }
}

mean.cv.errors<-apply(cv.errors,2,mean)
plot(mean.cv.errors,type='b',ylab="Mean Cross Validation Error",xlab="Number of Variables",)
points(4,mean.cv.errors[4],col="red",pch=4,cex=2)
which.min(val.errors)
coef(regfit.best,4)

#####################
####### Model #######
#####################
set.seed(38)
train<-sample(nrow(credit3),nrow(credit3)*0.9)

out.credit<-lm(Balance~Income+Rating+Age+Student,data=credit3,subset=train)
summary(out.credit)

out.credit2<-lm(Balance~Income+Rating+Age+Student+Income:Student,data=credit3,subset=train)
summary(out.credit2)

out.credit3<-lm(Balance~Income+Rating+Age+Student,data=credit3)
summary(out.credit3)

#####################
#### Assumptions ####
#####################

## Linearity
avPlots(out.credit3)



## Normality
par(mfrow=c(1,2))
hist(scale(residuals(out.credit3)),main="Histogram of Residuals",xlab="Scaled Residuals")

qqnorm(scale(residuals(out.credit3)))
qqline(scale(residuals(out.credit3)),col="red",lwd=2)


## Equal Variance (1st plot)
par(mfrow=c(1,1))
plot(out.credit3)

### Prediction
test <- data.frame(credit3[-train,])
prd <- predict.lm(out.credit3,test,interval=c("predict"))
plot(1:31,predict(out.credit3,test),ylim=c(-5,2100),main="Prediction Performance of Model",xlab="Observation number",ylab="Predicted balance")
points(1:31,test$Balance,col="blue",pch=4)
segments(1:31,prd[1:31,2],1:31,prd[1:31,3],col="red")
legend("topleft",legend=c("Predicted Balance","Actual Balance","Prediction Interval"),col=c("black","blue","red"),pch=c(1,4,NA),lty=c(0,0,1))

bias<-mean(prd-test$Balance)
mse<-mean((prd-test$Balance)^2)
rmse<-sqrt(mse)

estimate<-as.data.frame(summary(out.credit3)$coefficients[,1])
ci<-as.data.frame(summary(out.credit3)$coefficients[,2])
ci2<-as.data.frame(matrix(nrow=5,ncol=2))
ci2[,1]<-round(estimate+c(-1.96)*ci,2)
ci2[,2]<-round(estimate+c(1.96)*ci,2)
ci3<-paste("(",ci2[,1],",",ci2[,2],")",sep="")

pred.table<-as.data.frame(cbind(estimate,ci3))
names(pred.table)<-c("Estimate","95% CI")
row.names(pred.table)<-c("Intercept","Income","Credit Rating","Age","Student Status")

library(xtable)
xtable(pred.table)

test

