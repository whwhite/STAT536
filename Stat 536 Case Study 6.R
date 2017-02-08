library(bestglm)
library(xtable)

crash<-read.table("~/STAT GRAD/Crash.txt",header=TRUE)
crash$CASENUM <- NULL
crash[crash$TYP_INT==10,5] <- 1 # Make these 1's
crash[crash$TYP_INT==6|crash$TYP_INT==7|crash$TYP_INT==5,5] <- "U"

#weather
nrow(crash)
crash <- crash[-which(crash$WEATHER==12),]

# Air_bag
crash[crash$AIR_BAG==7,7] <- 9 # change other to unknown

# VNUM_LAN
crash[crash$VNUM_LAN==7,] # maybe combine 

# VSURCOND # 5 is sand, 8 is other, 11 is mud dirt gravel, 6 is water
# combine 5, 11, and 8
crash[crash$VSURCOND==5|crash$VSURCOND==11|crash$VSURCOND==8,13] <- "debris"

crash[,2]<-as.factor(crash[,2])
crash[,3]<-as.factor(crash[,3])
crash[,4]<-as.factor(crash[,4])
crash[,5]<-as.factor(crash[,5])
crash[,6]<-as.factor(crash[,6])
crash[,7]<-as.factor(crash[,7])
crash[,9]<-as.factor(crash[,9])
crash[,12]<-as.factor(crash[,12])
crash[,13]<-as.factor(crash[,13])


Xy<-cbind(modx[,-1],crash[,c(-1,-8)],crash[,8])
subsel.crash.ns<-bestglm(Xy,family = binomial)
subsel.crash.aic<-bestglm(Xy,family = binomial,IC="AIC")
# subsel.crash.cv<-bestglm(Xy,family = binomial,IC="CV") Error in bestglm(Xy, family = binomial, IC = "CV") : Cross-validation not available when there are categorical variables with more than 2 levels!

summary(subsel.crash$BestModel)
summary(subsel.crash.ns$BestModel)

table(crash2$HOUR)
table(crash2$LGT_COND)
table(crash2$WEATHER)

table(crash2$VSURCOND)


scatter.smooth(crash$VSPD_LIM,crash$SEVERITY)

# Fit the model
levels(crash$ALCOHOL)<-c("Alcohol","No Alcohol")
levels(crash$REST_USE)<-c("NA","Shoulder Only","Lap Only","Lap & Shoulder","Motorcycle Helmet","None","Used but Unknown")
levels(crash$AIR_BAG)<-c("NA","Front","Side","Roof","Combo","Unknown Deployed","Not Deployed")

ALCOHOL2<-factor(crash$ALCOHOL,c("No Alcohol","Alcohol"))
REST_USE2<-factor(crash$REST_USE,c("None","Shoulder Only","Lap Only","Lap & Shoulder","Motorcycle Helmet","NA","Used but Unknown"))
AIR_BAG2<-factor(crash$AIR_BAG,c("Not Deployed","Front","Side","Roof","Combo","Unknown Deployed","NA"))

fit.crash<-glm(SEVERITY~ns(HOUR,df=3)+ALCOHOL2+REST_USE2+AIR_BAG2+VSPD_LIM,data = crash,family=binomial)

fit.crash2<-glm(SEVERITY~ns(HOUR,df=3)+ALCOHOL2+REST_USE2+AIR_BAG2+VSPD_LIM + AIR_BAG2:VSPD_LIM,data = crash,family=binomial)
summary(fit.crash)
nullmod<-glm(SEVERITY~1,data = crash,family = "binomial")

# R-squared
1-(logLik(fit.crash)/logLik(nullmod))




pred.sev<-predict.glm(fit.crash,se.fit = TRUE,type = "link")
pred.sev<-predict(fit.crash,type = "response")


# 0.5 cutoff
pred.sev.5<-as.numeric(pred.sev>=0.5)

pred.err.5<-pred.sev.5-crash$SEVERITY
table(pred.err.5)


# 0.3 cutoff
pred.sev.3<-as.numeric(pred.sev>=0.3)

pred.err.3<-pred.sev.3-crash$SEVERITY
table(pred.err.3)
sum(pred.err.3==0)


# Error rates at different cutoffs
pred.err.rates<-matrix(nrow = 100,ncol = 2)

for (i in 1:100){
  pred.sev.all<-as.numeric(pred.sev>=(i/100))
  pred.err<-pred.sev.all-crash$SEVERITY
  num.false.neg<-sum(pred.err==-1)
  num.false.pos<-sum(pred.err==1)
  counts<-c(num.false.neg,num.false.pos)
  pred.err.rates[i,]<-counts/8602
}

#plot(seq(0.01,1,by=0.01),pred.err.rates[,1],type='l',ylim = c(0,0.6))
#lines(seq(0.01,1,by=0.01),pred.err.rates[,3])

plot(seq(0.01,1,by=0.01),pred.err.rates[,2],type='l',col='blue',xlab="Cutoff Value",ylab = "Error Rate",lwd=2)
lines(seq(0.01,1,by=0.01),pred.err.rates[,1],col='red',lwd=2)
legend("top",legend = c("False Positive","False Negative"),col = c("blue","red"),lty = 1,lwd = 2,cex = 0.9,pt.cex = 1)

#0.445 cutoff
pred.sev.445<-as.numeric(pred.sev>=0.445)

pred.err.445<-pred.sev.445-crash$SEVERITY
table(pred.err.445)

# Confusion matrix
con.mat.445<-data.frame(rbind(c(2519,1535),c(1548,3000)),row.names = c("True Yes","True No"))
names(con.mat.445)<-c("Predicted Yes","Predicted No")
xtable(con.mat.445)

#0.35 cutoff
pred.sev.35<-as.numeric(pred.sev>=0.35)

pred.err.35<-pred.sev.35-crash$SEVERITY
table(pred.err.35)


library(splines)
mod<-lm(SEVERITY~ns(HOUR,df=3),crash)
modx<-model.matrix(SEVERITY~ns(HOUR,df=3),crash)




fit.crash.sum <- summary(fit.crash)
upper <- fit.crash.sum$coef[,1] + qnorm(0.975)*fit.crash.sum$coef[,2]
lower <- fit.crash.sum$coef[,1] - qnorm(0.975)*fit.crash.sum$coef[,2]
coef.table<-exp(cbind(fit.crash.sum$coef[,1],lower,upper))
xtable(coef.table)
