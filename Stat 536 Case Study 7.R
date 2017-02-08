library(tree)
library(randomForest)

torn <- read.csv('~/Downloads/Tornados2012.csv')
torn<-torn[c(-400,-936),]

# get rid of duplicates
dim(torn)
dup <- duplicated(torn$Number)
ind <- which(dup)
repobs<-torn[ind,]
length(unique(torn$Number))

repobs.clean<-data.frame()

for(i in (nrow(repobs)-1)){
  if(repobs[i,1]==repobs[(i+1),1]){
    repobs.clean[i,]<-t(c(repobs[i,1:6],paste(repobs[i,7],repobs[(i+1),7],sep = "/"),max(repobs[i,8],repobs[(i+1),8]),sum(repobs[i,9],repobs[(i+1),9]),sum(repobs[i,10],repobs[(i+1),10]),sum(repobs[i,11],repobs[(i+1),11]),sum(repobs[i,12],repobs[(i+1),12]),repobs[i,13],repobs[i,14],repobs[(i+1),15],repobs[(i+1),16],sum(repobs[i,17],repobs[(i+1),17]),max(repobs[i,18],repobs[(i+1),18])))
  }
}


repob1<-cbind(torn[114,1:10],t(c(0.550,0,37.5000,-89.7300,37.6110,-89.1668,51.65,300)))
names(repob1)<-""
torn<-rbind(torn,repob1)

torn$Fatalities <- as.factor(torn$Fatalities)
torn$Fscale <- as.factor(torn$Fscale)
# plot data
boxplot(Fatalities~Fscale,data=torn)



new.torn <- torn[,c(-1,-2,-3,-4,-5,-6,-7)]
new.torn$Fscale <- as.factor(torn$Fscale)





# cross validation
k <- 10
n <- dim(new.torn)[2] - 1

folds <- sample(1:k, nrow(torn), replace = TRUE)
cv.errors <- matrix(NA, k, n, dimnames = list(NULL, paste(1:n)))

for(j in 1:k){
  cat(j)
  test <- new.torn[folds == j,]
  train <- new.torn[folds !=j,]
  for(i in 1:n){
    rf.torn <- randomForest(Fscale~.,data=train, mtry=i)
    yhat <- predict(rf.torn,newdata=test)
    cv.errors[j,i] <- sum(as.numeric(yhat)-1==as.numeric(test$Fscale)-1)/dim(test)[1]
  }
}

cv <- colMeans(cv.errors) # I was getting mtry=3 as best
final.forest <- randomForest(Fscale~.,data=new.torn, mtry=3)
importance(final.forest)


# ROC curve
final.forest.pr = predict(final.forest,newdata=new.torn)
final.forest.pred = prediction(as.numeric(final.forest.pr)-1, new.torn$Fscale)
adult.rf.perf = performance(adult.rf.pred,"tpr","fpr")
plot(adult.rf.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")



library(randomForest)

torn <- read.csv('~/Downloads/Tornados2012.csv')

# get rid of duplicates
dim(torn)
dup <- duplicated(torn$Number)
ind <- which(dup)
torn[ind,]
length(unique(torn$Number))

new.torn <- torn[-c(116,122,149,168,180,189,200,209,400,936,187),c(-1,-2,-3,-4,-5,-6,-7)]

new.torn$Fscale <- as.factor(new.torn$Fscale)


# cross validation
k <- 10
n <- dim(new.torn)[2] - 1

folds <- sample(1:k, nrow(new.torn), replace = TRUE)
cv.errors <- matrix(NA, k, n, dimnames = list(NULL, paste(1:n)))

for(j in 1:k){
  cat(j)
  test <- new.torn[folds == j,]
  train <- new.torn[folds !=j,]
  for(i in 1:n){
    rf.torn <- randomForest(Fscale~.,data=train, mtry=i)
    yhat <- predict(rf.torn,newdata=test)
    cv.errors[j,i] <- sum(as.numeric(yhat)-1==as.numeric(test$Fscale)-1)/dim(test)[1]
  }
}

cv <- colMeans(cv.errors) # I was getting mtry= as best
mtry <- which.max(cv)
# final model
final.forest <- randomForest(Fscale~.,data=new.torn, mtry=2,proximity=TRUE)

tree.plot<-getTree(final.forest,k=1,labelVar = TRUE)
plot(tree.plot)
plot(final.forest,log="y")
plot.randomForest(final.forest)
MDSplot(final.forest,new.torn$Fscale)

# importance of predictors
importance(final.forest)
varImpPlot(final.forest)

# how well does this predict?
final.forest$confusion

# plot a single tree
single.tree<-tree(Fscale~.,data=new.torn)
plot(single.tree)
text(single.tree,pretty = 0,cex=0.5)

library(party)
test.tree<-ctree(Fscale~.,data=new.torn)
plot(test.tree,type="simple")


getConds<-function(tree){
  #store all conditions into a list
  conds<-list()
  #start by the terminal nodes and find previous conditions
  id.leafs<-which(tree$status==-1)
  j<-0
  for(i in id.leafs){
    j<-j+1
    prevConds<-prevCond(tree,i)
    conds[[j]]<-prevConds$cond
    while(prevConds$id>1){
      prevConds<-prevCond(tree,prevConds$id)
      conds[[j]]<-paste(conds[[j]]," & ",prevConds$cond)
      if(prevConds$id==1){
        conds[[j]]<-paste(conds[[j]]," => ",tree$prediction[i])
        break()
      }
    }
    
  }
  
  return(conds)
}

prevCond<-function(tree,i){
  if(i %in% tree$right_daughter){
    id<-which(tree$right_daughter==i)
    cond<-paste(tree$split_var[id],">",tree$split_point[id])
  }
  if(i %in% tree$left_daughter){
    id<-which(tree$left_daughter==i)
    cond<-paste(tree$split_var[id],"<",tree$split_point[id])
  }
  
  return(list(cond=cond,id=id))
}

#remove spaces in a word
collapse<-function(x){
  x<-sub(" ","_",x)
  
  return(x)
}


data(iris)
require(randomForest)
mod.rf <- randomForest(Species ~ ., data=iris)
tree<-getTree(mod.rf, k=1, labelVar=TRUE)
#rename the name of the column
colnames(tree.plot)<-sapply(colnames(tree.plot),collapse)
rules<-getConds(tree.plot)
print(rules)
plot(rules,type="simple")
