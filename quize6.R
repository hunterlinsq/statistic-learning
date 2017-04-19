set.seed(123)
xmat <- matrix(NA,4000,10)
for(i in 1:10){
        xmat[,i] <- rnorm(4000)
}
y <- ifelse(apply(xmat,1,function(x) sum(x^2))>9.34,1,0)
dat <- data.frame(cbind(y,xmat))
dat$y <- as.factor(dat$y)
for(i in 2:11){
        colnames(dat)[i] <- paste("x",i-1,sep = "")
}

train <- sample(1:4000,2000)


library(tree)
library(randomForest)
library(gbm)

#boosting
boost.error <- rep(NA,200)
for(i in 1:200){
        boost.model <- gbm(y~.,data = dat[train,],distribution = "adaboost",n.trees = i,interaction.depth = 1)
        yhat.boost <- predict(boost.model,newdata = dat[-train,-1],n.trees = i,type = "response")
        yhat.boost <- ifelse(yhat.boost>0.5,1,0)
        boost.error[i] <- sum(yhat.boost!=dat[-train,]$y)/2000
}

plot(1:200,boost.error,main="boosting",type = "l")
abline(h=boost.error[1])

#bagging
bag.error <- rep(NA,200)
for(i in 1:200){
        bag.model <- randomForest(y~.,data = dat[train,],ntree=i,mtry=10,importance=TRUE)
        yhat.bag <- predict(bag.model,newdata = dat[-train,-1])
        yhat.table <- table(yhat.bag,dat[-train,]$y)
        bag.error[i] <- 1-(yhat.table[1,1]+yhat.table[2,2])/2000
}
plot(1:200,bag.error,main="bagging",type = "l")

#randomforest
rf.error <- rep(NA,200)
for(i in 1:200){
        rf.model <- randomForest(y~.,data = dat[train,],ntree=i,mtry=sqrt(10),importance=TRUE)
        yhat.rf <- predict(rf.model,newdata = dat[-train,-1])
        yhat.table <- table(yhat.bag,dat[-train,]$y)
        rf.error[i] <- 1-(yhat.table[1,1]+yhat.table[2,2])/2000
}
plot(1:200,rf.error,main="randomforest",type = "l")



