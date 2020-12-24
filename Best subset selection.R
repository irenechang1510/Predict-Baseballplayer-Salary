library(ISLR)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))
library(leaps)
regfit <- regsubsets(Salary ~., df, nvmax=19)
regfit.summary <- summary(regfit)
#plot statistics against np. of variables
par(mfrow=c(2,2))
plot(regfit.summary$rss ,xlab="Number of Variables ",ylab="RSS",
			 type="l")
plot(regfit.summary$adjr2 ,xlab="Number of Variables ",
			 ylab="Adjusted RSq",type="l")
which.max(regfit.summary$adjr2)
points(11,regfit.summary$adjr2[11], col="red",cex=2,pch=20)
plot(regfit.summary$cp ,xlab="Number of Variables ",ylab="Cp", type="l")
which.min(regfit.summary$cp)
points(10,regfit.summary$cp [10],col="red",cex=2,pch=20)
which.min(regfit.summary$bic)
plot(regfit.summary$bic ,xlab="Number of Variables ",ylab="BIC",
		 type="l")
points(6,regfit.summary$bic [6],col="red",cex=2,pch=20)
#plot to see which subset size is best
plot(regfit, scale="r2")
plot(regfit,scale="adjr2")
plot(regfit,scale="Cp")
plot(regfit,scale="bic")
#see the coefficients
coef(regfit,6)

#forward backward
regfit.fwd=regsubsets (Salary ~.,data=Hitters ,nvmax=19, method ="forward")
sfw <- summary(regfit.fwd)
regfit.bwd=regsubsets (Salary ~.,data=Hitters ,nvmax=19,
												 method ="backward")
sbw<- summary(regfit.bwd)

#nhận xét: ở best subset thì ở mỗi subset size sẽ có những combination khác nhau, còn ở forward and backward thì các variable sẽ luôn xuất hiện từ lúc bắt đầu đc select, aka có một variable sẽ luôn có dấu *

#using cross-validation instead of statistics - just tham khảo vì nó chưa đúng
set.seed (1)
train=sample(c(TRUE,FALSE), nrow(Hitters),replace = TRUE)
test =(! train )
regfit.best=regsubsets(Salary ~.,data=Hitters[train,], nvmax =19)
test.mat=model.matrix(Salary~.,data=Hitters[test,])
val.errors=rep(NA,19)
for(i in 1:19){
	coefi=coef(regfit.best,id=i)
	pred=test.mat[,names(coefi)]%*%coefi
	val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}

names(regfit.best) #call
regfit.best$call[[2]]

coef(regfit.best ,10)

predict.regsubsets<- function(model, newdata, id,...){
	form = as.formula(model$call[[2]])
	mat = model.matrix(form, newdata)
	coefi = coef(model, id = id) # id: which model out of 19 models obtained
	xvars = names(coefi)
	mat[,xvars]%*%coefi
}

k = 4
set.seed(4)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))

for(j in 1:k){
	best.fit = regsubsets(Salary ~., data = Hitters[folds!=j,], nvmax=19)
	for (i in 1:19){ 
		pred = predict(best.fit,Hitters[folds==j,],id=i)
		cv.errors[j, i]= mean((Hitters$Salary[folds==j]-pred)^2)
	}
}
cv.errors

mean.cv.errors=apply(cv.errors ,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors ,type='b')