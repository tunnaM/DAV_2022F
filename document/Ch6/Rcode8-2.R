### Shrinkage methods
library(ISLR)
library(glmnet)

Hitters=na.omit(Hitters)

x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

# grid=10^seq(10,-2,length=100)
set.seed(1)

train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,], y[train],alpha=0, 
                 lambda = cv.out$lambda.min) #,lambda=grid)
plot(ridge.mod, label=T, xvar="lambda")
plot(ridge.mod, label=T, xvar="norm")


coef(ridge.mod)
predict(ridge.mod, x[test, ])



cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out) # plot of MSE by different Lambda
bestlam=cv.out$lambda.min
bestlam

M.ridge <- glmnet(x[train,],y[train],alpha=0)
plot(M.ridge, label=T, xvar="lambda") #Solution path
plot(M.ridge, label = T, xvar="norm")

#### LASSO
cv.lasso <- cv.glmnet(x[train,], y[train], alpha=1/2)
plot(cv.lasso)

M.lasso <- glmnet(x[train,],y[train],alpha=1, 
                  lambda=cv.lasso$lambda.min)
coef(M.lasso) # variable selected given lambda
predict(M.lasso, x[test, ])


M.lasso <- glmnet(x[train,],y[train],alpha=1)
plot(M.lasso, label=T, xvar="lambda") #Solution path
plot(M.lasso, label = T, xvar="norm")
predict(M.lasso, x[test,])[,5] # predict response given lambda





ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

predict(ridge.mod,s=50,type="coefficients")[1:20,]




ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T,x=x[test,],y=y[test])
mean((ridge.pred-y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients",x=x[test,],y=y[test])[1:20,]




ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]
