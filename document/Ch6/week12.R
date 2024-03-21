library(glmnet)
library(ISLR2)
library(MASS)
## 载入数据
Hitters = na.omit(Hitters)
names(Hitters)
## AIC

model1 = lm(Salary~., data=Hitters)
summary(model1)
step(model1)
stepAIC(model1, direction="forward")

## ridge regression
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

model.ridge = glmnet(x, y, alpha=0)
plot(model.ridge) # 天然不能做变量选择

model.lasso = glmnet(x, y, alpha=1)
plot(model.lasso, label=T,xvar="norm")

grid = 10^seq(10,-2,length=100) #lamda candidate values
set.seed(1) # reproduce data on different terminals
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,], y[train],alpha=0, 
                 # = cv.out$lambda.min) #,
                 lambda=grid)
summary(ridge.mod)
coef(ridge.mod)
predict(ridge.mod, x[test, ])

cv.out = cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out) # plot of MSE by different Lambda
bestlam = cv.out$lambda.min
bestlam
plot(ridge.mod, label=T, xvar="lambda")

ridge.mod = glmnet(x, y, alpha=0, lambda = bestlam)
coef(ridge.mod)
predict(ridge.mod, x[test, ])

# lasso 
# Q1: best lambda for lasso
ridge.mod=glmnet(x[train,], y[train],alpha=1, 
                 # = cv.out$lambda.min) #,
                 lambda=grid)
cv.out = cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out) # plot of MSE by different Lambda
bestlam = cv.out$lambda.min
bestlam
plot(ridge.mod, label=T, xvar="norm")
# Q2: estimated coefficients
ridge.mod = glmnet(x, y, alpha=0, lambda = bestlam)
coef(ridge.mod)
predict(ridge.mod, x[test, ])

































