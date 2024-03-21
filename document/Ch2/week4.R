###### String ######


A <- matrix(11:16, nrow=3, ncol=2, byrow=T)
print(A)

rowSums(A)
rowMeans(A)
colSums(A)
colMeans(A)

dim(A)
ncol(A)
nrow(A)

A[1,2]
A[1,]
A[,1]

A[1:2,]
A[c(1,3),]
A[c(1,3),1:2]

colnames(A) = c("x1","x2")
rownames(A) = c("ID1", "ID2", "ID3")
dim(A)

A[,'x2']
hist(A[,'x2'])

A[A[,'x1']>=2,'x1']  

tidyverse

# To join matrices
cbind(c(1,2), c(3,4), c(5,6))
A
cbind(A,A)
rbind(A,A)

A + A
3 * A
t(A)

B = A %*% t(A)
det(B) # determinant: hanglieshi
solve(B)

A^2
A / A

# generate your dataset (X,Y)
e = rnorm(100,0,1)
hist(e)

beta = c(1,1,1)
X = matrix(rnorm(200,1,1),
           nrow=100, ncol=2, byrow=T)

X = cbind( rep(1,100), X)
Y = X %*% beta + e # your model: y = xB + e
bethat = solve(t(X) %*% X) %*% t(X) %*% Y
bethat
rownames(bethat) = c('intercept', 'beta1', 'beta2')
bethat

# apply()
class(X)
X

apply(X, 1, sum) # 1 for row operation
apply(X, 1, sum) == rowSums(X)
cor(apply(X, 1, sum), rowSums(X)) #

XXXX = function(x){
  x^2
}

apply(X, 1, XXXX)

apply(X, 2, var)

### tapply(), sapply(), mapply()

# bing how to generate random numbers in r stackoverflow
# 多元统计分析

### if and for loop ###

a = c(1,2,4,6,8)

a = 8

if(a > 3){
  print(a)
}

a[a>3]

ifelse(a>3, a, 0)

if(a > 3){
  print(a)
}
# else print('warning')

### for
for(i in 1:length(a)){
  print(a[i]^2)
}#五次结果


nsample = 500
n = 1000
Xmean = rep(0, nsample)

apply(rnorm(100,1,1),2,mean)

rep(rnorm(100,1,1),500)
cbind(rnorm(100,1,1)

for (i in 1:nsample){
  X = rnorm(100,1,1)
  Xmean[i] = mean(X)
}
Xmean
mean(Xmean)
var(Xmean)

hist(Xmean)











