library(gridExtra)  ##支持ggplot2多图并列
library(ggplot2)
rm(list=ls())
data("diamonds")
head(diamonds)

set.seed(123456) 
dsmall <- diamonds[sample(nrow(diamonds),100),]

attach(dsmall)
p1 <- qplot(carat, price)  #价格和重量之间的关系
p2 <- qplot(log(carat), log(price))  #变量变换
p3 <- qplot(carat,x*y*z)  #变量组合,体积和重量之间关系
grid.arrange(p1,p2,p3, ncol=3)

qplot(carat, data=dsmall, facets=color~cut)

p4 <- qplot(carat, price, color=color) #将color变量映射到点的颜色
p5 <- qplot(carat, price, shape=cut)  # 将cut变量映射到点的形状
grid.arrange(p4,p5,ncol=2)

p6 <- qplot(carat, price, data=dsmall, geom=c("point", "smooth"), se=FALSE)
grid.arrange(p5, p6, ncol=2)

qplot(carat, price, data=dsmall, geom=c("point","smooth"), span=0.2)


p9 <- qplot(carat, price, data=dsmall, 
            geom=c("point","smooth"), method="lm")
library(splines)
p10 <- qplot(carat, price, data=dsmall, 
             geom=c("point","smooth"), method="lm", 
             formula=y~ns(x,5))
grid.arrange(p9,p10, ncol=2)



p13 <- qplot(carat, data=diamonds, geom="histogram")  #直方图
p14 <- qplot(carat, data=diamonds, geom="density")  #密度曲线图
grid.arrange(p13, p14, ncol=2)

p15 <- qplot(carat, data=diamonds, geom="density", color=color)
p16 <- qplot(carat, data=diamonds, geom="histogram", fill=color)
#等式后面的color是数据集中的变量color钻石颜色，按color分组。
grid.arrange(p15,p16,ncol=2)


p21 <- qplot(carat, data=diamonds, 
             facets = color ~ ., 
             geom = "histogram", 
             binwidth=0.1, 
             xlim = c(0,3))
p22 <- qplot(carat, ..density.., 
             data=diamonds, 
             facets = color ~ ., 
             geom = "histogram", 
             binwidth=0.1, 
             xlim = c(0,3))  
# ..density..告诉ggplot2将密度而不是频数映射到y轴。
grid.arrange(p21, p22, ncol=2)
#左图展示的是频数，右图展示的是频率。在比较不同组分布时，频率图不受该组样本量大小的影响。图形显示，高质量的钻石（颜色D)在小尺寸分布上是偏斜的，但随着质量下降，重量分布越来越平坦。



p23 <- qplot(carat, price, data=dsmall,xlab ="Weight(carats)" , ylab="Price($)", 
        main="Price-weight relationship")
p24 <- qplot(carat, price/carat, data=dsmall,xlab = "Weight(carats)", 
             ylab=expression(frac(price,carat)),
             main="Price-weight relationship",xlim=c(.2,1))
p25 <- qplot(carat, price, data=dsmall, log="xy")
grid.arrange(p23,p24,p25,ncol=3)

# airquality
ggplot(data = airquality,aes(x=Wind,y=Temp)) + geom_point(color='green')
ggplot(data = airquality,aes(x=Wind,y=Temp)) + geom_point(aes(color=factor(Month)))

ggplot(data=airquality,aes(x=Wind,y=Temp)) + geom_point() + geom_smooth()

# 使用stat也可以实现
ggplot(data=airquality,aes(x=Wind,y=Temp)) + geom_point() + stat_smooth()

# 指定回归方法为线性回归，关闭置信区间显示
ggplot(data=airquality,aes(x=Wind,y=Temp)) +
  geom_point() +
  stat_smooth(method='lm',se=FALSE)


# 每个月对应一条不同颜色的回归线
ggplot(data=airquality,aes(x=Wind,y=Temp)) + 
  stat_smooth(method='lm',se=FALSE,aes(color=factor(Month)))

# 对总体绘制回归线，再按照月份绘制回归线
ggplot(data=airquality,aes(x=Wind,y=Temp)) +
  geom_point(alpha=0.7,size=0.5) +
  stat_smooth(method='lm',se=FALSE,aes(group=1),color='yellow') +
  stat_smooth(method='lm',se=FALSE,aes(color=factor(Month)))


# 按照月份分成五个面板
ggplot(data=airquality,aes(x=Wind,y=Temp)) +
  geom_point(alpha=0.7,size=0.5) +
  stat_smooth(method='lm',se=FALSE,aes(color=factor(Month))) +
  facet_grid(.~Month)



#### SVM with ggplot2
#number of datapoints
n <- 200
#Generate dataframe with 2 uniformly distributed predictors x1 and x2 in (0,1)
df <- data.frame(x1=runif(n),x2=runif(n))
p <- ggplot(data=df, aes(x=x1,y=x2)) + geom_point()
#display it
p

# if x1>x2 then -1, else +1
df$y <- factor(ifelse(df$x1-df$x2>0,-1,1),levels=c(-1,1))

p <- ggplot(data=df, aes(x=x1,y=x2,colour=y)) + geom_point() + 
  scale_colour_manual(values=c("-1"="red","1"="blue"))
#add decision boundary
p <- p + geom_abline(slope=1,intercept=0)
#display plot
p

#create a margin of 0.05 in dataset
delta <- 0.05
# retain only those points that lie outside the margin
df1 <- df[abs(df$x1-df$x2)>delta,]
#check number of datapoints remaining
nrow(df1)

p <- ggplot(data=df1, aes(x=x1,y=x2,colour=y)) + geom_point() + 
  scale_colour_manual(values=c("-1"="red","1"="blue"))
#add decision boundary
p <- p + geom_abline(slope=1,intercept=0)
#display plot
p
#add margins to plot object created earlier
p <- p + geom_abline(slope=1,intercept = delta, linetype="dashed") + 
  geom_abline(slope=1,intercept = -delta, linetype="dashed")
#display plot
p




library(e1071)
#set seed for random number generation
set.seed(1)
#split train and test data 80/20
df[,"train"] <- ifelse(runif(nrow(df))<0.8,1,0)
trainset <- df[df$train==1,]
testset <- df[df$train==0,]
#find "train" column index
trainColNum <- grep("train",names(trainset))
#remove column from train and test sets
trainset <- trainset[,-trainColNum]
testset <- testset[,-trainColNum]

svm_model <- svm(y ~ ., data=trainset, type="C-classification", kernel="linear", scale=FALSE)
pred_train <- predict(svm_model,trainset)
mean(pred_train==trainset$y)
#test accuracy
pred_test <- predict(svm_model,testset)
mean(pred_test==testset$y)
svm_model
svm_model$index
#Support vectors
svm_model$SV
#build plot of training set, distinguishing classes by colour as before
p <- ggplot(data=trainset, aes(x=x1,y=x2,colour=y)) + geom_point()+ scale_colour_manual(values=c("red","blue"))
#identify support vectors in training set
df_sv <- trainset[svm_model$index,]
#add layer marking out support vectors with semi-transparent purple blobs
p <- p + geom_point(data=df_sv,aes(x=x1,y=x2),colour="purple",size = 4,alpha=0.5)
#display plot
p


w <- t(svm_model$coefs) %*% svm_model$SV
slope_1 <- -w[1]/w[2]
slope_1
#calculate intercept
intercept_1 <- svm_model$rho/w[2]
intercept_1

plot(x=svm_model, data=trainset)

#p created in earlier code block
p <- p + geom_abline(slope=slope_1,intercept = intercept_1-1/w[2], linetype="dashed")+
  geom_abline(slope=slope_1,intercept = intercept_1+1/w[2], linetype="dashed")
#display plot
p

svm_model <- svm(y ~ ., data=trainset, 
                 type="C-classification", kernel="linear", cost=100, scale=FALSE)
w <- t(svm_model$coefs) %*% svm_model$SV
#calculate slope
slope_100 <- -w[1]/w[2]
slope_100
#calculate intercept
intercept_100 <- svm_model$rho/w[2]
intercept_100


