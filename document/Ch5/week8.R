library(gridExtra)  ##支持ggplot2多图并列
library(ggplot2)
rm(list=ls())
data("diamonds")
head(diamonds)

set.seed(123456)
dsmall <- diamonds[sample(nrow(diamonds),100),] # 随机取100行出来
dsmall

attach(dsmall) # 把每一列单独拆开作为变量

carat
color
table(color) # 每种进行汇总

p1 <- qplot(carat, price) # 价格和重量之间的关系
p2 <- qplot(log(carat), log(price)) # 变量变换
p3 <- qplot(carat, x*y*z) # 变量组合，体积和重量之间的关系
grid.arrange(p1, p2, p3, ncol=3) # 来自gridExtra
# 白色画板，灰色画布，
plot(carat, price) # 不使用qqplot

# price clarity scatterplot
p11 <- qplot(clarity, price) # X取值离散
p12 <- qplot(cut, price)
grid.arrange(p11, p12, ncol=2)

qplot(carat,)
p4 <- qplot(carat, price, color=color)
p5 <- qplot(carat, price, shape=cut)
grid.arrange(p4, p5, ncol=2)

# plot(,legend...) 图例
p6 <- qplot(carat, price, data=dsmall, geom=c("point", "smooth"), se=FALSE)
grid.arrange(p5, p6, ncol=2)

qplot(carat, price, data=dsmall, geom="smooth", se=T)
qplot(carat, price, data=dsmall, geom=c("point","smooth"), span=0.2)
# span是做局部估计选择点的个数
p61 <- qplot(carat, geom="histogram")
p62 <- qplot(carat, geom="boxplot")
p63 <- qplot(carat, geom="density")
p64 <- qplot(cut, geom="bar")
grid.arrange(p62, p63, p64, ncol=3)

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
             facets = color ~ ., # 分面
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

# carat. facets using cut
p211 <- qplot(carat, data=diamonds, 
             facets = cut ~ ., # 分面
             geom = "histogram", 
             binwidth=0.1, 
             xlim = c(0,3))
p221 <- qplot(carat, ..density.., 
             data=diamonds, 
             facets = cut ~ ., 
             geom = "histogram", 
             binwidth=0.1, 
             xlim = c(0,3)) 
grid.arrange(p211, p221, ncol=2)

p23 <- qplot(carat, price, data=dsmall,xlab ="Weight(carats)" , ylab="Price($)", 
             main="Price-weight relationship")
p24 <- qplot(carat, price/carat, data=dsmall,xlab = "Weight(carats)", 
             ylab=expression(frac(price,carat)),
             main="Price-weight relationship",xlim=c(.2,1))
p25 <- qplot(carat, price, data=dsmall, log="xy")
grid.arrange(p23,p24,p25,ncol=3)
c










