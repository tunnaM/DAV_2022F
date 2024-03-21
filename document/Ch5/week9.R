library(tidyverse)
rm(list=ls())
data(mtcars, package="datasets")

str(mtcars)

head(mtcars)
names(mtcars)
rownames(mtcars)

mtcars$cyl <- as.factor(mtcars$cyl)

# plots with ggplot2
library(ggplot2)
                # 做美学图层需要
ggplot(mtcars, aes(x=hp, y=mpg, 
                   color="red", shape=cyl)) +
  geom_point(size=3)# 横纵轴如何定义 + 代表添加图示
# 由图片看出，马力越大，油耗越高
ggplot(mtcars, aes(x=hp, y=mpg, 
                   color=cyl)) +
  geom_point(size=3)

ggplot(mpg, aes(x=displ, y=hwy, 
                   color=class)) +
  geom_point(size=3)

head(mpg)
# aes美学层 geom_point用于画散点图
# alpha表示灰度，打印成本降低 分类在3-4个左右较好
ggplot(mpg, aes(x=displ, y=hwy, 
                alpha=class)) +
  geom_point(size=3)

ggplot(mpg, aes(x=displ, y=hwy, 
                alpha=class,shape=trans)) +
  geom_point(size=3)
  # 把auto和manual分别进行合并
  # 先进行filter


# facet
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 3) # 一个变量

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl) # 两个变量

table(mpg$drv)# f 前驱 r 后驱 4 全驱

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(. ~ cyl) # = facet_wrap(~ cyl)

# exercise
# Q1：x = displ, y = hwy class ~ trans
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(class ~ trans) # = facet_wrap(~ cyl)

# Q2：x = hp,y = mpg
ggplot(data = mtcars) + 
  geom_point(mapping = aes(x = hp, y = mpg)) + 
  facet_grid(cyl ~ gear)

# Geometric objects
# left
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
# right
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))
#倘若两个图横纵坐标相同, 改变散点颜色需要单独定义
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() +
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = cyl)) +
  geom_smooth(method='lm')# 对两个图单独影响

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))
  # 画出了三条曲线，linetype三种不同曲线

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)
  # 说明x与drv产生interaction,可以省去检验部分

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()
# R for data science

#' add separate regression lines
ggplot(mtcars, aes(x=hp, y=mpg, color=cyl, shape=cyl)) +
  geom_point(size=3) + # 散点
  geom_smooth(method="lm", aes(fill=cyl)) #对曲线有作用
  # 对4、6、8生成三段回归曲线，重点参数fill可以做局部回归
  # 检验，交互作用interaction，拆成三组做三个回归，说明系数不同，anova检验

#' add overall smooth
ggplot(mtcars, aes(x=hp, y=mpg)) +
  geom_point(size=3, aes(color=cyl, shape=cyl)) +
  geom_smooth(method="loess", color="black", se=FALSE) +
  geom_smooth(method="lm", aes(color=cyl, fill=cyl)) 
  # 平滑曲线与直线类似，说明第一段线性关系比较强，
  # 最后一段可能忽略了平方项

ggplot(mtcars, aes(x=hp, y=mpg)) +
  geom_point(size=3, aes(color=cyl, shape=cyl)) +
  geom_smooth(method="loess", color="black", se=FALSE, aes(fill=cyl)) +
  geom_smooth(method="lm", aes(color=cyl, fill=cyl)) 


#' change the theme
last_plot() + theme_bw()









