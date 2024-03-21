library(tidyverse)
rm(list=ls())
data(mtcars, package="datasets")

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

ggplot(data = demo) + 
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity") 

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = stat(prop), 
                         group = 1))

ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median)
# 类似箱线图，只有最大值最小值中位数
# 一般不使用statistic transformation
# group_by facet更快

ggplot(data = diamonds) + 
    geom_bar(mapping = aes(x = cut, fill = clarity))
# 条件分布 不太一样

ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")

ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity),
           position = "fill")
# 比率拉齐到1，各自归一化，再看条件分布，清晰的不一样

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity),
           position = "dodge")









