library(tidyverse)

data(diamonds)
# 离散数据 带分组
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x=cut))
  # 可以看出众数是 ideal
# 频数分布表
diamonds %>%
  count(cut)
# color分布是否平均
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x=color))

diamonds %>%
  count(color)
# 连续变量
diamonds %>%
  ggplot() +
  geom_histogram(mapping=aes(x=carat),binwidth = 0.5)

diamonds %>%
  count(cut_width(carat,0.5))
  # 看到右偏，不能进行正态性分布

diamonds %>%
  ggplot() +
  geom_histogram(mapping=aes(x=price),binwidth = 1000)

diamonds %>%
  count(cut_width(price,5000))

# 分析一部分数据
smaller <- diamonds %>%
  filter(carat<3)

smaller %>%
  ggplot() +
  geom_histogram(mapping=aes(x=carat),binwidth = 0.1)
  # 数据呈现两个峰，一个分布不够刻画，两个或三个正态分布混在一起

smaller %>%
  ggplot() + 
  geom_freqpoly(mapping=aes(x=carat,colour = cut),binwidth = 0.1)

smaller %>%
  ggplot() + 
  geom_freqpoly(mapping=aes(x=price,colour = cut),binwidth = 1000)

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))
  # 碎钻 水钻 影响较小可以删掉，甚至认为数据是假的
  # 异常值处理 除了删掉
diamonds2 <- diamonds %>% 
  filter(between(y, 3, 20))

diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60 ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 
                  1/4)

ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(colour = clarity), binwidth = 500)

ggplot(data = diamonds, 
       mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cut), 
                binwidth = 500)

ggplot(data = diamonds, aes(x = cut, y = price)) +
  geom_boxplot()

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))

ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

diamonds %>% count(color, cut) %>% 
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))

ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price))

ggplot(smaller, aes(x = carat, y = price)) + 
  geom_boxplot(aes(group = cut_width(carat, 0.1)))

ggplot(data = smaller, aes(x=carat,y=price)) +
  geom_smooth(method="lm")

carat2price = lm(price~carat+color, data=smaller)
summary(carat2price)

ggplot(data = faithful) + 
  geom_point(aes(x = eruptions, y = waiting))

library(modelr)
mod <- lm(log(price) ~ log(carat), data = diamonds)
diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))
ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid))






