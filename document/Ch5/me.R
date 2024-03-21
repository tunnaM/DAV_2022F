#### GGplot exercise

library(ggplot2)
library(tidyverse)
data(airquality, package="datasets")
as_tibble(airquality)
### Q1 利用 airquality 数据集，x是Wind，y是Temp，进行散点图，绿色
ggplot(data = airquality,aes(x=Wind,y=Temp)) +
  geom_point(color='green')
### Q2 将散点图不同月份用不同颜色标出
ggplot(data = airquality,aes(x=Wind,y=Temp)) +
  geom_point(aes(color=factor(Month)))
### Q3 增加x和y的光滑拟合线
ggplot(data = airquality, mapping = aes(x = Wind, y = Temp)) + 
  geom_point() + 
  stat_smooth(method='loess')
### Q4 增加x和y的线性回归线，不要输出置信带
ggplot(data = airquality, mapping = aes(x = Wind, y = Temp)) + 
  geom_point() + 
  stat_smooth(method='lm',se=FALSE)
### Q5 对于不同月份的数据，分别作x和y的线性回归线，
### 用不同颜色展示，不要输出置信带
ggplot(data=airquality,aes(x=Wind,y=Temp)) +
    stat_smooth(method='lm',se=FALSE,aes(color=factor(Month)))
### Q6 对于不同月份的数据，分别作x和y的线性回归线；再用黄色做出所有数据的
### 线性回归线，用黄色展示，不要输出置信带
ggplot(data=airquality,aes(x=Wind,y=Temp)) +
  stat_smooth(method='lm ', se=FALSE, aes(color=factor(Month))) +
  stat_smooth(method='lm', se=FALSE, color='yellow')
### Q7 按照⽉份分成五个⾯板，每个面板画x和y的回归线和散点图
ggplot(data=airquality,aes(x=Wind,y=Temp)) +
  geom_point(alpha=0.7,size=0.5) +
  stat_smooth(method='lm',se=FALSE,aes(color=factor(Month))) +
  facet_grid(.~Month)
### Q8 按照月份分成五个面板，每个面板画Ozone和Temp散点图，并增加
### 回归线，你认为不同月份的回归线之间有显著区别吗？
ggplot(data=airquality,aes(x=Ozone,y=Temp)) + 
  geom_point(alpha=0.7,size=0.5) +
  stat_smooth(method='lm',se=FALSE,aes(color=factor(Month))) +
  facet_grid(.~Month)
### Q9 夏季的平均臭氧浓度是否更高？请用图展示
airquality.gp = airquality %>% group_by(Month) %>%
  summarise(Ozone.avg = mean(Ozone,na.rm=T))
ggplot(data = airquality.gp,aes(x = Month, y = Ozone.avg)) +  
    geom_point(aes(alpha=1.5,size=1.0))
airquality <- mutate(airquality, season = "")
# 季节分组
airquality$season[airquality$Month == 6|airquality$Month == 7|airquality$Month == 8]="S"
airquality$season[airquality$Month == 5|airquality$Month == 9]="O"
ggplot(data = airquality) + 
  stat_summary(
    mapping = aes(x = season, y = Ozone),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median)

### Q10 Ozone与风速是否呈现负相关？不同季节中，该相关性是否有变化？
ggplot(data=airquality,aes(x=Ozone,y=Wind)) + 
  geom_point(alpha=0.7,size=0.5) +
  stat_smooth(method='lm',se=FALSE,aes(color=factor(season))) +
  facet_grid(.~season)


