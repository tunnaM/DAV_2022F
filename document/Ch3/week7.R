library(nycflights13)
library(tidyverse)

flights_sml <- select(flights,
                      year:day,
                      ends_with("delay"),
                      distance,
                      air_time
)

mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60
)#未存储 tuple immutable

flights_sml

flights_sml1 <- mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60
)

mutate(flights_sml, 
       gain = dep_delay - arr_delay, 
       hours = air_time / 60,
       gain_per_hour = gain / hours)
transmute(flights, 
          gain = dep_delay - arr_delay,
          hours = air_time / 60, 
          gain_per_hour = gain / hours)
transmute(flights, dep_time, 
          hour = dep_time %/% 100,
          minute = dep_time %% 100)
# cumsum cumprod
1:5
cumsum(1:5) # 累计频率
cumprod(1:5) # 累乘

y <- c(1, 2, 2, NA, 3, 4)
min_rank(y) #> [1] 1 2 2 NA 4 5
min_rank(desc(y)) #> [1] 5 3 3 NA 2 1

## summarize
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))
#> # A tibble: 1 x 1
#> delay
#> <dbl>
#> 1 12.6

by_day = group_by(flights, year, month, day) # 365 groups
# 并不会改变数据结构,但已经默认分过组
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

by_dest <- group_by(flights, dest)
delay <- summarise(by_dest, count = n(), dist = mean(distance, na.rm = TRUE), delay =
                     mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dest != "HNL")
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)
# 需要很多中间变量

delays <- flights %>%
  group_by(dest) %>%
  summarise( count = n(),dist = mean(distance, na.rm = TRUE), delay =
               mean(arr_delay, na.rm = TRUE)) %>%
  filter(count > 20, dest != "HNL")

flights %>%
  group_by(year, month, day) %>%
  summarise(n_early = sum(dep_time<500, na.rm = T))











