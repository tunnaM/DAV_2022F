library(nycflights13)
library(tidyverse)

head(flights)
dim(flights)

head(flights,20)

# filter out a subset of your tibble
filter(flights, month == 1, day == 1)
filter(flights, month == 1, day == 1, carrier == 'UA', origin == 'EWR')

jan1 = filter(flights, month == 1, day == 1)
jan1

# Jan1-Jan3
Jan13 = filter(flights, month == 1, day <= 3)
JanMar = filter(flights, month <= 3)
JanMar = filter(flights, 1<= month <= 3) #error
JanMar = filter(flights, (month <= 3) & (month >= 1))
filter(flights, month == 11 | month == 12)

nov_dec <- filter(flights, month %in% c(11, 12))
oct_nov_dec <- filter(flights, between(month, 10, 12))
# between takes two values in between, where both limits are included.

# Q1: flights that take off in Season 3.
Jul_aug_sep <- filter(flights, between(month, 7, 9))

flight4 = filter(flights, arr_delay <= 120, dep_delay <= 120)
filter(flights, !(arr_delay > 120 | dep_delay > 120))

# Q2: flights that come from 'UA','AA',dep_delay not over 1hr
flight5 = filter(flights, carrier %in% c('UA','AA'), dep_delay <= 60)

X <- 'NA'
is.na(X)

df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x>1)

arrange(flights, year, month, day)

# Q3: order flights by arr_time, sched_arr_time
arrange(flights, arr_time, sched_arr_time)

arrange(flights, desc(dep_delay))

arrange(flights, desc(dep_delay), desc(dep_delay))
flights_ordered <- arrange(flights, desc(dep_delay))
flights_ordered %>% head(5)
head(flights_ordered)

# Q4: flights that come from 'UA' and 'AA',
# top 10 arrival-delayed flights
flights5 = filter(flights, carrier %in% c('UA','AA'))
arrange(flights5, desc(arr_delay))

# select
select(flights, starts_with("dep"))
select(flights, ends_with("delay"))

rename(flight5, tail_num = tailnum) 
select(flight5, tail_num = tailnum)

select(flights, time_hour, air_time, everything())
