##########################################################################
##########################################################################
##########################################################################

# Note: 1. If lack of packages, plz 'install' and 'library' the packages;
#       2.  




rm(list=ls()) ## To remove the objects in the R environment if there is any
############ Preparations: Installing and Loading  R packages ############
# install.packages("readr", dependencies=T)
# install.packages("knitr", dependencies=T)
# install.packages("tidyverse", dependencies=T)
# install.packages("readxl", dependencies=T)
library(readr);library(knitr);library(readxl);library(tidyverse)


##################### Part 1: Read Files into R ##########################
path <- setwd("/Users/bearliu/Desktop/DataVis/Ch3") # Set your own workding directory.
getwd()
list.files() # To show the files in the current directory

### import .csv as tibble
class.tib <- read_csv('class.csv', col_names=T) # E.g.1: import class.csv as tibble
kable(class.tib) ## demo of tibble
class.tib ## demo of tibble


### import .csv as data frame
class.df <- read.csv('class.csv', header=T, as.is = T, stringsAsFactors=T)
class.df ## demo of dataframe


### import .xlxs as tibble
class.xls1 <- read_xlsx('class.xlsx', col_names=T, skip=1)
class.xls1 ## not run
class.xls2 <- read_excel('class.xlsx', col_names=T, skip=1)
class.xls2 ## not run

### import .csv as pure characters
class.char <- readLines('class.csv')
print(class.char)


##################### Part 2: Descriptive Statistics #####################
# Note: In the following, if not specified, we use .tibble format to do analysis.


### Basic info of a data set, such as sample size, no. of variables, etc...
dim(class.tib)
names(class.tib)
head(class.tib, 5)
typeof(class.tib)
lapply(class.tib, class) # return the type of each column variable as a list
sapply(class.tib, class) # return the type of each column variable as a vector
sapply(class.df, class) # return the type of each column variable as a vector
sapply(class.df, typeof) # return the type of each column variable as a vector
sapply(class.df, mode) # not run


### Five number summary and boxplots of numeric variables
summary(class.tib)
summary(class.df)
boxplot(class.tib[,3:5]) # bp for all variables into one graph
boxplot(class.tib$height~class.tib$sex, 
        ylab="Height", xlab="Group") # bp without outlier
boxplot(class.tib$weight~class.tib$sex, 
        ylab="Weight", xlab="Group",horizontal=T) # bp without outlier
boxplot(class.tib$weight~class.tib$sex, 
        ylab="Weight", xlab="Group",col=3, outline=T, notch=F) # bp with outlier
boxplot(class.tib$weight~class.tib$sex, 
        ylab="Weight", xlab="Group",col=2, outline=F, notch=T) # drop outlier

hist(class.tib$height, freq=T, col=c(1,2,3), labels=T)
histogram

# lattice; rga(d);

### To get confidence intervals 
# install.packages("BSDA", dependencies=T)
library(BSDA)
alpha <- 0.05
height.CI <- z.test(class.tib$height, sigma.x=sd(class.tib$height), conf.level=1-alpha)$conf.int
height.CI ## not run

### Hypothesis Test
mu.test <- 50

### Two-side test
height.test <- z.test(class.tib$height, mu=mu.test, sigma.x=sd(class.tib$height))
height.test$p.value
height.test$statistic
height.test$estimate

### Two-side test
height.left <- z.test(class.tib$height, mu=mu.test, sigma.x=sd(class.tib$height), alternative='less')
height.left$p.value

height.right <- z.test(class.tib$height, mu=mu.test, sigma.x=sd(class.tib$height), alternative='greater')
height.right$p.value


### To right two self-created functions which give the CI and hypothese test with p.value and teststatistic
z.test.1s <- function(x, n=length(x), mu=0, sigma=sd(x), alternative="two.sided", alpha=0.05){
  z <- (mean(x) - mu) / (sigma / sqrt(n))
  lower <- mean(x) + qnorm(alpha/2) * sigma / sqrt(n)
  upper <- mean(x) + qnorm(1-alpha/2) * sigma / sqrt(n)
  if(alternative=="two.sided"){   # 双侧检验
    pvalue <- 2*(1 - pnorm(abs(z)))
  } else if(alternative=="less"){ # 左侧检验
    pvalue <- pnorm(z)
  } else if(alternative=="greater"){ # 右侧检验
    pvalue <- 1 - pnorm(z)
  } else {
    stop("alternative unknown!")
  }
  
  c(stat=z, pvalue=pvalue, lower=lower, upper=upper)
}

z.test.1s(class.tib$height, mu=65, alpha=0.05) ## not run



### To right two self-created functions which give the CI and hypothese test with p.value and teststatistic
t.test.1s <- function(x, n=length(x), df=length(x)-1, mu=0, sigma=sd(x), alternative="two.sided", alpha=0.05){
  t <- (mean(x) - mu) / (sigma / sqrt(n))
  lower <- mean(x) + qt(alpha/2, df) * sigma / sqrt(n)
  upper <- mean(x) + qt(1-alpha/2, df) * sigma / sqrt(n)
  if(alternative=="two.sided"){   # 双侧检验
    pvalue <- 2*(1 - pt(abs(t), df))
  } else if(alternative=="less"){ # 左侧检验
    pvalue <- pt(t,df)
  } else if(alternative=="greater"){ # 右侧检验
    pvalue <- 1 - pt(t, df)
  } else {
    stop("alternative unknown!")
  }
  
  round(c(stat=t, pvalue=pvalue, lower=lower, upper=upper),4)
}

t.test.1s(class.tib$height, mu=50, alpha=0.05) ## not run

t.test(class.tib$height, mu=50, alternative="two.sided") 
# Compare the results from self-created t-test with embedded functions

### Two sample t-test
### To right two self-created functions which give the CI and hypothese test with p.value and teststatistic
t.test.2s <- function(x1, x2, 
                      mu1=0, mu2=0, 
                      alternative="two.sided", 
                      pool=T, alpha=0.05){
  n1 <- length(x1)
  n2 <- length(x2)
  df <- n1 + n2 -2
  sig1 <- sd(x1)
  sig2 <- sd(x2)
  if (pool){
    sigma <- sqrt(((n1-1)*sig1^2 + (n2-1)*sig2^2)/df) * sqrt(1/n1+1/n2)
    t <- (mean(x1)-mean(x2) - mu1+mu2) / (sigma)
    if(alternative=="two.sided"){   # 双侧检验
      pvalue <- 2*(1 - pt(abs(t), df))
    } else if(alternative=="less"){ # 左侧检验
      pvalue <- pt(t,df)
    } else if(alternative=="greater"){ # 右侧检验
      pvalue <- 1 - pt(t, df)
    } else {
      stop("alternative unknown!")
    }
  } else if (1-pool){
    sigma <- sqrt(sig1^2/n1 + sig2^2/n2)
    t <- (mean(x1)-mean(x2) - mu1+mu2) / sigma
    if(alternative=="two.sided"){   # 双侧检验
      pvalue <- 2*(1 - pt(abs(t), df))
    } else if(alternative=="less"){ # 左侧检验
      pvalue <- pt(t,df)
    } else if(alternative=="greater"){ # 右侧检验
      pvalue <- 1 - pt(t, df)
    } else {
      stop("alternative unknown!")
    }
  }
  
  lower <- mean(x1) - mean(x2) + qt(alpha/2, df) * sigma
  upper <- mean(x1) - mean(x2) + qt(1-alpha/2, df) * sigma

  
  round(c(stat=t, pvalue=pvalue, lower=lower, upper=upper),4)
}


## Equal variance 2sample t test
t.test.2s(class.tib$height[class.tib$sex=="F"], 
          class.tib$height[class.tib$sex=="M"],
          mu1=50, mu2=50, alpha=0.05) ## not run
t.test(class.tib$height[class.tib$sex=="F"], 
       class.tib$height[class.tib$sex=="M"], mu=0, alternative="two.sided", var.equal = T) 
# Note that t.test for 2 samples, mu is the difference of two samples.


## Unequal variance 2-s t-test
t.test.2s(class.tib$height[class.tib$sex=="F"], 
          class.tib$height[class.tib$sex=="M"],
          mu1=52, mu2=50, alpha=0.05) ## not run
t.test(class.tib$height[class.tib$sex=="F"], 
       class.tib$height[class.tib$sex=="M"], mu=2, alternative="two.sided", var.equal = F) 
# Note that t.test for 2 samples, mu is the difference of two samples.




### Proporntion z test
prop.test.1s <- function(x, n, p=0.5, alternative="two.sided", alpha=0.05){
  phat <- x/n
  zstat <- (phat - p)/sqrt(p*(1-p)/n)
  lower <- phat + pnorm(alpha/2)*sqrt(p*(1-p)/n)
  upper <- phat + pnorm(1-alpha/2)*sqrt(p*(1-p)/n)
  if(alternative=="two.sided"){ # 双侧检验
    pvalue <- 2*(1 - pnorm(abs(zstat)))
  } else if(alternative=="less"){ # 左侧检验
    pvalue <- pnorm(zstat)
  } else if(alternative=="greater"){ # 右侧检验
    pvalue <- 1 - pnorm(zstat)
  } else {
    stop("alternative unknown!")
  }
  
  round(c(stat=zstat, pvalue=pvalue, lower=lower, upper=upper),4)
}



### Quiz 1 ###
# 1. Find the confidence interval of a sample variance
# 2. Find the confidence interval of ratio of 2 sample variances
# 3. Given a dataset with continuous columns, 
#    find confidence interval of population mean of each column variable
t(apply(class.tib[,4:5], 2, t.test.1s))
t(sapply(class.tib[,4:5], t.test.1s))

# Plot 2 histograms
histogram(class.tib$height)
histogram(class.tib$weight, col=2)


iris.tib <- as_tibble(iris)
iris.tib <- mutate(iris.tib, Sepal.LWRatio=Sepal.Length/Sepal.Width, Petal.LWRatio=Petal.Length/Petal.Width)
boxplot(apply(iris.tib[,c(1:4,6:7)], 2, FUN=function(x){(x-mean(x))/sd(x)}), 
        horizontal=T,col=c(2,3,4,5,6,7), cex=0.3)
boxplot(apply(iris.tib[,c(1:4,6:7)], 2, FUN=function(x){x}), 
        horizontal=T,col=c(2,3,4,5,6,7), cex=0.3)

summary(iris.tib)
hist(iris.tib$Sepal.Length)
plot(iris.tib$Sepal.Length, 
     iris.tib$Sepal.LWRatio,
     col=1)
color <- ifelse(iris.tib$Species=="setosa", 1,
                ifelse(iris.tib$Species=="versicolor",2,3))
plot(x=iris.tib$Sepal.Length, 
     y=iris.tib$Sepal.LWRatio, 
     col=color)
plot(Sepal.)
table(iris.tib$Species)
lines(iris.tib$Sepal.Width,col="blue")




