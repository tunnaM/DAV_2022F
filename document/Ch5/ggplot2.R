#' ## Load the `mtcars` data
data(mtcars, package="datasets")
str(mtcars)

names(mtcars)
rownames(mtcars)
#' ## Plot with base graphics
#' This doesn't quite come out right because it is hard to coordinate the 
#' choices for point symbols and colors in the plot with those in the legend

mtcars$cyl <- as.factor(mtcars$cyl)
plot(mpg ~ hp , data=mtcars, 
     col=cyl, pch=c(4,6,8)[mtcars$cyl], cex=1.2)
legend("topright", legend=levels(mtcars$cyl),
       pch = c(4,6,8),
       col=levels(mtcars$cyl))

#' ## Plots with ggplot2
library(ggplot2)
ggplot(mtcars, aes(x=hp, y=mpg, 
                   color="red", shape=cyl)) +
  geom_point(size=3)

#' add separate regression lines
ggplot(mtcars, aes(x=hp, y=mpg, color=cyl, shape=cyl)) +
  geom_point(size=3) +
  geom_smooth(method="lm", aes(fill=cyl)) 

#' add overall smooth
ggplot(mtcars, aes(x=hp, y=mpg)) +
  geom_point(size=3, aes(color=cyl, shape=cyl)) +
  geom_smooth(method="loess", color="black", se=FALSE) +
  geom_smooth(method="lm", aes(color=cyl, fill=cyl)) 

#' change the theme
last_plot() + theme_bw()

#' ## break it down into stages
plt <-
  ggplot(mtcars, aes(x=hp, y=mpg, color=cyl, shape=cyl)) +
  geom_point(size=3) 

plt +
  geom_smooth(method="lm", aes(fill=cyl)) 

plt +
  geom_smooth(method="lm", aes(color=cyl, fill=cyl)) +
  geom_smooth(method="loess", color="black", se=FALSE)
  
  #' ## faceting
  plt <-
  ggplot(mtcars, aes(x=hp, y=mpg, color=cyl, shape=cyl)) +
  geom_point(size=3) +
  geom_smooth(method="lm", aes(fill=cyl)) 

plt + facet_wrap(~cyl+vs)

plt + facet_wrap(~gear)




library(ggplot2)
library(ggrepel)
plt2 <- ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point(color = "red", size=2) +
  geom_smooth(method="lm", se=FALSE) + 
  labs(y="Miles per gallon", x="Weigth (1000 lbs.)") +
  theme_classic(base_size = 16)  

plt2 + geom_text(aes(label=rownames(mtcars)))

#ggplot(mtcars, aes(wt, mpg)) +
plt2 + 
  geom_point(color = 'red') +
  geom_text_repel(aes(label = rownames(mtcars))) +
  theme_classic(base_size = 16)

# point selection
mod <- lm(mpg~wt, data=mtcars)
resids <- residuals(mod)
mtcars$label <- ifelse(abs(resids) > 2.5, 
                       rownames(mtcars), "")
plt2 + 
  geom_text_repel(aes(label = mtcars$label)) 



names(plt2)
class(plt2)
