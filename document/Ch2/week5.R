######### Data frame

d <- data.frame(
  name=c(" 李明", " 张聪", " 王建"),#空格 
  age=c(30, 35, 28),
  height=c(180, 162, 175), 
  stringsAsFactors=FALSE)
print(d)

# transfer a matrix to a dataframe


d[2,3]
d[,3]
d[,2:3]
d[,'age']
d$age

d[d[,'age']>=30,]


# exercise
set.seed(123)
exer.df = rnorm(100)

#Q1: change this into a dataframe df, 20 rows and 5 colomns
# each column has a frame "Var1" -"Var5"

df = as.data.frame(matrix(exer.df,20,5,byrow=F))
print(df)
names(df) = paste("Var",1:5,sep='')

#Q2: print all rows that volues of Var3 <= 0, find the number of rows
df[ df$Var3 <= 0,]
nrow(df[ df$Var3 <= 0,])
dim(df[ df$Var3 <= 0,])

##### tibble类型
library(tibble)
library(readr)

path = getwd() # get working directory
file.path = paste(path,'/desktop/class.csv',sep='')
t.class = read_csv(file.path)

d.class = read_csv("E:/大三上/6、数据分析与可视化/课程文档/Ch2/class.csv")
d.class

class(d.class)

d.class$sex

# exercise
# Q1
temp = d.class[d.class$age>=15,]
temp
temp[temp$sex=='F',]
x = d.class$age


##list

rec <- list(name=c(" 李明"," 张三"), 
            age=c(30,40),
            scores=matrix(c(85,76,90,88,66,88),
                          nrow=2,ncol=3,byrow=T))
rec
rec[[3]]
rec$scores
rec[[3]][,2]

name=c(" 李明"," 张三")
age=c(30,40)
scores=matrix(c(85,76,90,88,66,88),
              nrow=2,ncol=3,byrow=T)

li1 = list(name,age,scores)
li1
unlist(li1)
class(unlist(li1))

x<-c('10,8,7','5,2,2','3,7,8','8,8,9')
res <-strsplit(x,',')
res

length(res)
res.mat = matrix(rep(0,,12),4,3)

for(i in 1:length(res)){
  temp = res[[i]]
  res.mat[i,] = as.numeric(temp)
}

res.mat

sapply(res, as.numeric) #sapply works as a list


