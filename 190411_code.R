###ITS CONFERENCE

library(NbClust)
library(ggplot2)
library(dplyr)
library(reshape2)
library(VarSelLCM)

###DATA LOADING
car.shr <- read.csv('C:/Users/jongu/OneDrive/바탕 화면/workspace/carsharing_f.csv')

###CHECK LOAD DATA
head(car.shr)
tail(car.shr)
summary(car.shr)

sd(car.shr$나이)
sd(car.shr$이용시간)
sd(car.shr$이용거리)
sd(car.shr$시간)


###SET VARIABLE TYPE (CATEGORY=FACTOER, CONTINUOUS=NUMERIC)
car.shr$차종 <- as.factor(car.shr$차종)
car.shr$나이 <- as.numeric(car.shr$나이)
car.shr$성별 <- as.factor(car.shr$성별)
car.shr$이용시간 <- as.numeric(car.shr$이용시간)
car.shr$이용거리 <- as.numeric(car.shr$이용거리)
car.shr$구분 <- as.factor(car.shr$구분)
car.shr$시간 <- as.numeric(car.shr$시간)

str(car.shr)

###CONTINUOUS VARIABLE STANDARDIZATION
car.shr$나이 <- scale(car.shr$나이)
car.shr$이용시간 <- scale(car.shr$이용시간)
car.shr$이용거리 <- scale(car.shr$이용거리)
car.shr$시간 <- scale(car.shr$시간)

car.shr.km2 <- kmeans(car.shr, centers = 2, iter.max = 10000)
car.shr.km3 <- kmeans(car.shr, centers = 3, iter.max = 10000)
car.shr.km4 <- kmeans(car.shr, centers = 4, iter.max = 10000)
car.shr.km5 <- kmeans(car.shr, centers = 5, iter.max = 10000)
car.shr.km6 <- kmeans(car.shr, centers = 6, iter.max = 10000)

###RESULT CHECK
###2 CLUSTER: SIZE: 32906, 33889
car.shr.km2
###3 CLUSTER: SIZE: 1735, 30419, 34641
car.shr.km3
###4 CLUSTER: SIZE: 13848, 1551, 26160, 25236
car.shr.km4
###5 CLUSTER: SIZE: 24321, 23999, 13098, 684, 4693
car.shr.km5
###6 CLUSTER: SIZE: 22525, 9333, 15090, 4311, 671, 14865
car.shr.km6

### MELT
car.km2.melt<- melt(car.shr.km2$centers)
car.km2.melt

car.km3.melt<- melt(car.shr.km3$centers)
car.km3.melt

car.km4.melt<- melt(car.shr.km4$centers)
car.km4.melt

car.km5.melt<- melt(car.shr.km5$centers)
car.km5.melt

car.km6.melt<- melt(car.shr.km6$centers)
car.km6.melt

### SAVE IMAGE
getwd()
dev.off()

#### VISUALIZATION
ggplot(data= car.km2.melt, aes(x= Var2, y= value, group= Var1, color= Var1)) + geom_line()
ggplot(data= car.km3.melt, aes(x= Var2, y= value, group= Var1, color= Var1)) + geom_line()
ggplot(data= car.km4.melt, aes(x= Var2, y= value, group= Var1, color= Var1)) + geom_line()
ggplot(data= car.km5.melt, aes(x= Var2, y= value, group= Var1, color= Var1)) + geom_line()
ggplot(data= car.km6.melt, aes(x= Var2, y= value, group= Var1, color= Var1)) + geom_line()

### CLUSTER 4
str(car.km4.melt)
car.km4.melt$Var1 <- as.factor(car.km4.melt$Var1)
names(car.km4.melt) <- c("군집","변수","표준점수")
ggplot(data= car.km4.melt, aes(x= 변수, y= 표준점수, group= 군집, color= 군집)) + geom_line(size=0.8)


### FINISH

### REVIEW