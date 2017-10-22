library(ggplot2)
library(tidyr)
library(mice)
library(glmnet)
library(randomForest)
library(tree)
library(plyr)
library(gbm)
library(caret)
library(mgcv)


kos_2d <- read.csv("kosovo-rgdtk2est_oth.csv")
###################################################################################################################################################
#Data Cleaning
md.pattern(kos_2d)
tempData <- mice(kos_2d,m=5,maxit=50,meth='pmm',seed=500)
kos_2d<-complete(tempData,2)
kos_2d <-  separate(kos_2d, dtk2,  into = c("Date", "Month","Year" ), sep = c(2,5))
kos_2d$Year <- sub("^", "19", kos_2d$Year )
kos_2d$Month[kos_2d$Month == "Mar"] <- 03
kos_2d$Month[kos_2d$Month == "Apr"] <- 04
kos_2d$Month[kos_2d$Month == "May"] <- 05
kos_2d$Month[kos_2d$Month == "Jun"] <- 06
kos_2d$Date <-as.integer(kos_2d$Date)
kos_2d$Month<-as.integer(kos_2d$Month)
kos_2d$Year <- as.integer(kos_2d$Year)
kos_2d$dtk2 <- paste(kos_2d$Year,kos_2d$Month,kos_2d$Date, sep="/")
kos_2d$dtk2 <- as.Date(kos_2d$dtk2)

kos_2d$Date<- NULL
kos_2d$Month<- NULL
kos_2d$Year<- NULL

########################################################################################################################################################################################
#exploratory analysis
ggplot(kos_2d, aes(kos_2d$dtk2, kos_2d$lvcnt)) + geom_line() + ggtitle("number of people fleeing Vs dates")
ggplot(kos_2d, aes(kos_2d$dtk2, kos_2d$nsum)) + geom_line() + ggtitle("number of people got killed Vs dates")
ggplot(kos_2d, aes(kos_2d$dtk2, kos_2d$bomb)) + geom_line() + ggtitle("The number of NATO air strikes Vs dates")
ggplot(kos_2d, aes(kos_2d$gcode, kos_2d$bomb)) + geom_bar(stat = "identity") + ggtitle("The number of NATO air strikes Vs Gcodes")
ggplot(kos_2d, aes(kos_2d$gcode, kos_2d$lvcnt)) + geom_bar(stat = "identity") + ggtitle("The number of people migrated Vs Gcodes")
ggplot(kos_2d, aes(kos_2d$gcode, kos_2d$nsum)) + geom_bar(stat = "identity") + ggtitle("The number of people got killed Vs Gcodes")
plot(kos_2d$nsum,kos_2d$lvcnt,xlab = "Number of people got killed",ylab = "Number of people got migrated", main ="Number of people got killed Vs number people migrated")

#region based splitting
ggplot(kos_2d, aes(kos_2d$gcode, kos_2d$lvcnt)) + geom_bar(stat = "identity") + ggtitle("The number of people fleed according to the geographic code")
north <- kos_2d[which(kos_2d$gcode == "north"),]
south <- kos_2d[which(kos_2d$gcode == "south"),]
east <- kos_2d[which(kos_2d$gcode == "east"),]
west <- kos_2d[which(kos_2d$gcode == "west"),]

ggplot(north, aes(north$dtk2, north$nsum)) + geom_line() + ggtitle("estimated deaths in North")
ggplot(south, aes(south$dtk2, south$nsum)) + geom_line() + ggtitle("estimated deaths in South")
ggplot(east, aes(east$dtk2, east$nsum)) + geom_line()+ ggtitle("estimated deaths in east")
ggplot(west, aes(west$dtk2, west$nsum)) + geom_line()+ ggtitle("estimated deaths in west")

ggplot(north, aes(north$dtk2, north$lvcnt)) + geom_line()+ ggtitle("estimated migrated people in North") 
ggplot(south, aes(south$dtk2, south$lvcnt)) + geom_line()+ ggtitle("estimated migrated people in South")
ggplot(east, aes(east$dtk2, east$lvcnt)) + geom_line()+ ggtitle("estimated migrated people in east")
ggplot(west, aes(west$dtk2, west$lvcnt)) + geom_line()+ ggtitle("estimated migrated people in West")

ggplot(north, aes(north$dtk2, north$bomb)) + geom_line()+ ggtitle("Number of bombs by NATO in North") 
ggplot(south, aes(south$dtk2, south$bomb)) + geom_line()+ ggtitle("Number of bombs by NATO in South")
ggplot(east, aes(east$dtk2, east$bomb)) + geom_line()+ ggtitle("Number of bombs by NATO in east")
ggplot(west, aes(west$dtk2, west$bomb)) + geom_line()+ ggtitle("Number of bombs by NATO in West")
#################################################################################################################



##Cleaning the data
a <- read.csv("kosovo-alt_cnts-border.csv")
md.pattern(a)
tempData <- mice(a,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)
a1<-complete(tempData,2)
a1$exdate <- as.Date(a1$exdate, "%d%B%Y")
###################################################################
#cleaning the date for the system to understand
b <- read.csv("kosovo-morina-border.csv")
b <-  separate(b, exdate,  into = c("Date", "Month","Year" ), sep = c(2,5))
b$Year <- sub("^", "19", b$Year )
b$Month[b$Month == "Mar"] <- 03
b$Month[b$Month == "Apr"] <- 04
b$Month[b$Month == "May"] <- 05
b$Month[b$Month == "Jun"] <- 06

b$Date <-as.integer(b$Date)
b$Month<-as.integer(b$Month)
b$Year <- as.integer(b$Year)



b$exdate <- paste(b$Year,b$Month,b$Date, sep="/")
b$exdate <- as.Date(b$exdate)

b$Date<- NULL
b$Month<- NULL
b$Year<- NULL
###################################################################
#Explorartory analysis of the merged data
c <- merge(a1,b, by = "exdate")
head(c)
ggplot(c, aes(c$exdate)) + geom_line(aes(y=c$uncnt), colour="red") + geom_line(aes(y=c$emgcnt), colour="blue")+ ggtitle("[Number of people crossed the border] RED-reported by UN, Blue-reported by EMG")

######################################################################################################################
##Modeling

#data splitting
set.seed(702)
a <- sample(1:(nrow(kos_2d)),0.8*(nrow(kos_2d)))
training1 <- kos_2d[a,]
test1 <- kos_2d[-a,]

#linear regression
set.seed(702)
model_lm = lm(lvcnt ~ ., data = training1)
summary(model_lm)
a = predict(model_lm, test1)
mean((test1$lvcnt - a)^2)
varImp(model_lm)

#linear regression for top 3 important variables
set.seed(702)
model_lm = lm(lvcnt ~ nsum + dtk2 + klaBlag , data = training1)
summary(model_lm)
a = predict(model_lm, test1)
mean((test1$lvcnt - a)^2)


#Tree
set.seed(702)
model1 = tree(lvcnt ~ ., data = training1)
summary(model1)
plot(model1)
text(model1, cex = 0.8)
a = predict(model1, test1)
mean((test1$lvcnt - a)^2)

#Tree using CV
set.seed(702)
model1_cv = cv.tree(model1, FUN = prune.tree)
summary(model1_cv)
par(mfrow = c(1, 2))
plot(model1_cv$size, model1_cv$dev, type = "b")
plot(model1_cv$k, model1_cv$dev, type = "b")
which(model1_cv$dev == min(model1_cv$dev))
which.min(model1_cv$dev)
model1_cv$size

#Tree pruning with best = 3
model1_prune = prune.tree(model1, best = 3)
summary(model1_prune) 
par(mfrow = c(1, 1))
plot(model1_prune)
text(model1_prune, cex = 1)
a = predict(model1_prune, test1)
mean((test1$lvcnt - a)^2)

#Bagging
set.seed(702)
b = randomForest(lvcnt ~ ., data = training1, mtry = 3, ntree = 500, importance = T)
b
a = predict(b, test1)
mean((test1$lvcnt - a)^2)
varImp(b)

#bagging using top 3 important variables
set.seed(702)
b = randomForest(lvcnt ~ nsum + dtk2 + gcode , data = training1, mtry = 3, ntree = 500, importance = T)
b
a = predict(b, test1)
mean((test1$lvcnt - a)^2)

#Random Forest
set.seed(702)
mse = 0
for(i in 1:9){
  model3 = randomForest(lvcnt ~ ., data = training1, mtry = i, ntree = 500, importance = T)
  a = predict(model3, test1)
  print(importance(model3))
  mse[i] = mean((test1$lvcnt - a)^2)
}
mean(mse)
varImp(model3)

#Random forest using top 3 important variables
set.seed(702)
mse = 0
for(i in 1:3){
  model3 = randomForest(lvcnt ~ nsum + dtk2 + gcode, data = training1, mtry = i, ntree = 500, importance = T)
  a = predict(model3, test1)
  print(importance(model3))
  mse[i] = mean((test1$lvcnt - a)^2)
}
mean(mse)

#LASSO
set.seed(702)
model_mat = model.matrix(lvcnt ~ ., data = training1)
y = training1$lvcnt
model_mat1 = model.matrix(lvcnt ~ ., data = test1)
model_gnet = glmnet(model_mat, y, alpha = 1)
a = predict(model_gnet, s = 0.01, newx = model_mat1)
mean((test1$lvcnt - a)^2)

#Ridge
model_mat = model.matrix(lvcnt ~ ., data = training1)
y = training1$lvcnt
model_mat1 = model.matrix(lvcnt ~ ., data = test1)
model_gnet = glmnet(model_mat, y, alpha = 0)
a = predict(model_gnet, s = 0.01, newx = model_mat1)
mean((test1$lvcnt - a)^2)


#boosting
set.seed(702)
model3 <- train(lvcnt ~ ., data=training1, method="gbm", verbose=FALSE)
model3$finalModel
a <- predict(model3, test1)
mean((test1$lvcnt - a)^2)
varImp(model3)

#boosting with top 2 most important variables
set.seed(702)
model3 <- train(lvcnt ~ nsum + dtk2, data=training1, method="gbm", verbose=FALSE)
model3$finalModel
a <- predict(model3, test1)
mean((test1$lvcnt - a)^2)

#Boosting using CV
a <- trainControl(method = "repeatedcv", repeats = 3)
b <- expand.grid(n.trees = seq(100, 1000, by = 50), interaction.depth = seq(1, 7, by = 2), shrinkage = c(0.01, 0.1), n.minobsinnode = 10)

set.seed(702)
model3_cv <- train(lvcnt ~ ., data=training1, method="gbm", verbose=FALSE, tuneGrid=b, trControl=a)
model3_cv[6]
model3_cv$finalModel
a <- predict(model3_cv, test1)
mean((test1$lvcnt - a)^2)
varImp(model3_cv)

#Boosting using CV with 2 most important variables
a <- trainControl(method = "repeatedcv", repeats = 3)
b <- expand.grid(n.trees = seq(100, 1000, by = 50), interaction.depth = seq(1, 7, by = 2), shrinkage = c(0.01, 0.1), n.minobsinnode = 10)

set.seed(702)
model3_cv <- train(lvcnt ~ nsum + dtk2, data=training1, method="gbm", verbose=FALSE, tuneGrid=b, trControl=a)
model3_cv[6]
model3_cv$finalModel
a <- predict(model3_cv, test1)
mean((test1$lvcnt - a)^2)

#Gaussian GAM
model4<-gam(lvcnt ~ s(nsum) + s(bomb) +s(bomblag) ,family=gaussian(),data=kos_2d)
summary(model4)
model4
varImp(model4)
##############################################################################################################################