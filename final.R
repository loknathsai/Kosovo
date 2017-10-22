# adding libraries
library(ggplot2)
library(tidyr)
library(mice)
library(glmnet)
library(randomForest)
library(tree)
library(plyr)
library(gbm)


kos_2d <- read.csv("kosovo_rgdtk2est_oth.csv")

a <- sample(1:(nrow(kos_2d)),0.8*(nrow(kos_2d)))
training1 <- kos_2d[a,]
test1 <- kos_2d[-a,]

# linear regression
set.seed(702)
model_lm = lm(lvcnt ~ ., data = training1)
summary(model_lm)
a = predict(model_lm, test1)
mean((test1$lvcnt - a)^2)

#LASSO
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
par(mfrow = c(1, 2))
plot(model1_cv$size, model1_cv$dev, type = "b")
plot(model1_cv$k, model1_cv$dev, type = "b")
which(model1_cv$dev == min(model1_cv$dev))
which.min(model1_cv$dev)

#Tree pruning with best = 3
model1_prune = prune.tree(model1, best = 3)
par(mfrow = c(1, 1))
plot(model1_prune)
text(model1_prune, cex = 1)
a = predict(model1_prune, test1)
mean((test1$lvcnt - a)^2)

#Bagging
set.seed(702)
a = randomForest(lvcnt ~ ., data = training1, mtry = 3, ntree = 500, importance = T)
a = predict(a, test1)
mean((test1$lvcnt - a)^2)


#Random Forest
mse = 0
for(i in 1:3){
  model3 = randomForest(lvcnt ~ ., data = training1, mtry = i, ntree = 500, importance = T)
  a = predict(model3, test1)
  print(importance(model3))
  mse[i] = mean((test1$lvcnt - a)^2)
}
mean(mse)

#Boosting
set.seed(702)
a = seq(-10, -0.2, by = 0.1)
lambdas = 10^a
lambdas_len = length(lambdas)
train_err = rep(NA, lambdas_len)
test_err = rep(NA, lambdas_len)
for (i in 1:lambdas_len) {
  model3 = gbm(lvcnt~., data = training1, distribution = "multinomial", n.trees = 1000, shrinkage = lambdas[i])
  test_pred = predict(model3, test1, n.trees = 1000, type = "response")
  mse[i] = mean((test1$lvcnt - test_pred)^2)
}
model4 = gbm(LiverPatient ~ Age + sgpt + TP + ALB, data = training1, distribution = "multinomial", n.trees = 1000, shrinkage = lambdas[which.min(test_err)])
summary(model4)
min(test_err, na.rm = T)