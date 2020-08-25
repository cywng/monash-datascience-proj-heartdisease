library(caret)
library(rsample)
library(glmnet) 

rm(list=ls())
load(file = "caddata.RData")
df <- as.data.frame(lapply(cad.df.balanced, function(x) if(is.factor(x)){
  as.numeric(x)-1
} else x))
df$Cath <- as.factor(df$Cath)


set.seed(123)
split<-initial_split(df ,prop = .8)
train <- training(split)
test  <- testing(split)

rcontrol <- rfeControl(functions = lrFuncs,method="cv")
result<-rfe(Cath ~., data = train,sizes = c(1:54),rfeControl =rcontrol)
train <- train[,c(predictors(result),"Cath")]
test  <- test[,c(predictors(result),"Cath")]


control <- trainControl(method="repeatedcv", number=10)

#1.Penalized logictic regression
#train_model_lasso<-train(Cath ~., data = train, method="glmnet",tuneGrid = expand.grid(alpha = 1, lambda = seq(0.0001, 1, length = 10)),trControl=control)
#train_model_lasso$results
#pred1=predict(train_model_lasso,test)

#train_model_ridge<-train(Cath ~., data = train, method="glmnet",tuneGrid = expand.grid(alpha = 0, lambda = seq(0.0001, 1, length = 10)),trControl=control)
#train_model_ridge$results
#pred2=predict(train_model_ridge,test)

#use glemnet for both ridge and lasso
train_model<- train(Cath ~., data = train, method = "glmnet",tuneGrid = expand.grid(alpha = 0:1, lambda = seq(0.0001, 1, length = 10)), trControl = control)
pred=predict(train_model,test)  




#2.doesn't use a penalty term:
#train_model3<-train(Cath ~., data = train, method="glm", family = "binomial" ,trControl=control)
#pred4=predict(train_model3,test) #-----low accuracy 



mean(pred== test$Cath)








