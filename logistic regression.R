library(caret)
#library(rsample)
library(glmnet) 
rm(list=ls())
load(file = "caddata.RData")
load(file = "Featuresselected.RData")
df <- as.data.frame(lapply(cad.df.balanced, function(x) if(is.factor(x)){
  as.numeric(x)-1
} else x))
df$Cath <- as.factor(df$Cath)


set.seed(123)

train <- train.df[,c(predictors(lr.features),"Cath")]
test  <- test.df[,c(predictors(lr.features),"Cath")]


control <- trainControl(method="repeatedcv", number=10)

#1.Penalized logictic regression
#train_model_lasso<-train(Cath ~., data = train, method="glmnet",tuneGrid = expand.grid(alpha = 1, lambda = seq(0.0001, 1, length = 10)),trControl=control)
#train_model_lasso$results
#pred1=predict(train_model_lasso,test)

#train_model_ridge<-train(Cath ~., data = train, method="glmnet",tuneGrid = expand.grid(alpha = 0, lambda = seq(0.0001, 1, length = 10)),trControl=control)
#train_model_ridge$results
#pred2=predict(train_model_ridge,test)

#use glemnet for both ridge and lasso
#train_model<- train(Cath ~., data = train, method = "glmnet",tuneGrid = expand.grid(alpha = 0:1, lambda = seq(0.0001, 1, length = 10)), trControl = control)
#pred=predict(train_model,test)    # 85%




#2.doesn't use a penalty term:
train_model3<-train(Cath ~., data = train, method="glm", family = "binomial" ,trControl=control)
pred4=predict(train_model3,test) #-----high accuracy   87%



mean(pred4== test$Cath)
