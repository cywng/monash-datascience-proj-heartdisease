library(caret)
library(rsample)
library(klaR)

rm(list=ls())
load(file = "caddata.RData")
df=as.data.frame(cad.df.balanced)
set.seed(123)
split<-initial_split(df ,prop = .8)
train <- training(split)
test  <- testing(split)

rcontrol <- rfeControl(functions = nbFuncs,method="cv")#, repeats=10,verbose = FALSE)
result<-rfe(x=train[,c(1:42,44:54)],y=train[,43],sizes = c(1:54),rfeControl =rcontrol)
predictors(result)


train <- train[,c(predictors(result),"Cath")]
test  <- test[,c(predictors(result),"Cath")]


control <- trainControl(method="repeatedcv", number=10)
train_model<-train(Cath ~., data = train, method="nb", ,trControl=control)
train_model$results
pred=predict(train_model,test)

mean(pred== test$Cath)


