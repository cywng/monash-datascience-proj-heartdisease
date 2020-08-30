library(caret)
library(ranger)

load(file = "caddata.RData")
load(file = "Featuresselected.RData")
df=as.data.frame(cad.df.balanced)

set.seed(123)


train <- train.df[,c(predictors(rf.features),"Cath")]
test  <- test.df[,c(predictors(rf.features),"Cath")]


control <- trainControl(method="repeatedcv", number=10)
train_model<-train(Cath ~., data = train, method="rf",trControl=control)
train_model$results
pred=predict(train_model,test)

mean(pred== test$Cath)#0.87