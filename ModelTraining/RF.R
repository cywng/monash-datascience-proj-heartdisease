library(caret)
library(ranger)

load(file = "DataWrangling/caddata.RData")
load(file = "DataWrangling/Featuresselected.RData")
df=as.data.frame(cad.df.balanced)

set.seed(123)


train <- train.df[,c(predictors(rf.features),"Cath")]
test  <- test.df[,c(predictors(rf.features),"Cath")]


control <- trainControl(method="repeatedcv", number=10)
RF_model<-train(Cath ~., data = train, method="rf",trControl=control)
RF_model$results
pred=predict(RF_model,test)

mean(pred== test$Cath)#0.87
save(RF_model,rf.features,file="Models/RF.RData")
