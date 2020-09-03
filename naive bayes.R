library(caret)
library(rsample)
library(klaR)

load(file = "caddata.RData")
load(file = "Featuresselected.RData")
df=as.data.frame(cad.df.balanced)

set.seed(123)


train <- train.df[,c(predictors(nb.features),"Cath")]
test  <- test.df[,c(predictors(nb.features),"Cath")]


control <- trainControl(method="repeatedcv", number=10)
NB_model<-train(Cath ~., data = train, method="nb", ,trControl=control)
pred=predict(NB_model,test)

mean(pred== test$Cath)#81%
save(NB_model,nb.features,file="NB.RData")