library(caret)
library(rsample)
library(klaR)

rm(list=ls())
load(file = "/Users/wangyunxuan/Downloads/caddata (3).RData")
df=as.data.frame(cad.df.balanced)
#head(df)
#which( colnames(df)=="Cath" )
#n<-ncol(df)

set.seed(123)
rcontrol <- rfeControl(functions = nbFuncs,method="repeatedcv")#, repeats=10,verbose = FALSE)
result<-rfe(x=df[,c(1:42,44:54)],y=df[,43],sizes = c(1:42,44:54),rfeControl =rcontrol)
print(result)
predictors(result)
result$fit

split<-initial_split(df[,c(predictors(result),"Cath")] ,prop = .8)
train <- training(split)
test  <- testing(split)



control <- trainControl(method="repeatedcv", number=10)
train_model<-train(Cath ~., data = train, method="nb", ,trControl=control)
train_model$results
pred=predict(train_model,test)
mean(pred== test$Cath)

