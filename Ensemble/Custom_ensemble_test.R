library(plyr)
library(caret)
library(e1071)
rm(list=ls())
load(file = "DataWrangling/Featuresselected.RData")
load(file = "Models/SVMradmodel.RData")
load(file = "Models/svmpolymodel.RData")
load(file = "Models/svmlinmodel.RData")
load(file = "Models/RF.RData")
load(file = "Models/Neural_Network.RData")
load(file = "Models/NB.RData")
load(file = "Models/LR.RData")
load(file = "Models/LDAmodel.RData")
load(file = "Models/knnmodel.RData")

train.df$Cath <- as.factor(ifelse(train.df$Cath == 0,"N","Y"))
test.df$Cath <- as.factor(ifelse(test.df$Cath == 0,"N","Y"))

generate_ensemble_df <- function(caddataset){
  #models <-  c(knn.model, lda.model, LR_model, NB_model, RF_model, svmlin.model, svmpoly.model, svmrad.model, nn1) this is some fucked up thing
  aggregate_pred.df <- as.data.frame(caddataset$Cath)
  colnames(aggregate_pred.df)=c("Cath")
  #attach predictions
  aggregate_pred.df$knnres <-  predict(knn.model, caddataset[knn.features])
  aggregate_pred.df$ldares <-  predict(lda.model, caddataset[lda.features$optVariables])
  aggregate_pred.df$lrres <-  predict(LR_model, caddataset[lr.features$optVariables])
  aggregate_pred.df$nbres <-  predict(NB_model, caddataset[nb.features$optVariables])
  aggregate_pred.df$rfres <-  predict(RF_model, caddataset[rf.features$optVariables])
  aggregate_pred.df$svmLres <-  predict(svmlin.model, caddataset[svmlin.features$optVariables])
  aggregate_pred.df$svmPres <-  predict(svmpoly.model, caddataset[svmpoly.features$optVariables])
  aggregate_pred.df$svmRres <-  predict(svmrad.model, caddataset[svmrad.features$optVariables])
  aggregate_pred.df$NNres <-  predict(nn1, caddataset)
  return(aggregate_pred.df)
}

#====Naive voting ensemble ====
vote_ensemble <- function(dataset, label="Cath"){
  #label should be string name of column
  df = dataset[,names(dataset) != c(label)]
  num = dim(df)[2]
  numericdf <- data.frame(apply(df, 2,function(x){revalue(x,c("Y"=1,"N"=0),warn_missing = FALSE)}))
  #change the sum formula here for weignting. Can dot product with weight vector.
  vote = apply(numericdf, 1, function(x) sum(as.numeric(x)))/num
  return(as.factor(ifelse(round(vote) == 0,"N","Y")))
}

pred.df <- generate_ensemble_df(train.df)
ensem_result <- vote_ensemble(pred.df)
confusionMatrix(ensem_result,train.df$Cath)

ensem_result_test <- vote_ensemble(generate_ensemble_df(test.df))
confusionMatrix(ensem_result_test,test.df$Cath)

#====Train logistic regression on result====
