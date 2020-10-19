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
  #Input: caddataset - a data frame with features identical to that of the initial CAD data set of size (nrow x 63)
  #Output: a data frame of size (nrow x 9). Each feature (column) is comprised of the predictions made by that specific model, as factors.
  #Runtime: Linear
  
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

pred.df <- generate_ensemble_df(train.df)
#====Naive voting ensemble ====
vote_ensemble <- function(dataset, label="Cath"){
  #Input: dataset - Any data set. 
  #Input label - A column name to predict values for.
  #Output: a vector containing the average value of all features for each input row.
  #Converts Y or N in input df to 1 and 0 respectively.
  #To be used to take a unweighted vote of columns, aka. vote ensembling when combined with generate_ensemble_df
  
  df = dataset[,names(dataset) != c(label)]
  num = dim(df)[2]
  numericdf <- data.frame(apply(df, 2,function(x){revalue(x,c("Y"=1,"N"=0),warn_missing = FALSE)}))
  vote = apply(numericdf, 1, function(x) sum(as.numeric(x)))/num
  return(as.factor(ifelse(round(vote) == 0,"N","Y")))
}

ensem_result <- vote_ensemble(pred.df)
confusionMatrix(ensem_result,train.df$Cath)

ensem_result_test <- vote_ensemble(generate_ensemble_df(test.df))
confusionMatrix(ensem_result_test,test.df$Cath)

#====Train logistic regression on result==== #These can be adjusted to give AUC/ROC

#results in errors due to non-convergence as data is mostly uniform
#1: glm.fit: algorithm did not converge
#2: In predict.lm(object, newdata, se.fit, scale = 1, type = if (type ==  ... :
#                                         prediction from a rank-deficient fit may be misleading

control <- trainControl(method="repeatedcv", number=10)
lr_ensem<-train(Cath ~., data = pred.df, method="glm", family = "binomial" ,trControl=control)

ensem_lr_test=predict(lr_ensem,generate_ensemble_df(test.df))
confusionMatrix(ensem_lr_test,test.df$Cath)

#====Train RF====

control <- trainControl(method="repeatedcv", number=10)
rf_ensem<-train(Cath ~., data = pred.df, method="rf", family = "binomial" ,trControl=control)

ensem_rf_test=predict(rf_ensem,generate_ensemble_df(test.df))
confusionMatrix(ensem_rf_test,test.df$Cath)

#====Try feature selection to drop some predictors====
control <- rfeControl(functions=rfFuncs, method="cv")
rf_ensem.features <- rfe(pred.df[,names(pred.df) != c("Cath")], pred.df$Cath, sizes=c(1:9), rfeControl=control)
print(rf_ensem.features)

plot(rf_ensem.features, type=c("g", "o"))
#Results show same accuracy for all feature subsets - this shows that this is not a good method of ensembling.
#Instead we should use the weights from each model as an indicator of how 'sure' it is, and be more affected by a higher certianty.
