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

#====Naive voting ensemble ====
vote_ensemble <- function(dataset, label="Cath", prob='class',input='factor'){
  #label should be string name of column, or FALSE
  #prob = class means we want probabilities out.
  #input = factor means we putting in factors instead of probabilities.
  
  
  #Input: dataset - Any data set. 
  #Input: label - A column name to predict values for.
  #Input: prob - type of output. prob='class' returns 1 or 0, else probability between 0 and 1
  #Input: input - The type of inputs. Input ='factor' indicates that inputs are factors, else numeric.
  #Output: a vector containing the average value of all features for each input row.
  #To be used to take a unweighted vote of columns, aka. vote ensembling when combined with generate_ensemble_df
  
  if(label==FALSE){
    df = dataset
  }  else {
    df = dataset[,names(dataset) != c(label)]
  }
  if(input=='factor'){
    numericdf <- data.frame(apply(df, 2,function(x){revalue(x,c("Y"=1,"N"=0),warn_missing = FALSE)}))
  } else {numericdf <- df}
  num = dim(df)[2]
  vote = apply(numericdf, 1, function(x) sum(as.numeric(x)))/num #change the sum formula here for weignting. Can dot product with weight vector.
  if(prob=='class'){
    return(as.factor(ifelse(round(vote) == 0,"N","Y")))
  } else {return(vote)}
}

#====Generate a data frame of probabilities====
generate_ensemble_df <- function(caddataset){
  
  #Input: caddataset - a data frame with features identical to that of the initial CAD data set of size (nrow x 63)
  #Output: a data frame of size (nrow x 7). Each feature (column) is comprised of the predictions made by that specific model.
  #predictions are given as a probability between 0 and 1, with the three SVM models aggregated.
  #Runtime: Linear
  
  aggregate_pred.df <- as.data.frame(caddataset$Cath)
  aggregate_pred.df$knnres <-  predict(knn.model, caddataset[knn.features], type="prob")$Y
  aggregate_pred.df$ldares <-  predict(lda.model, caddataset[lda.features$optVariables], type="prob")$Y
  aggregate_pred.df$lrres <-  predict(LR_model, caddataset[lr.features$optVariables], type="prob")[,2]
  aggregate_pred.df$nbres <-  predict(NB_model, caddataset[nb.features$optVariables], type="prob")[,2]
  aggregate_pred.df$rfres <-  predict(RF_model, caddataset[rf.features$optVariables], type="prob")[,2]
  colnames(aggregate_pred.df)=c("Cath", "knn", "lda","lr","nb","rf")
  aggregate_pred.df$NNres <-  predict(nn1, caddataset, type="prob")$Y
  
  #SVM has no natural prediction, so we aggregate these 3.
  svm_res_agg <- as.data.frame(predict(svmlin.model, caddataset[svmlin.features$optVariables]))
  svm_res_agg$poly <-  predict(svmpoly.model, caddataset[svmpoly.features$optVariables])
  svm_res_agg$rad <-  predict(svmrad.model, caddataset[svmrad.features$optVariables])
  aggregate_pred.df$svm <- vote_ensemble(svm_res_agg, FALSE, prob='prob',input='factor')
  
  return(aggregate_pred.df)
}

pred.df <- generate_ensemble_df(train.df)

#====results====
ensem_result <- vote_ensemble(pred.df, label = 'Cath', prob='class',input='prob')
confusionMatrix(ensem_result,train.df$Cath)
#train accuracy 0.92

ensem_result_test <- vote_ensemble(generate_ensemble_df(test.df),label = 'Cath', prob='class',input='prob')
confusionMatrix(ensem_result_test,test.df$Cath)
#test accuracy 0.87

#====Train logistic regression on result====
#results in errors due to non-convergence as data is mostly uniform
#1: glm.fit: algorithm did not converge
#2: In predict.lm(object, newdata, se.fit, scale = 1, type = if (type ==  ... :
#                                         prediction from a rank-deficient fit may be misleading

control <- trainControl(method="repeatedcv", number=10)
lr_ensem <- train(Cath ~., data = pred.df, method="glm", family = "binomial" ,trControl=control)

ensem_lr_test=predict(lr_ensem,generate_ensemble_df(test.df))
confusionMatrix(ensem_lr_test,test.df$Cath)
#Test accuracy 0.84

#====Train RF====
#this is robust to the strange data that we have left. LDA did not like that some data was all 1.

control <- trainControl(method="repeatedcv", number=10)
rf_ensem<-train(Cath ~., data = pred.df, method="rf", family = "binomial" ,trControl=control)

ensem_rf_test=predict(rf_ensem,generate_ensemble_df(test.df))
confusionMatrix(ensem_rf_test,test.df$Cath)
#test accuracy 0.85

#====Try feature selection to drop some predictors====
control <- rfeControl(functions=rfFuncs, method="cv")
rf_ensem.features <- rfe(pred.df[,names(pred.df) != c("Cath")], pred.df$Cath, sizes=c(1:9), rfeControl=control)
print(rf_ensem.features)
#3 variables: rf, lr, svm.

plot(rf_ensem.features, type=c("g", "o"))
#Results show same accuracy for all feature subsets - this shows that this is not a good method of ensembling.
#Instead we should use the weights from each model as an indicator of how 'sure' it is, and be more affected by a higher certianty.

control <- trainControl(method="repeatedcv", number=10)
rf_ensem_fs<-train(Cath ~ rf + lr + svm, data = pred.df, method="rf", family = "binomial" ,trControl=control)

ensem_rf_test_fs=predict(rf_ensem_fs,generate_ensemble_df(test.df))
confusionMatrix(ensem_rf_test_fs,test.df$Cath)
#0.87 test accuracy. not great.
