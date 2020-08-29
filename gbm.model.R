#gbm assumes Gaussian prior on all features. Does not really work given our data, 
#would not be a good idea. Can apply to a subset of the data but we will avoid for now.
#resource: https://rpubs.com/maulikpatel/229684
#We should feature select AFTER train/test split. proof:https://stackoverflow.com/questions/56308116/should-feature-selection-be-done-before-train-test-split-or-after#:~:text=The%20contradicting%20answer%20is%20that,in%20random_state%20of%20the%20Train_Test_Split.

library(MASS)
library(caret)
rm(list=ls())
set.seed(123)
load(file = "caddata.RData")
load(file = "Featuresselected.RData") #import training and test data from the shared file

plot(rf.features, type=c("g", "o"))
predictors(rf.features)


#Train and test the model
gbmGrid <-  expand.grid(interaction.depth = c(1,2, 5, 9),n.trees = (1:15)*50,shrinkage = 0.1,n.minobsinnode = c(10,20) )
gbm.model = train(train.df[rf.features$optVariables], train.df$Cath, method="gbm", verbose=FALSE, trControl = trainControl(method = "cv"),tuneGrid = gbmGrid)

gbm.pred = predict(gbm.model, test.df[rf.features$optVariables])

confusionMatrix(gbm.pred,test.df$Cath)
#table:true  (test accuracy .87)

#Save resultant model
save(gbm.model,gbm.features, file = "gbmmodel.RData")
#we only really need gbm.model