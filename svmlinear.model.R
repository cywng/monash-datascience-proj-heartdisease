library(caret)
library(e1071)
rm(list=ls())
load(file = "caddata.RData")
load(file = "Featuresselected.RData")
set.seed(123)


#for error "Error in { : task 1 failed - "argument 1 is not a vector"," change factor levels from 01 to NY, as FS encodes it into factor name
#https://stackoverflow.com/questions/30663205/rfe-in-rs-caret-package-giving-error-as-task-1-failed-argument-1-is-not-a/30744475
train.df$Cath <- as.factor(ifelse(train.df$Cath == 0,"N","Y"))
test.df$Cath <- as.factor(ifelse(test.df$Cath == 0,"N","Y"))

#Source https://neerajkumar.org/writings/svm/#:~:text=Prescaling%2Fnormalization%2Fwhitening,are%20different%20types%20of%20whitening.)

#====Radial Basis Function====
control <- rfeControl(functions = caretFuncs, method="cv")
svmlin.features <- rfe(train.df[,names(train.df) != c("Cath")], train.df$Cath, sizes = c(1,5,10:25,30),rfeControl = control,method = "svmLinear")

predictors(svmlin.features)
plot(svmlin.features, type=c("g", "o"))
#25 features selected, but the results are very close. We take tol = 0.5% training accuracy.
svmlin.num <- pickSizeTolerance(svmlin.features$results,metric= "Accuracy",maximize = TRUE, tol = 0.5)   

# Set up Repeated k-fold Cross Validation
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
form <- paste(svmlin.features$optVariables[1:svmlin.num], collapse ="+")
svmlin.model <- train(form=as.formula(paste("Cath ~ ",form,sep = "")), data=train.df, method = "svmLinear", trControl = train_control,  tuneGrid = expand.grid(C=c(0.01,0.1,0.3,0.5,1,2,5)), preProcess = c("center","scale"), tuneLength = 10)
svmlin.model$bestTune



svmlin.pred = predict(svmlin.model, newdata=test.df)
#87% test accuracy

confusionMatrix(svmlin.pred,test.df$Cath)
save(svmlin.model,svmlin.features, file = "svmlinmodel.RData")

