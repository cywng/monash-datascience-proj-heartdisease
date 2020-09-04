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
svmrad.features <- rfe(train.df[,names(train.df) != c("Cath")], train.df$Cath, sizes = c(1,5,10:25,30),rfeControl = control,method = "svmRadial")

predictors(svmrad.features)
plot(svmrad.features, type=c("g", "o"))
#25 features selected, but the results are very close. We take tol = 0.5% training accuracy.
svmrad.num <- pickSizeTolerance(svmrad.features$results,metric= "Accuracy",maximize = TRUE, tol = 0.5)   

# Set up Repeated k-fold Cross Validation
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
form <- paste(svmrad.features$optVariables[1:svmrad.num], collapse ="+")
grid_radial <- expand.grid(sigma = c(0,0.01, 0.02, 0.025, 0.03, 0.04,
                                     0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
                           C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75,
                                 1, 1.5, 2,5))
svmrad.model <- train(form=as.formula(paste("Cath ~ ",form,sep = "")), data=train.df, method = "svmRadial", trControl = train_control,  tuneGrid = grid_radial, preProcess = c("center","scale"), tuneLength = 10)
svmrad.model$bestTune



svmrad.pred = predict(svmrad.model, newdata=test.df[svmrad.features$optVariables])
#88.5% test accuracy

confusionMatrix(svmrad.pred,test.df$Cath)
save(svmrad.model,svmrad.features, file = "SVMradmodel.RData")

