library(caret)
library(e1071)
rm(list=ls())
load(file = "DataWrangling/caddata.RData")
load(file = "DataWrangling/Featuresselected.RData")
set.seed(123)


#for error "Error in { : task 1 failed - "argument 1 is not a vector"," change factor levels from 01 to NY, as FS encodes it into factor name
#https://stackoverflow.com/questions/30663205/rfe-in-rs-caret-package-giving-error-as-task-1-failed-argument-1-is-not-a/30744475
train.df$Cath <- as.factor(ifelse(train.df$Cath == 0,"N","Y"))
test.df$Cath <- as.factor(ifelse(test.df$Cath == 0,"N","Y"))

#Source https://neerajkumar.org/writings/svm/#:~:text=Prescaling%2Fnormalization%2Fwhitening,are%20different%20types%20of%20whitening.)

#====Polynominal Function====
control <- rfeControl(functions = caretFuncs, method="cv")
#takes 1 hour
svmpoly.features <- rfe(train.df[,names(train.df) != c("Cath")], train.df$Cath, sizes = c(1,5,10:25,30),rfeControl = control,method = "svmPoly")

predictors(svmpoly.features)
plot(svmpoly.features, type=c("g", "o"))
#25 features selected, but the results are very close. We take tol = 0.5% training accuracy.
svmpoly.num <- pickSizeTolerance(svmpoly.features$results,metric= "Accuracy",maximize = TRUE, tol = 0.5)   

# Set up Repeated k-fold Cross Validation
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
form <- paste(svmpoly.features$optVariables[1:svmpoly.num], collapse ="+")

#This takes like 4 hours to run
svmpoly.model <- train(form=as.formula(paste("Cath ~ ",form,sep = "")), data=train.df, method = "svmPoly", trControl = train_control, preProcess = c("center","scale"), tuneLength = 10)
svmpoly.model$bestTune
plot(svmpoly.model)


svmpoly.pred = predict(svmpoly.model, newdata=test.df[svmpoly.features$optVariables])

confusionMatrix(svmpoly.pred,test.df$Cath)

save(svmpoly.model,svmpoly.features, file = "Models/svmpolymodel.RData")

