library(caret)
library(e1071)
rm(list=ls())
load(file = "caddata.RData")
load(file = "Featuresselected.RData")


#for error "Error in { : task 1 failed - "argument 1 is not a vector"," change factor levels from 01 to NY, as FS encodes it into factor name
#https://stackoverflow.com/questions/30663205/rfe-in-rs-caret-package-giving-error-as-task-1-failed-argument-1-is-not-a/30744475
train.df$Cath <- as.factor(ifelse(train.df$Cath == 0,"N","Y"))
test.df$Cath <- as.factor(ifelse(test.df$Cath == 0,"N","Y"))

#Source https://neerajkumar.org/writings/svm/#:~:text=Prescaling%2Fnormalization%2Fwhitening,are%20different%20types%20of%20whitening.)


#Feature selection using Recursive feature elimination 
control <- rfeControl(functions = caretFuncs, method="cv")
svm.features <- rfe(train.df[,names(train.df) != c("Cath")], train.df$Cath, sizes = c(1,5,10:20,25,30),rfeControl = control,method = "svmRadial")

predictors(svm.features)
#14 results,  [1] "Typical.Chest.Pain" "Atypical"           "Age"                "Nonanginal"        
#[5] "EF.TTE"             "HTN"                "Tinversion"         "Region.RWMA2"      
#[9] "VHD.Severe"         "DM"                 "Diastolic.Murmur"   "TG"                
#[13] "ESR"                "FBS" 


# Set up Repeated k-fold Cross Validation
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
form <- paste(lda.features$optVariables, collapse ="+")
svm.model <- train(form=as.formula(paste("Cath ~ ",form,sep = "")), data=train.df, method = "svmRadial", trControl = train_control,  preProcess = c("center","scale"), tuneLength = 10)
#svm.model$bestTune radial
#sigma      C
#0.01182739 1


svm.pred = predict(svm.model, newdata=test.df)

table(pred = svm.pred, true = test.df$Cath) 

