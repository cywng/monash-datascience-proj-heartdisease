library(caret)
library(e1071)
rm(list=ls())
load(file = "caddata.RData")
df1 <- cad.df.balanced
#Source https://neerajkumar.org/writings/svm/#:~:text=Prescaling%2Fnormalization%2Fwhitening,are%20different%20types%20of%20whitening.)

set.seed(101)
index <- sample.int(n = nrow(df1), size = floor(0.80*nrow(df1)), replace = F) #use caret
train.df = df1[index,]
test.df = df1[-index,]


#Feature selection using Recursive feature elimination 
control <- rfeControl(functions=rfFuncs, method="cv")
svm.features <- rfe(train.df[,names(df1) != c("Cath")], train.df$Cath, sizes=c(1:60), rfeControl=control) #takes a long time, care
plot(svm.features, type=c("g", "o"))
predictors(svm.features)
#14 results,  [1] "Typical.Chest.Pain" "Atypical"           "Age"                "Nonanginal"        
#[5] "EF.TTE"             "HTN"                "Tinversion"         "Region.RWMA2"      
#[9] "VHD.Severe"         "DM"                 "Diastolic.Murmur"   "TG"                
#[13] "ESR"                "FBS" 


# Set up Repeated k-fold Cross Validation
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)

svm.model <- train(Cath ~., data = train.df, method = "svmRadial", trControl = train_control,  preProcess = c("center","scale"), tuneLength = 10)
#svm.model$bestTune radial
#sigma      C
#0.01182739 1


svm.pred = predict(svm.model, test.df)

table(pred = svm.pred, true = test.df$Cath) 

