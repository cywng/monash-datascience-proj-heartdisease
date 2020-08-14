library(e1071)
rm(list=ls())
load(file = "caddata.RData")
#Source https://neerajkumar.org/writings/svm/#:~:text=Prescaling%2Fnormalization%2Fwhitening,are%20different%20types%20of%20whitening.)

#Standardise non-boolean features
#df1 <- as.data.frame(lapply(cad.df, function(x) if(is.numeric(x)){
#  scale(x, center=TRUE, scale=TRUE)
#  } else x))
#df1 <- cad.df

set.seed(101)
train_control <- trainControl(method="repeatedcv", number=10, repeats=10)

#caret
#svm.model = svm(Cath~., data = df1[-test.idx,], kernal = "lin",cost = 1)
#svm.model = svm(Cath~., data = df1[-test.idx,], kernal = "polynomial",cost = 1,degree = 1,gamma=1,coef0 = 0)
#this does not work yet. i am confused. It should be the same as linear
svm.model <- train(Cath ~., data = cad.df, method = "svmLinear", trControl = train_control,  preProcess = c("center","scale"))
svm.pred = predict(svm.model, cad.df[test.idx,])

table(pred = svm.pred, true = df1[test.idx,]$Cath) 
#svm.model$coefs
