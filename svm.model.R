library(caret)
libraty(e1071)
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

svm.model <- train(Cath ~., data = cad.df.balanced, method = "svmRadial", trControl = train_control,  preProcess = c("center","scale"), tuneLength = 10)
#svm.model$bestTune radial
#sigma      C
#0.01182739 1

test.idx <- sample.int(n = nrow(cad.df.balanced), size = floor(0.2*nrow(cad.df.balanced)), replace = F)
svm.pred = predict(svm.model, cad.df.balanced[test.idx,])

table(pred = svm.pred, true = cad.df.balanced[test.idx,]$Cath) 
#svm.model$coefs
importance <- varImp(svm.model, scale=FALSE)
# summarize importance
print(importance)
plot(importance)
