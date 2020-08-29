library(caret)
library(e1071)
rm(list=ls())
load(file = "caddata.RData")
load(file = "Featuresselected.RData")
set.seed(123)

train.df$Cath <- as.factor(ifelse(train.df$Cath == 0,"N","Y"))
test.df$Cath <- as.factor(ifelse(test.df$Cath == 0,"N","Y"))

#====KNN feature selection====
control <- rfeControl(functions = caretFuncs, method="cv")
knn.features <- rfe(train.df[,names(train.df) != c("Cath")], train.df$Cath, sizes = c(1:30,40,50),rfeControl = control,method = "knn")

predictors(knn.features) #Only selected one feature??
plot(knn.features, type=c("g", "o"))

knn.features <- tb.features
knn.num <- pickSizeTolerance(knn.features$results,metric= "Accuracy",maximize = TRUE, tol = 0.5)   
train_control <- trainControl(method="repeatedcv", number=10, repeats=10)
form <- paste(knn.features$optVariables[1:knn.num], collapse ="+")
knn.model <- train(form=as.formula(paste("Cath ~ ",form,sep = "")), data=train.df, method = "knn", trControl = train_control, preProcess = c("center","scale"), tuneLength = 10)


knn.model
knn.pred = predict(knn.model, newdata=test.df)
confusionMatrix(knn.pred,test.df$Cath)
#Other results: .86 acc LDA
#lr: .88 #nb .83 rf:87 tb:.85


#Confusion Matrix and Statistics for using 2 featuresm Typical Chest pain, age, k=9
#Reference
#Prediction  N  Y
#N 14  8
#Y  4 35
#Accuracy : 0.8033     
#save(knn.model,knn.features, file = "knnmodel.RData")

#wildly fluctuating results 78% here.
knn.modelall <- train(Cath~., data=train.df, method = "knn", trControl = train_control, preProcess = c("center","scale"), tuneLength = 10)
knn.modelall
knn.predall = predict(knn.modelall, newdata=test.df)
confusionMatrix(knn.predall,test.df$Cath)
