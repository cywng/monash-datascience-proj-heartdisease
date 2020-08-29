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


knn.num <- pickSizeTolerance(knn.features$results,metric= "Accuracy",maximize = TRUE, tol = 0.5)   
train_control <- trainControl(method="repeatedcv", number=10, repeats=10)
form <- paste(knn.features$optVariables[1:knn.num], collapse ="+")
knn.model <- train(form=as.formula(paste("Cath ~ ",form,sep = "")), data=train.df, method = "knn", trControl = train_control, preProcess = c("center","scale"), tuneLength = 10)


knn.model
knn.pred = predict(knn.model, newdata=test.df)
confusionMatrix(knn.pred,test.df$Cath)


save(knn.model,knn.features, file = "knnmodel.RData")

#wildly fluctuating results 
knn.modelall <- train(Cath~., data=train.df, method = "knn", trControl = train_control, preProcess = c("center","scale"), tuneLength = 10)
knn.modelall
knn.predall = predict(knn.modelall, newdata=test.df)
confusionMatrix(knn.predall,test.df$Cath)
