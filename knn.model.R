library(caret)
library(e1071)
rm(list=ls())
load(file = "caddata.RData")
load(file = "Featuresselected.RData")
set.seed(123)

train.df$Cath <- as.factor(ifelse(train.df$Cath == 0,"N","Y"))
test.df$Cath <- as.factor(ifelse(test.df$Cath == 0,"N","Y"))

#====KNN feature selection====
train_control <- trainControl(method="repeatedcv", number=10, repeats=10)
knn.modelall <- train(Cath~., data=train.df, method = "knn", trControl = train_control, preProcess = c("center","scale"), tuneLength = 10)
importance <- varImp(knn.modelall,scale = FALSE)
knn.features <- row.names(importance$importance[c(importance$importance[,1]>0.57),])
form <- paste(knn.features, collapse ="+")

#==== Train model based on features with more than 0.57 importance.====
knn.model <- train(form=as.formula(paste("Cath ~ ",form,sep = "")), data=train.df, method = "knn", trControl = train_control, preProcess = c("center","scale"), tuneLength = 10)

knn.model
knn.pred = predict(knn.model, newdata=test.df[knn.features])
confusionMatrix(knn.pred,test.df$Cath)

save(knn.model, knn.features, file="knnmodel.RData")
