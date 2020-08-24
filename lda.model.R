#LDA assumes Gaussian prior on all features. Does not really work given our data, 
#would not be a good idea. Can apply to a subset of the data but we will avoid for now.
#resource: https://rpubs.com/maulikpatel/229684
#We should feature select AFTER train/test split. proof:https://stackoverflow.com/questions/56308116/should-feature-selection-be-done-before-train-test-split-or-after#:~:text=The%20contradicting%20answer%20is%20that,in%20random_state%20of%20the%20Train_Test_Split.

library(MASS)
library(caret)
rm(list=ls())
set.seed(123)
load(file = "caddata.RData")
load(file = "Featuresselected.RData") #import training and test data from the shared file

plot(lda.features, type=c("g", "o"))
predictors(lda.features)
#22 features selected
#[1] "Typical.Chest.Pain" "Age"                "Atypical"           "FBS"                "HTN"                "DM"                
#[7] "EF.TTE"             "K"                  "PR"                 "ESR"                "TG"                 "Tinversion"        
#[13] "Lymph"              "Neut"               "St.Depression"      "Dyspnea"            "Nonanginal"         "Region.RWMA2"      
#[19] "VHD.Mild"           "PLT"                "BMI"                "Na"   


#Train and test the model
lda.model = train(train.df[lda.features$optVariables], train.df$Cath, method="lda", trControl = trainControl(method = "cv"))
lda.pred = predict(lda.model, test.df[lda.features$optVariables])
table(pred = lda.pred, true = test.df$Cath) 
#table:true  (test accuracy .90, train accuracy .88
#pred  0  1
#0    14  3
#1     3 40

#Save resultant model
save(lda.model,lda.features, file = "LDAmodel.RData")
#we only really need lda.model