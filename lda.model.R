#LDA assumes Gaussian prior on all features. Does not really work given our data, 
#would not be a good idea. Can apply to a subset of the data but we will avoid for now.
#resource: https://rpubs.com/maulikpatel/229684
#We should feature select AFTER train/test split. proof:https://stackoverflow.com/questions/56308116/should-feature-selection-be-done-before-train-test-split-or-after#:~:text=The%20contradicting%20answer%20is%20that,in%20random_state%20of%20the%20Train_Test_Split.

library(MASS)
library(caret)
rm(list=ls())
set.seed(101)
load(file = "caddata.RData")

#convert factors to numerical again because LDA does not like factors.
df1 <- as.data.frame(lapply(cad.df.balanced, function(x) if(is.factor(x)){
  as.numeric(x)-1
} else x))
df1$Cath <- as.factor(df1$Cath)

#Separate into training and test data

index <- sample.int(n = nrow(df1), size = floor(0.80*nrow(df1)), replace = F) #use caret
train.df = df1[index,]
test.df = df1[-index,]

#Feature selection using Recursive feature elimination on LDA - converted  
control <- rfeControl(functions=ldaFuncs, method="cv")
lda.features <- rfe(train.df[,names(df1) != c("Cath")], train.df$Cath, sizes=c(1:60), rfeControl=control) #takes a long time, care
plot(lda.features, type=c("g", "o"))
predictors(lda.features)
#21 features selected
#[1] "Typical.Chest.Pain" "Atypical"           "Age"                "EF.TTE"            
#[5] "FBS"                "HTN"                "ESR"                "DM"                
#[9] "Tinversion"         "K"                  "TG"                 "PR"                
#[13] "Neut"               "Nonanginal"         "Lymph"              "Na"                
#[17] "St.Depression"      "VHD.Mild"           "Region.RWMA2"       "Dyspnea"           
#[21] "PLT"   


#Train and test the model
lda.model = train(train.df[lda.features$optVariables], train.df$Cath, method="lda", trControl = trainControl(method = "cv"))
lda.pred = predict(lda.model, test.df[lda.features$optVariables])
table(pred = lda.pred, true = test.df$Cath) 
#table:true  (test accuracy .90, train accuracy .88
#pred  0  1
#0    14  3
#1     3 41

#Save resultant model
save(lda.model,lda.features, file = "LDAmodel.RData")
#we only really need lda.model