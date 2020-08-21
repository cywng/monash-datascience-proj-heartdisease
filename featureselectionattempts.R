#Testing a bunch of feature selection algs.
library(caret)
library(klaR)
library(RVenn)
rm(list=ls())
set.seed(123)
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

#====LDA====
control <- rfeControl(functions=ldaFuncs, method="cv")
lda.features <- rfe(train.df[,names(df1) != c("Cath")], train.df$Cath, sizes=c(1:30), rfeControl=control) 

#====Logistic Regression====
control <- rfeControl(functions=lrFuncs, method="cv")
lr.features <- rfe(train.df[,names(df1) != c("Cath")], train.df$Cath, sizes=c(1:30), rfeControl=control) 

#====RandomForest====
control <- rfeControl(functions=rfFuncs, method="cv")
rf.features <- rfe(train.df[,names(df1) != c("Cath")], train.df$Cath, sizes=c(1:30), rfeControl=control)

#====Naive Bayes====
control <- rfeControl(functions=nbFuncs, method="cv")
nb.features <- rfe(train.df[,names(df1) != c("Cath")], train.df$Cath, sizes=c(1:30), rfeControl=control)

#====Plot all results====
p1 <- plot(lda.features, type=c("g", "o"),main="LDA")
p2 <- plot(lr.features, type=c("g", "o"),main="LogisticRegression")
p3 <- plot(rf.features, type=c("g", "o"),main="RandForest")
p4 <- plot(nb.features, type=c("g", "o"),main="NaiveBayes")

print(p1, split=c(1,1,2,2), more=TRUE)
print(p2, split=c(1,2,2,2), more=TRUE)
print(p3, split=c(2,1,2,2), more=TRUE)
print(p4, split=c(2,2,2,2), more=TRUE)

#====Check all for best within 3% accuracy.====

lda.num <- pickSizeTolerance(lda.features$results,metric= "Accuracy",maximize = TRUE, tol = 0.5)
lr.num <- pickSizeTolerance(lr.features$results,metric= "Accuracy",maximize = TRUE, tol = 0.5)
rf.num <- pickSizeTolerance(rf.features$results,metric= "Accuracy",maximize = TRUE, tol = 0.5)
nb.num <- pickSizeTolerance(nb.features$results,metric= "Accuracy",maximize = TRUE, tol = 0.5)

#====Print out the variables====
a=lda.features$optVariables#[1:lda.num]
b=lr.features$optVariables#[1:lr.num]
c=rf.features$optVariables#[1:rf.num]
d=nb.features$optVariables#[1:nb.num]

#====Intersections====
var.dia = Venn(list(a,b,c,d))
overlap(var.dia)
               