library(e1071)
rm(list=ls())
load(file = "caddata.RData")
#Source https://neerajkumar.org/writings/svm/#:~:text=Prescaling%2Fnormalization%2Fwhitening,are%20different%20types%20of%20whitening.)

#Standardise non-boolean features
df1 <- as.data.frame(lapply(cad.df, function(x) if(is.numeric(x)){
  scale(x, center=TRUE, scale=TRUE)
  } else x))

set.seed(7)
test.idx <- sample.int(n = nrow(df1), size = floor(0.30*nrow(df1)), replace = F)

svm.model = svm(Cath~., data = df1[-test.idx,], kernal = "radial",cost = 1000000000, gamma = 1)
svm.pred = predict(svm.model, df1[test.idx,])

table(pred = svm.pred, true = df1[test.idx,]$Cath)
svm.model$coefs
