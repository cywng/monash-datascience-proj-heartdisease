library(e1071)
load(file = "caddata.RData")
#Source https://neerajkumar.org/writings/svm/#:~:text=Prescaling%2Fnormalization%2Fwhitening,are%20different%20types%20of%20whitening.)

#Standardise non-boolean features
df1 <- as.data.frame(lapply(cad.df, function(x) if(is.numeric(x)){
  scale(x, center=TRUE, scale=TRUE)
  } else x))

svm.model = svm(Cath~., data = df1[101:303,], cost = 100, gamma = 1)
svm.pred = predict(svm.model, df1[1:100,])

table(pred = svm.pred, true = df1[1:100,])
