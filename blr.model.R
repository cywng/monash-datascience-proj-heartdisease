#LDA and QDA assume Gaussian prior on all features. Does not really work given our data, 
#would not be a good idea. Can apply to a subset of the data but we will avoid for now.
library(rstanarm)
rm(list=ls())
load(file = "caddata.RData")

#Standardise non-boolean features
df1 <- as.data.frame(lapply(cad.df, function(x) if(is.numeric(x)){
  scale(x, center=TRUE, scale=TRUE)
} else x))

#QDA gives error Error in qda.default(x, grouping, ...) : some group is too small for 'qda'
#need to feature select

set.seed(101)
test.idx <- sample.int(n = nrow(df1), size = floor(0.30*nrow(df1)), replace = F)

lda.model = lda(Cath~., data = df1[-test.idx,])
lda.pred = predict(lda.model, df1[test.idx,])

table(pred = lda.pred$class,true = df1[test.idx,]$Cath)
#svm.model$coefs