load(file = "caddata.RData")
#Source https://neerajkumar.org/writings/svm/#:~:text=Prescaling%2Fnormalization%2Fwhitening,are%20different%20types%20of%20whitening.)

#Function to see see if feature was boolean (test if it is now -1 1 or not)
isBoolean <- function(x) { 
  dataval = unique(x)
  ret = (isTRUE(all.equal(dataval, c(-1,1)))|isTRUE(all.equal(dataval, c(1,-1))))
}

#Standardise non-boolean features
df1 <- as.data.frame(lapply(cad.df, function(x) if(!(isBoolean(x))){
  scale(x, center=TRUE, scale=TRUE)
  } else x))

