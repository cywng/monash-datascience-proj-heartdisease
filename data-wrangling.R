# Data wrangling
# Note the presence or lack of heart disease is the 'Cath' feature, with 1 for yes and 0 for no.
library(xlsx)
library(caret)
cad.df <- read.xlsx("Z-Alizadeh sani dataset.xlsx", 1, header=TRUE)
#This data set has no NA's or missing values.
#====Categorical Data====
#Here we see that there are some categorical data. We create dummy variables for them all.

#Note: This results in multicollinearity in our dataset and so we won't
#be able to use our weight vector to calculate the feature importance.
#To avoid this, we drop one of the feature columns. This does not lose info,
#as we can reconstruct it from the other columns.
cad.df$Function.Class1 <- as.factor(ifelse(cad.df$Function.Class == 1,1,-1))
cad.df$Function.Class2 <- as.factor(ifelse(cad.df$Function.Class == 2,1,-1))
cad.df$Function.Class3 <- as.factor(ifelse(cad.df$Function.Class == 3,1,-1))

cad.df$LBBB <- as.factor(ifelse(cad.df$BBB == "LBBB",1,-1))
cad.df$RBBB <- as.factor(ifelse(cad.df$BBB == "RBBB",1,-1))

cad.df$VHD.Mild <- as.factor(ifelse(cad.df$VHD == 'mild',1,-1))
cad.df$VHD.Moderate <- as.factor(ifelse(cad.df$VHD == 'Moderate',1,-1))
cad.df$VHD.Severe <- as.factor(ifelse(cad.df$VHD == 'Severe',1,-1))

cad.df$Region.RWMA1 <- as.factor(ifelse(cad.df$Region.RWMA == 1,1,-1))
cad.df$Region.RWMA2 <- as.factor(ifelse(cad.df$Region.RWMA == 2,1,-1))
cad.df$Region.RWMA3 <- as.factor(ifelse(cad.df$Region.RWMA == 3,1,-1))
cad.df$Region.RWMA4 <- as.factor(ifelse(cad.df$Region.RWMA == 4,1,-1))

#====Boolean strings to factosr====
cad.df$Sex <- as.factor(ifelse(cad.df$Sex == 'Male', 1, -1))
#Remove old variables, as well as Exertional.CP, as all entries are N
cad.df = subset(cad.df, select=-c(Exertional.CP,VHD,BBB,Function.Class, Region.RWMA))

#Here we deal with changing "Y" "N" to 0 and 1, as well as 0 1 to -1 and 1 (unbiased data, centered on 0)
for(i in colnames(cad.df))
{
  datarange = as.vector(unique(cad.df[,i]))
  if(isTRUE(all.equal(datarange, c("N","Y")))|isTRUE(all.equal(datarange, c("Y","N"))))
  {
    cad.df[,i] <- as.factor(ifelse(cad.df[,i]=='Y',1,-1))
  }
  else if(isTRUE(all.equal(datarange, c(0,1)))|isTRUE(all.equal(datarange, c(1,0))))
  {
    cad.df[,i] <- as.factor(ifelse(cad.df[,i]==1,1,-1))
  }
}

#====Extra: Remove features based on high correlation====
#Result: colums 2 and 18 removed.
df1 <- Filter(is.numeric, cad.df)
correlationMat <- cor(df1[,-51]) #Exclude target heart disease
highlyCorrelated <- findCorrelation(correlationMat, cutoff=0.70)
#We've chosen the cutoff to be 0.7 as that is a common threshold for 'highly correlated'
#Source: https://www.westga.edu/academics/research/vrc/assets/docs/scatterplots_and_correlation_notes.pdf

cad.df = subset(cad.df, select=-as.numeric(highlyCorrelated))

#Finally change the supervised variable (Cath) from "Cad" "Normal" to 1 0, respectively
cad.df$Cath <- as.factor(ifelse(cad.df$Cath =='Cad',1,-1))

#====remove very imbalanced features (95%)====
cad.df.balanced = cad.df
for(i in colnames(cad.df.balanced)){
  if(is.factor(cad.df.balanced[,i])){
    datapoints = summary(cad.df.balanced[,i])
    if(datapoints[1]/datapoints[2] > 40 | datapoints[2]/datapoints[1] > 40){
      cad.df.balanced[,i] <- NULL
      print(i)
    }
  }
}

save(cad.df, cad.df.balanced, file = "caddata.RData")

rm(list=ls())
#We can now normalise the data as required, as some methehods are optimised for euclidean distance

#Here are some resources to read, as this will depend on the method used
#Main resource, note this is in python. 
#https://towardsdatascience.com/normalization-vs-standardization-quantitative-analysis-a91e8a79cebf
#Reference for different scaling methods
#https://scikit-learn.org/stable/auto_examples/preprocessing/plot_all_scaling.html#sphx-glr-auto-examples-preprocessing-plot-all-scaling-py
