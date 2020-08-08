# Data wrangling
# Note the presence or lack of heart disease is the 'Cath' feature, with 1 for yes and 0 for no.
library(xlsx)
cad.df <- read.xlsx("Z-Alizadeh sani dataset.xlsx", 1, header=TRUE)
#This data set has no NA's or missing values.
#Here we see that there are some categorical data. We create dummy variables for them all.

#Note: This results in multicollinearity in our dataset and so we won't
#be able to use our weight vector to calculate the feature importance.
#To avoid this, we drop one of the feature columns. This does not lose info,
#as we can reconstruct it from the other columns.
cad.df$Function.Class1 <- ifelse(cad.df$Function.Class == 1,1,-1)
cad.df$Function.Class2 <- ifelse(cad.df$Function.Class == 2,1,-1)
cad.df$Function.Class3 <- ifelse(cad.df$Function.Class == 3,1,-1)

cad.df$LBBB <- ifelse(cad.df$BBB == "LBBB",1,-1)
cad.df$RBBB <- ifelse(cad.df$BBB == "RBBB",1,-1)

cad.df$VHD.Mild <- ifelse(cad.df$VHD == 'mild',1,-1)
cad.df$VHD.Moderate <- ifelse(cad.df$VHD == 'Moderate',1,-1)
cad.df$VHD.Severe <- ifelse(cad.df$VHD == 'Severe',1,-1)

cad.df$Region.RWMA1 <- ifelse(cad.df$Region.RWMA == 1,1,-1)
cad.df$Region.RWMA2 <- ifelse(cad.df$Region.RWMA == 2,1,-1)
cad.df$Region.RWMA3 <- ifelse(cad.df$Region.RWMA == 3,1,-1)
cad.df$Region.RWMA4 <- ifelse(cad.df$Region.RWMA == 4,1,-1)

cad.df$Sex <- ifelse(cad.df$Sex == 'Male', 1, -1)

#Remove old variables, as well as Exertional.CP, as all entries are N
cad.df = subset(cad.df, select=-c(Exertional.CP,VHD,BBB,Function.Class, Region.RWMA))

#Here we deal with changing "Y" "N" to 0 and 1, as well as 0 1 to -1 and 1 (unbiased data, centered on 0)
for(i in colnames(cad.df))
{
  datarange = as.vector(unique(cad.df[,i]))
  if(isTRUE(all.equal(datarange, c("N","Y")))|isTRUE(all.equal(datarange, c("Y","N"))))
  {
    cad.df[,i] <- ifelse(cad.df[,i]=='Y',1,-1)
  }
  else if(isTRUE(all.equal(datarange, c(0,1)))|isTRUE(all.equal(datarange, c(1,0))))
  {
    cad.df[,i] <- ifelse(cad.df[,i]==1,1,-1)
  }
}


#Finally change the supervised variable (Cath) from "Cad" "Normal" to 1 0, respectively
cad.df$Cath <- ifelse(cad.df$Cath =='Cad',1,-1)
save(cad.df, file = "caddata.RData")

#We can now normalise the data as required, as some methods are optimised for euclidean distance

#Here are some resources to read, as this will depend on the method used
#Main resource, note this is in python. 
  #https://towardsdatascience.com/normalization-vs-standardization-quantitative-analysis-a91e8a79cebf
#Reference for different scaling methods
  #https://scikit-learn.org/stable/auto_examples/preprocessing/plot_all_scaling.html#sphx-glr-auto-examples-preprocessing-plot-all-scaling-py

