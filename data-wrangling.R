# Data wrangling
# Note the presence or lack of heart disease is the 'Cath' feature, with 1 for yes and 0 for no.
library(xlsx)
cad.df <- read.xlsx("Z-Alizadeh sani dataset.xlsx", 1, header=TRUE)
#This data set has no NA's or missing values.

#head(cad.df)
#Here we see that there are some categorical data. We create dummy variables for them all.
cad.df$Function.Class1 <- as.numeric(cad.df$Function.Class == 1)
cad.df$Function.Class2 <- as.numeric(cad.df$Function.Class == 2)
cad.df$Function.Class3 <- as.numeric(cad.df$Function.Class == 3)
cad.df$Function.Class <- as.numeric(cad.df$Function.Class == 0)

cad.df$LBBB <- as.numeric(cad.df$BBB == "LBBB")
cad.df$RBBB <- as.numeric(cad.df$BBB == "RBBB")

cad.df$VHD.Mild <- as.numeric(cad.df$VHD == 'mild')
cad.df$VHD.Moderate <- as.numeric(cad.df$VHD == 'Moderate')
cad.df$VHD.Severe <- as.numeric(cad.df$VHD == 'Severe')

#Remove old variables, as well as Exertional.CP, as all entries are N
cad.df = subset(cad.df, select=-c(Exertional.CP,VHD,BBB))

#Here we deal with changing "Y" "N" to 0 and 1.
for(i in colnames(cad.df))
{
  datarange = as.vector(unique(cad.df[,i]))
  if(isTRUE(all.equal(datarange, c("N","Y")))|isTRUE(all.equal(datarange, c("Y","N"))))
  {
    cad.df[,i] <- ifelse(cad.df[,i]=='Y',1,0)
  }
}

#Finally change the supervised variable (Cath) from "Cad" "Normal" to 1 0, respectively
cad.df$Cath <- ifelse(cad.df$Cath =='Cad',1,0)
save(cad.df, file = "caddata.RData")

#We can now normalise the data as required, as some methods are optimised for euclidean distance

#Here are some resources to read, as this will depend on the method used
#Main resource, note this is in python. 
  #https://towardsdatascience.com/normalization-vs-standardization-quantitative-analysis-a91e8a79cebf
#Reference for different scaling methods
  #https://scikit-learn.org/stable/auto_examples/preprocessing/plot_all_scaling.html#sphx-glr-auto-examples-preprocessing-plot-all-scaling-py

