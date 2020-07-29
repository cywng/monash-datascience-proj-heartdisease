# Data wrangling

library(xlsx)
cad.df <- read.xlsx("Z-Alizadeh sani dataset.xlsx", 1, header=TRUE)
cad.df.og = cad.df #copy for comparison, to be deleted after
#This data set has no NA's or missing values.

head(cad.df)
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

#Finally we can normalise the data, as some formulas are optimised for euclidian distance


