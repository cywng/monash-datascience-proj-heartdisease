# Data wrangling

library(xlsx)
cad.df <- read.xlsx("Z-Alizadeh sani dataset.xlsx", 1, header=TRUE)
#This data set has no NA's or missing values.

head(cad.df)
#Here we see that there are some boolean feature to be changed to 0 and 1, as well as one categorical feature (Function.Class)
cad.df$Function.Class1 <- as.numeric(cad.df$Function.Class == 1)
cad.df$Function.Class2 <- as.numeric(cad.df$Function.Class == 2)
cad.df$Function.Class3 <- as.numeric(cad.df$Function.Class == 3)
cad.df$Function.Class <- as.numeric(cad.df$Function.Class == 0)


for(i in colnames(cad.df)) ##TODO: this works for indexing via $ as it is a char, but not for indexing using [] as it is a dataframe.
{
  if(isTRUE(all.equal(as.vector(range(cad.df[,i])), c("N","Y"))))
  {
    print(i)
    cad.df[,i] <- ifelse(cad.df[,i]=='Y',1,0)
  }
}

#Now we have to deal with BBB, and the categorical VHD.

#Finally we can normalise the data



