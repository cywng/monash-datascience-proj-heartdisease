library(caret)
rm(list=ls())
load(file = "DataWrangling/Featuresselected.RData")
load(file = "Models/LDAmodel.RData")
#train.df$Cath <- as.factor(ifelse(train.df$Cath == 0,"N","Y"))
#test.df$Cath <- as.factor(ifelse(test.df$Cath == 0,"N","Y"))

pred <- predict(lda.model, test.df[1,][lda.model$finalModel$xNames])
#pred for first entry is Y

#====investigate model====
var_names <- lda.model$finalModel$xNames

## [1] "Typical.Chest.Pain" "Age"                "Atypical"          
## [4] "FBS"                "HTN"                "DM"                
## [7] "EF.TTE"             "K"                  "PR"                
## [10] "ESR"                "TG"                 "Tinversion"        
## [13] "Lymph"              "Neut"               "St.Depression"     
## [16] "Dyspnea"            "Nonanginal"         "Region.RWMA2"      
## [19] "VHD.Mild"           "PLT"                "BMI"               
## [22] "Na"  

# show parameter definition for the  features
for (i in 1:22) {
  var <- var_names[i]
  train_data_subs <- train.df[, which(colnames(train.df) == var)]
  type <- class(train_data_subs)
  if (type == "numeric") {
    min <- min(train_data_subs)
    max <- max(train_data_subs)
  }
  cat("Variable:", var, "is of type:", type, "\n",
      "Min value in training data =", min, "\n",
      "Max value in training data =", max, "\n----------\n")
}

#==== convert to json ====
input_data = test.df[1,][lda.model$finalModel$xNames]
#first entry as our test data

library(rjson)
test_case_json <- toJSON(input_data)
cat(test_case_json)
