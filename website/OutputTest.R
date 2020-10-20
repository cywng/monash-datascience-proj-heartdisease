library(stringr)
library(shinytest)
library(testthat)
library(here)
load((here(file ="website/FIT3164team1cadprediction/svm_linear.Rdata")))
load((here(file ="website/FIT3164team1cadprediction/svm_train_data.Rdata")))
context("Test Shiny app")
app <- shinytest::ShinyDriver$new("~/monash-datascience-proj-heartdisease/website/FIT3164team1cadprediction/")

test_that("output is correct", {
  i=1
  while (i<=10){
    #------
    if(as.numeric(svm.train$Typical.Chest.Pain[i])==0){
      Typical.Chest.Pain<-"No"
    }
    else{
   Typical.Chest.Pain<-"Yes"
    }
    #-----
    if(as.numeric(svm.train$DM[i])==0){
           DM<-"No"
    }
    else{
    DM<-"Yes"
    }
    #---------
    if(as.numeric(svm.train$HTN[i])==0){
      HTN<-"No"
    }
    else{
     HTN<-"Yes"
    }
    #-------------
    if(as.numeric(svm.train$Atypical[i])==0){
    Atypical<-"No"
    }
    else{
    Atypical<-"Yes"
    }
    data<-data.frame("Typical.Chest.Pain"=Typical.Chest.Pain,"Age"=svm.train$Age[i],
                     "Atypical"=Atypical,"FBS"=svm.train$FBS[i],"HTN"=HTN,
                     "DM"=DM,"EF.TTE"=svm.train$EF.TTE[i], "K"=svm.train$K[i],"BP"=svm.train$BP[i],"ESR"=svm.train$ESR[i], 
                     "TG"=svm.train$TG[i],"Region.RWMA"=svm.train$Region.RWMA[i])
    app$setInputs("Typical.Chest.Pain"=Typical.Chest.Pain,"Age"=svm.train$Age[i],
                  "Atypical"=Atypical,"FBS"=svm.train$FBS[i],"HTN"=HTN,
                  "DM"=DM,"EF.TTE"=svm.train$EF.TTE[i], "K"=svm.train$K[i],"BP"=svm.train$BP[i],"ESR"=svm.train$ESR[i], 
                  "TG"=svm.train$TG[i],"Region.RWMA"=svm.train$Region.RWMA[i])
    app$setInputs(button="click")
    output <- app$getValue(name="pred")
    a<-paste0("",output)
    numextract <- function(string){ 
      str_extract(string, "\\-*\\d+\\.*\\d*")
    } 
    num<-as.numeric(numextract(a))
    if(num>=0.5){
      result<-"Cad"
    }else{
      result<-"Normal"
    }
    
    Typical.Chest.Pain<-as.factor(as.numeric(ifelse(Typical.Chest.Pain=="Yes",1,0)))
    Atypical<-as.factor( ifelse(Atypical=="Yes","Y","N"))
    HTN<-as.factor(as.numeric(ifelse(HTN=="Yes",1,0)))
    DM<-as.factor(as.numeric(ifelse(DM=="Yes",1,0)))
    data<-data.frame("Typical.Chest.Pain"=Typical.Chest.Pain,"Age"=svm.train$Age[i],
                     "Atypical"=Atypical,"FBS"=svm.train$FBS[i],"HTN"=HTN,
                     "DM"=DM,"EF.TTE"=svm.train$EF.TTE[i], "K"=svm.train$K[i],BP=svm.train$BP[i],ESR=svm.train$ESR[i], 
                     "TG"=svm.train$TG[i],"Region.RWMA"=svm.train$Region.RWMA[i])
    pr=round(predict(svm.l,data[,!names(data) %in% c("Cath")],type = "prob")$Cad,2)
    if(pr>=0.5){
      test<-"Cad"
    }else{
      test<-"Normal"
    }
    expect_equal(result,test) 
    i=i+1
  }
  
  
  
  
  #app$setInputs("Typical.Chest.Pain"=0,"Age"=53,"Atypical"=0,"FBS"=90,"HTN"=1,"DM"=0,"EF.TTE"=50, "K"=4.7,"PR"=80,"ESR"=7,"TG"=250,"Tinversion"=1,"Lymph"=39,"Neut"=52,"St.Depression"=1,"Dyspnea"=0,"Nonanginal"=0,"Region.RWMA2"=0,"VHD.Mild"=0,"PLT"=261,"BMI"=29.38,"Na"=141)
  
}
)
# stop the Shiny app
app$stop()


