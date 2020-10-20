library(stringr)
library(shinytest)
library(testthat)
context("Test Shiny app")
app <- shinytest::ShinyDriver$new("~/monash-datascience-proj-heartdisease/website/FIT3164team1cadprediction/")

test_that("output is correct", {
  app$setInputs("Typical.Chest.Pain"=0,"Age"=53,
                "Atypical"="N","FBS"=90,"HTN"=1,
                "DM"=0,"EF.TTE"=50, "K"=4.7,BP=110,ESR=7, 
                "TG"=250,"Region.RWMA"=0)
  #app$setInputs("Typical.Chest.Pain"=0,"Age"=53,"Atypical"=0,"FBS"=90,"HTN"=1,"DM"=0,"EF.TTE"=50, "K"=4.7,"PR"=80,"ESR"=7,"TG"=250,"Tinversion"=1,"Lymph"=39,"Neut"=52,"St.Depression"=1,"Dyspnea"=0,"Nonanginal"=0,"Region.RWMA2"=0,"VHD.Mild"=0,"PLT"=261,"BMI"=29.38,"Na"=141)
  # get text_out
  app$setInputs(button="click")
  output <- app$getValue(name="pred")
  a<-paste0("",output)
  numextract <- function(string){ 
    str_extract(string, "\\-*\\d+\\.*\\d*")
  } 
  num<-as.numeric(numextract(a))
  if(num>=0.5){
    result<-"Y"
  }else{
    result<-"N"
  }
  expect_equal(result,"Y") 
  
}
)
# stop the Shiny app
app$stop()