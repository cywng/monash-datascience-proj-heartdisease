library(shinytest)
library(testthat)
context("Test Shiny app")
app <- shinytest::ShinyDriver$new("~/monash-datascience-proj-heartdisease/website")

test_that("output is correct", {
  app$setInputs(Typical.Chest.Pain=0,Age=52,Atypical=0,FBS=90,HTN=1,DM=0,EF.TTE=50, K=4.7,PR=80,ESR=7,TG=250,Tinversion=1,Lymph=39,Neut=52,St.Depression=1,Dyspnea=0,Nonanginal=0,Region.RWMA2=0,VHD.Mild=0,PLT=261,BMI=29.38,Na=141)
  #app$setInputs("Typical.Chest.Pain"=0,"Age"=52,"Atypical"=0,"FBS"=90,"HTN"=1,"DM"=0,"EF.TTE"=50, "K"=4.7,"PR"=80,"ESR"=7,"TG"=250,"Tinversion"=1,"Lymph"=39,"Neut"=52,"St.Depression"=1,"Dyspnea"=0,"Nonanginal"=0,"Region.RWMA2"=0,"VHD.Mild"=0,"PLT"=261,"BMI"=29.38,"Na"=141)
  # get text_out
  app$setInputs(button="click")
  output <- app$getValue(name="pred")
  # test
  expect_equal(output,"The case has a 0.93 chance of having Coronary Artery Disease.")  
})
# stop the Shiny app
app$stop()