library(shiny)
library(caret)
load(file = "svm_linear.RData")
load(file = "svm_train_data.RData")

Age<-round(mean(svm.train$Age),digits=0)  #
DM<-as.factor(round(mean(as.numeric(svm.train$DM)),digits=0))#
HTN<-as.factor(round(mean(as.numeric(svm.train$HTN)),digits=0))#
BP<-round(mean(svm.train$BP),digits=0)
Typical.Chest.Pain<-as.factor(round(mean(as.numeric(svm.train$Typical.Chest.Pain)),digits=0))
Atypical<-as.factor(round(mean(as.numeric(svm.train$Atypical)),digits=0))
FBS<-round(mean(svm.train$FBS),digits=0)#
TG<-round(mean(svm.train$TG),digits=0)#
ESR<-round(mean(svm.train$ESR),digits=0)#
K<-round(mean(svm.train$K),digits=0)#
EF.TTE<-round(mean(svm.train$EF.TTE),digits=0)#
Region.RWMA<-as.factor(round(mean(as.numeric(svm.train$Region.RWMA)),digits=0))

fluidPage(
  h1("Heart Disease Prediction",align = "center"),#titlePanel
  p("This is a tool to aid the diagnosis of Coronary Artery Disease. Please input the patient's information into the fields, or upload a CSV."),
  p("Typical values have been displayed brackets. An entry of 0 for a numeric category is treated as a null input. Blank entries or entries outside the range are acceptable, but will result in a lower confidence than displayed."),

  br(),
  
  sidebarLayout(
    sidebarPanel (
      fluidRow(
        #
        column(width =6 ,selectInput("Typical.Chest.Pain","Presence of chest pain:",c("Unknown","Yes","No"),multiple = F,selected =NULL)),
      #
          column(width =6 ,numericInput("Age","Age (30-86):",0))),
      
      fluidRow(
        #
        column(width =6, selectInput("Atypical","Atypical Pulse:",c("Unknown","Yes","No"),multiple = F,selected =NULL)), #N,Y 
       #
         column(width =6,numericInput("FBS","Fasting blood sugar level (mg/dl; 62-400):",0))),
      
      fluidRow(
        #
        column(width =6,selectInput("HTN","Hypertension:",c("Unknown","Yes","No"),multiple = F,selected =NULL)),
        #
        column(width =6,selectInput("DM","Diabetes Mellitus:",c("Unknown","Yes","No"),multiple = F,selected =NULL))),
      
      fluidRow(
        #
        column(width =6,numericInput("EF.TTE","Ejection Fraction (%; 15-60):",0)),
        #
        column(width =6,numericInput("K","Blood potassium Content (mEq/lit; 3.0-6.6):",0))), #float
      
      fluidRow(
        #
        column(width =6,numericInput("BP","Blood pressure (mmHg; 90–190):",0)),
        #
        column(width =6,numericInput("ESR","Erythrocyte sedimentation rate (mm/h; 1-90):",0))),
      
      fluidRow(
        #
        column(width =6,numericInput("TG","Blood triclyceride concentration (mg/dl; 35-1050):",0)),
        #need modify
        column(width =6,selectInput("Region.RWMA","Regional wall motion abnormality in heart region:",c(0,1,2,3,4),multiple = F,selected =NULL))),#region,RWAMA:023,
      

      fileInput("file1","or upload a CSV File",accept = ".csv"),
    
      actionButton("button", "Get prediction",width = "100%",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      #  submitButton("Submit",width = "100%"),
      width="100%"),
    mainPanel(
      textOutput("pred" ),
      tags$head(tags$style("#pred{color: black;
                               font-size: 20px;
                               }"
      )
      )

    )
  )
)