library(shiny)
library(Hmisc)
library(caret)
load(file = "LDAmodel.RData")
load(file = "Featuresselected.RData")

Age<-round(mean(train.df$Age),digits=0)
FBS<-round(mean(train.df$FBS),digits=0)
EF.TTE<-round(mean(train.df$EF.TTE),digits=0)
K<-round(mean(train.df$EF.TTE),digits=0)
PR<-round(mean(train.df$K),digits=0)
ESR<-round(mean(train.df$ESR),digits=0)
TG<-round(mean(train.df$TG),digits=0)
Lymph<-round(mean(train.df$Lymph),digits=0)
Neut<-round(mean(train.df$Neut),digits=0)
PLT<-round(mean(train.df$PLT),digits=0)
BMI<-round(mean(train.df$BMI),digits=0)
Na<-round(mean(train.df$Na),digits=0)

fluidPage(
  h1("Heart Disease Prediction",align = "center"),#titlePanel
  p("This is a tool to aid the diagnosis of Coronary Artery Disease. Please input the patient's information into the fields, or upload a CSV."),
  p("Typical values are in brackets. An entry of 0 for a numeric category is treated as a null input. Blank entries are acceptable, but will result in a lower confidence than displayed."),
  # titlePanel("Heart Disease Prediction"),
  br(),
  
  sidebarLayout(
    sidebarPanel (
      fluidRow(
        column(width =6 ,selectInput("Typical.Chest.Pain","Presence of chest pain:",c("Unknown","Yes","No"),multiple = F,selected =NULL)),
        column(width =6 ,numericInput("Age","Age (30-86):",0))),
      
      fluidRow(
        column(width =6, selectInput("Atypical","Atypical Pulse:",c("Unknown","Yes","No"),multiple = F,selected =NULL)), #N,Y
        column(width =6,numericInput("FBS","Fasting blood sugar level (mg/dl; 62-400):",0))),
      
      fluidRow(
        column(width =6,selectInput("HTN","Hypertension:",c("Unknown","Yes","No"),multiple = F,selected =NULL)),
        column(width =6,selectInput("DM","Diabetes Mellitus:",c("Unknown","Yes","No"),multiple = F,selected =NULL))),
      
      fluidRow(
        column(width =6,numericInput("EF.TTE","Ejection Fraction (%; 15-60):",0)),
        column(width =6,numericInput("K","Blood potassium Content (mEq/lit; 3.0-6.6):",0))), #float
      
      fluidRow(
        column(width =6,numericInput("PR","Resting heart rate (bpm; 50-110):",0)),
        column(width =6,numericInput("ESR","Erythrocyte sedimentation rate (mm/h; 1-90):",0))),
      
      fluidRow(
        column(width =6,numericInput("TG","Blood triclyceride concentration (mg/dl; 35-1050):",0)),
        column(width =6,selectInput("Tinversion","Tinversion in ECG:",c("Unknown","Yes","No"),multiple = F,selected =NULL))),
      
      fluidRow(
        column(width =6,numericInput("Lymph","Lymphocyte percent (%; 7-60):",0)),
        column(width =6,numericInput("Neut","Neutrophil percent (%; 32-89):",0))),
      
      fluidRow(
        column(width =6,selectInput("St.Depression","ST Depression in ECG:",c("Unknown","Yes","No"),multiple = F,selected =NULL)),
        column(width =6,selectInput("Dyspnea","Presence of dyspnea:",c("Unknown","Yes","No"),multiple = F,selected =NULL))),#N,Y
      
      fluidRow(
        column(width =6,selectInput("Nonanginal","Nonanginal chest pain:",c("Unknown","Yes","No"),multiple = F,selected =NULL)), # N,y
        column(width =6,selectInput("Region.RWMA2","Regional wall motion abnormality in heart region 2:",c("Unknown","Yes","No"),multiple = F,selected =NULL))),#region,RWAMA:023,
      
      fluidRow(
        column(width =6,selectInput("VHD.Mild","Mild valvular heart disease:",c("Unknown","Yes","No"),multiple = F,selected =NULL)), #
        column(width =6,numericInput("PLT","Platelet count (25-742):",0))),
      
      
      fluidRow(
        column(width =6,numericInput("BMI","Body Mass Index (18-41):",0)),
        column(width =6,numericInput("Na","Blood Sodium Content (mEq/lit; 128-156):",0))),
      
      fileInput("file1","or upload a CSV File",accept = ".csv"),
    
      actionButton("button", "Get prediction",width = "100%",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      #  submitButton("Submit",width = "100%"),
      width="100%"),
    mainPanel(
      textOutput("pred" ),
      textOutput("ou1"),
      plotOutput("plot"),
      tags$head(tags$style("#pred{color: black;
                               font-size: 20px;
                               }"
      )
      )
      # plotOutput("map")
    )
  )
)