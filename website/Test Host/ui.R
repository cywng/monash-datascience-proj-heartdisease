library(shiny)
library(Hmisc)
load(file = "LDAmodel.RData")

load(file = "caddata.RData")
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
  # titlePanel("Heart Disease Prediction"),
  br(),
  
  sidebarLayout(
    sidebarPanel (
      fluidRow(
        column(width =6 ,selectInput("Typical.Chest.Pain","Typical.Chest.Pain:",c("",0,1),multiple = F,selected =NULL)),
        column(width =6 ,numericInput("Age","Age:",0))),
      
      fluidRow(
        column(width =6, selectInput("Atypical","Atypical:",c("",0,1),multiple = F,selected =NULL)), #N,Y
        column(width =6,numericInput("FBS","FBS:",0))),
      
      fluidRow(
        column(width =6,selectInput("HTN","HTN:",c("",0,1),multiple = F,selected =NULL)),
        column(width =6,selectInput("DM","DM:",c("",0,1),multiple = F,selected =NULL))),
      
      fluidRow(
        column(width =6,numericInput("EF.TTE","EF.TTE:",0)),
        column(width =6,numericInput("K","K:",0))), #float
      
      fluidRow(
        column(width =6,numericInput("PR","PR:",0)),
        column(width =6,numericInput("ESR","ESR:",0))),
      
      fluidRow(
        column(width =6,numericInput("TG","TG:",0)),
        column(width =6,selectInput("Tinversion","Tinversion:",c("",0,1),multiple = F,selected =NULL))),
      
      fluidRow(
        column(width =6,numericInput("Lymph","Lymph:",0)),
        column(width =6,numericInput("Neut","Neut:",0))),
      fluidRow(
        column(width =6,selectInput("St.Depression","St.Depression:",c("",0,1),multiple = F,selected =NULL)),
        column(width =6,selectInput("Dyspnea","Dyspnea:",c("",0,1),multiple = F,selected =NULL))),#N,Y
      
      
      fluidRow(
        column(width =6,selectInput("Nonanginal","Nonanginal:",c("",0,1),multiple = F,selected =NULL)), # N,y
        column(width =6,selectInput("Region.RWMA2","Region.RWMA2:",c("",0,1),multiple = F,selected =NULL))),#region,RWAMA:023,
      
      
      fluidRow(
        column(width =6,selectInput("VHD.Mild","VHD.Mild:",c("",0,1),multiple = F,selected =NULL)), #
        column(width =6,numericInput("PLT","PLT:",0))),
      
      
      fluidRow(
        column(width =6,numericInput("BMI","BMI:",0)),
        column(width =6,numericInput("Na","Na:",0))),
      
      fileInput("file1","Choose a CSV File",accept = ".csv"),
      
      actionButton("button", "Get the result",width = "100%",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
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
    ))
)