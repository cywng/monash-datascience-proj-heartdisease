library(shiny)
library(Hmisc)
load(file = "Models/LDAmodel.RData")
load(file = "DataWrangling/caddata.RData")
new<-data.frame("Typical.Chest.Pain"=as.numeric("1"),"Age"=60,"Atypical"=0,"FBS"=0,"HTN"=1,"DM"=1,"EF.TTE"=20, "K"=2,"PR"=2,"ESR"=1,"TG"=2,"Tinversion"=0,"Lymph"=2,"Neut"=2,"St.Depression"=1,"Dyspnea"=1,"Nonanginal"=1,"Region.RWMA2"=1,"VHD.Mild"=1,"PLT"=2,"BMI"=2,"Na"=3)

#-----------
#predict(lda.model,new)
#lda.features$optVariables
#predict(lda.model, test.df[lda.features$optVariables])
#"Typical.Chest.Pain" "Age"                "Atypical"           "FBS"                "HTN"                "DM"                
#[7] "EF.TTE"             "K"                  "PR"                 "ESR"                "TG"                 "Tinversion"        
#[13] "Lymph"              "Neut"               "St.Depression"      "Dyspnea"            "Nonanginal"         "Region.RWMA2"      
#[19] "VHD.Mild"           "PLT"                "BMI"                "Na"   
if (interactive()) {
   load(file = "DataWrangling/caddata.RData")
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
  ui<-shinyUI(fluidPage(
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
    )),
    
  ))
  server <- function(input, output) {
     observeEvent(input$button,{
     output$pred<-renderPrint({
    #    observeEvent(input$button,{
       if(is.null(input$file1)){
          
          if(input$Typical.Chest.Pain==""){Typical.Chest.Pain<-round(mean(train.df$Typical.Chest.Pain),digits=0)}
          else{Typical.Chest.Pain<-as.numeric(input$Typical.Chest.Pain)}
          

          if(input$Age==0){Age<-round(mean(train.df$Age),digits=0)}
          else{Age<-input$Age}
          
          if(input$Atypical==""){Atypical<-round(mean(train.df$Atypical),digits=0)}
          else{Atypical<-as.numeric(input$Atypical)}
          
          if(input$FBS==0){FBS<-round(mean(train.df$FBS),digits=0)}
          else{FBS<-input$FBS}
          
          if(input$HTN==""){HTN<-round(mean(train.df$HTN),digits=0)}
          else{HTN<-as.numeric(input$HTN)}
          
          if(input$DM==""){DM<-round(mean(train.df$DM),digits=0)}
          else{DM<-as.numeric(input$DM)}
          
          if(input$EF.TTE==0){EF.TTE<-round(mean(train.df$EF.TTE),digits=0)}
          else{EF.TTE<-input$EF.TTE}
          
          if(input$K==0){K<-round(mean(train.df$K),digits=0)}
          else{K<-input$K}
          
          if(input$PR==0){PR<-round(mean(train.df$PR),digits=0)}
          else{PR<-input$PR}
          
          if(input$ESR==0){ESR<-round(mean(train.df$ESR),digits=0)}
          else{ESR<-input$ESR}
          
          if(input$TG==0){TG<-round(mean(train.df$TG),digits=0)}
          else{TG<-input$TG}
          
          if(input$Tinversion==""){Tinversion<-round(mean(train.df$Tinversion),digits=0)}
          else{Tinversion<-as.numeric(input$Tinversion)}
          
          if(input$Lymph==0){Lymph<-round(mean(train.df$Lymph),digits=0)}
          else{Lymph<-input$Lymph}
          
          if(input$Neut==0){Neut<-round(mean(train.df$Neut),digits=0)}
          else{Neut<-input$Neut}
          
          if(input$St.Depression==""){St.Depression<-round(mean(train.df$St.Depression),digits=0)}
          else{St.Depression<-as.numeric(input$St.Depression)}
          
          if(input$Dyspnea==""){Dyspnea<-round(mean(train.df$Dyspnea),digits=0)}
          else{Dyspnea<-as.numeric(input$Dyspnea)}
          
          if(input$Nonanginal==""){Nonanginal<-round(mean(train.df$Nonanginal),digits=0)}
          else{Nonanginal<-as.numeric(input$Nonanginal)}
          
          if(input$Region.RWMA2==""){Region.RWMA2<-round(mean(train.df$Region.RWMA2),digits=0)}
          else{Region.RWMA2<-as.numeric(input$Region.RWMA2)}
          
          if(input$VHD.Mild==""){VHD.Mild<-round(mean(train.df$VHD.Mild),digits=0)}
          else{VHD.Mild<-as.numeric(input$VHD.Mild)}
          
          if(input$PLT==0){PLT<-round(mean(train.df$PLT),digits=0)}
          else{PLT<-input$PLT}
          
          if(input$BMI==0){BMI<-round(mean(train.df$BMI),digits=0)}
          else{BMI<-input$BMI}
          
          if(input$Na==0){Na<-round(mean(train.df$Na),digits=0)}
          else{Na<-input$Na}
          
         
          
        new<-data.frame("Typical.Chest.Pain"=Typical.Chest.Pain,"Age"=Age,
                        "Atypical"=Atypical,"FBS"=FBS,"HTN"=HTN,
                        "DM"=DM,"EF.TTE"=EF.TTE, "K"=K,"PR"=PR,"ESR"=ESR, 
                        "TG"=TG,"Tinversion"=Tinversion,"Lymph"=Lymph,"Neut"=Neut,
                        "St.Depression"=St.Depression,"Dyspnea"=Dyspnea,"Nonanginal"=Nonanginal,
                        "Region.RWMA2"=Region.RWMA2,"VHD.Mild"=VHD.Mild,"PLT"=PLT,"BMI"=BMI,"Na"=Na)
         pr=predict(lda.model,new)
         cat("The case has CAD with:",pr,type="prob")$Y
       }
        else{
           file<-input$file1
           
           ext <- tools::file_ext(file$datapath)
           req(file)
           validate(need(ext == "csv", "Please upload a csv file"))
           file<-read.csv(file$datapath)
           #_-----
           load(file = "DataWrangling/caddata.RData")
           if(is.null(file$Typical.Chest.Pain)){Typical.Chest.Pain<-round(mean(train.df$Typical.Chest.Pain),digits=0)}
           else{Typical.Chest.Pain<-file$Typical.Chest.Pain}
           
           if(is.null(file$Age)){Age<-round(mean(train.df$Age),digits=0)}
           else{Age<-file$Age}
           
           if(is.null(file$Atypical)){Atypical<-round(mean(train.df$Atypical),digits=0)}
           else{Atypical<-file$Atypical}
           
           if(is.null(file$FBS)){FBS<-round(mean(train.df$FBS),digits=0)}
           else{FBS<-file$FBS}
           
           if(is.null(file$HTN)){HTN<-round(mean(train.df$HTN),digits=0)}
           else{HTN<-file$HTN}
           
           if(is.null(file$DM)){DM<-round(mean(train.df$DM),digits=0)}
           else{DM<-file$DM}
           
           ----
           if(is.null(file$EF.TTE)){EF.TTE<-round(mean(train.df$EF.TTE),digits=0)}
           else{EF.TTE<-file$EF.TTE}
           
           if(is.null(file$K)){K<-round(mean(train.df$K),digits=0)}
           else{K<-file$K}
           
           if(is.null(file$PR)){PR<-round(mean(train.df$PR),digits=0)}
           else{PR<-file$PR}
           
           if(is.null(file$ESR)){ESR<-round(mean(train.df$ESR),digits=0)}
           else{ESR<-file$ESR}
           
           if(is.null(file$TG)){TG<-round(mean(train.df$TG),digits=0)}
           else{TG<-file$TG}
           
           if(is.null(file$Tinversion)){Tinversion<-round(mean(train.df$Tinversion),digits=0)}
           else{Tinversion<-file$Tinversion}
           
           
           if(is.null(file$Lymph)){Lymph<-round(mean(train.df$Lymph),digits=0)}
           else{Lymph<-file$Lymph}
           
           if(is.null(file$Neut)){Neut<-round(mean(train.df$Neut),digits=0)}
           else{Neut<-file$Neut}
           
           if(is.null(file$St.Depression)){St.Depression<-round(mean(train.df$St.Depression),digits=0)}
           else{St.Depression<-file$St.Depression}
           
           
           if(is.null(file$Dyspnea)){Dyspnea<-round(mean(train.df$Dyspnea),digits=0)}
           else{Dyspnea<-file$Dyspnea}
           
           if(is.null(file$Nonanginal)){Nonanginal<-round(mean(train.df$Nonanginal),digits=0)}
           else{Nonanginal<-file$Nonanginal}
           
           
           if(is.null(file$Region.RWMA2)){Region.RWMA2<-round(mean(train.df$Region.RWMA2),digits=0)}
           else{Region.RWMA2<-file$Region.RWMA2}
           
           if(is.null(file$VHD.Mild)){VHD.Mild<-round(mean(train.df$VHD.Mild),digits=0)}
           else{VHD.Mild<-file$VHD.Mild}
           
           if(is.null(file$PLT)){PLT<-round(mean(train.df$PLT),digits=0)}
           else{PLT<-file$PLT}
           
           
           if(is.null(file$BMI)){BMI<-round(mean(train.df$BMI),digits=0)}
           else{BMI<-file$BMI}
           
           if(is.null(file$Na)){Na<-round(mean(train.df$Na),digits=0)}
           else{Na<-file$Na}
          
 
           #-------
           new2<-data.frame("Typical.Chest.Pain"=Typical.Chest.Pain,"Age"=Age,
                           "Atypical"=Atypical,"FBS"=FBS,"HTN"=HTN,
                           "DM"=DM,"EF.TTE"=EF.TTE, "K"=K,"PR"=PR,"ESR"=ESR, 
                           "TG"=TG,"Tinversion"=Tinversion,"Lymph"=Lymph,"Neut"=Neut,
                           "St.Depression"=St.Depression,"Dyspnea"=Dyspnea,"Nonanginal"=Nonanginal,
                           "Region.RWMA2"=Region.RWMA2,"VHD.Mild"=VHD.Mild,"PLT"=PLT,"BMI"=BMI,"Na"=Na)
           pr=predict(lda.model,new2,type="prob")$Y
          # if(pr=="N"){cat("The reault is: NORMAL")}else if (pr=="Y"){cat("The reault is: CAD")}
           cat("The case has CAD with confidence ",pr)
        }
        
       })
     })

  }
  
  app<- shinyApp(ui, server)
  runApp(app,port=getOption("shiny.port",8080),host = getOption("shiny.host", "127.0.0.1"))
}