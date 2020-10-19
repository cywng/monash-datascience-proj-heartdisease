library(shiny)
library(Hmisc)
library
load(file = "LDAmodel.RData")
load(file = "Featuresselected.RData")

#to test changes locally set wd to folder containing server.R and ui.R: library(shiny); runapp()
#to push changes: library(rsconnect); deployApp()

function(input, output) {
  observeEvent(input$button,{
    output$pred<-renderPrint({
      #If no input file
      if(is.null(input$file1)){
        
        #Set values to train data averages if no input.
        if(input$Typical.Chest.Pain=="Unknown"){Typical.Chest.Pain<-round(mean(train.df$Typical.Chest.Pain),digits=0)}
        else{Typical.Chest.Pain<-as.numeric(ifelse(input$Typical.Chest.Pain=="Yes",1,0))}
        
        
        if(input$Age==0 || is.na(input$Age)){Age<-round(mean(train.df$Age),digits=0)}
        else{Age<-input$Age}
        
        if(input$Atypical=="Unknown"){Atypical<-round(mean(train.df$Atypical),digits=0)}
        else{Atypical<-as.numeric(ifelse(input$Atypical=="Yes",1,0))}
        
        if(input$FBS==0 || is.na(input$FBS)){FBS<-round(mean(train.df$FBS),digits=0)}
        else{FBS<-input$FBS}
        
        if(input$HTN=="Unknown"){HTN<-round(mean(train.df$HTN),digits=0)}
        else{HTN<-as.numeric(ifelse(input$HTN=="Yes",1,0))}
        
        if(input$DM=="Unknown"){DM<-round(mean(train.df$DM),digits=0)}
        else{DM<-as.numeric(ifelse(input$DM=="Yes",1,0))}
        
        if(input$EF.TTE==0 || is.na(input$EF.TTE)){EF.TTE<-round(mean(train.df$EF.TTE),digits=0)}
        else{EF.TTE<-input$EF.TTE}
        
        if(input$K==0 || is.na(input$K)){K<-round(mean(train.df$K),digits=0)}
        else{K<-input$K}
        
        if(input$PR==0 || is.na(input$PR)){PR<-round(mean(train.df$PR),digits=0)}
        else{PR<-input$PR}
        
        if(input$ESR==0 || is.na(input$ESR)){ESR<-round(mean(train.df$ESR),digits=0)}
        else{ESR<-input$ESR}
        
        if(input$TG==0 || is.na(input$TG)){TG<-round(mean(train.df$TG),digits=0)}
        else{TG<-input$TG}
        
        if(input$Tinversion=="Unknown"){Tinversion<-round(mean(train.df$Tinversion),digits=0)}
        else{Tinversion<-as.numeric(ifelse(input$Tinversion=="Yes",1,0))}
        
        if(input$Lymph==0 || is.na(input$Lymph)){Lymph<-round(mean(train.df$Lymph),digits=0)}
        else{Lymph<-input$Lymph}
        
        if(input$Neut==0 || is.na(input$Neut)){Neut<-round(mean(train.df$Neut),digits=0)}
        else{Neut<-input$Neut}
        
        if(input$St.Depression=="Unknown"){St.Depression<-round(mean(train.df$St.Depression),digits=0)}
        else{St.Depression<-as.numeric(ifelse(input$St.Depression=="Yes",1,0))}
        
        if(input$Dyspnea=="Unknown"){Dyspnea<-round(mean(train.df$Dyspnea),digits=0)}
        else{Dyspnea<-as.numeric(ifelse(input$Dyspnea=="Yes",1,0))}
        
        if(input$Nonanginal=="Unknown"){Nonanginal<-round(mean(train.df$Nonanginal),digits=0)}
        else{Nonanginal<-as.numeric(ifelse(input$Nonanginal=="Yes",1,0))}
        
        if(input$Region.RWMA2=="Unknown"){Region.RWMA2<-round(mean(train.df$Region.RWMA2),digits=0)}
        else{Region.RWMA2<-as.numeric(ifelse(input$Region.RWMA2=="Yes",1,0))}
        
        if(input$VHD.Mild=="Unknown"){VHD.Mild<-round(mean(train.df$VHD.Mild),digits=0)}
        else{VHD.Mild<-as.numeric(ifelse(input$VHD.Mild=="Yes",1,0))}
        
        if(input$PLT==0 || is.na(input$PLT)){PLT<-round(mean(train.df$PLT),digits=0)}
        else{PLT<-input$PLT}
        
        if(input$BMI==0 || is.na(input$BMI)){BMI<-round(mean(train.df$BMI),digits=0)}
        else{BMI<-input$BMI}
        
        if(input$Na==0 || is.na(input$Na)){Na<-round(mean(train.df$Na),digits=0)}
        else{Na<-input$Na}

        new<-data.frame("Typical.Chest.Pain"=Typical.Chest.Pain,"Age"=Age,
                        "Atypical"=Atypical,"FBS"=FBS,"HTN"=HTN,
                        "DM"=DM,"EF.TTE"=EF.TTE, "K"=K,"PR"=PR,"ESR"=ESR, 
                        "TG"=TG,"Tinversion"=Tinversion,"Lymph"=Lymph,"Neut"=Neut,
                        "St.Depression"=St.Depression,"Dyspnea"=Dyspnea,"Nonanginal"=Nonanginal,
                        "Region.RWMA2"=Region.RWMA2,"VHD.Mild"=VHD.Mild,"PLT"=PLT,"BMI"=BMI,"Na"=Na)

        pr=round(predict(lda.model,new,type="prob")$Y,2)
        cat("The case has a ",pr," chance of having Coronary Artery Disease.")

        
        
      }
      #This is for if there is an input file. Check for no entry with is.na()
      else{
        file<-input$file1
        
        ext <- tools::file_ext(file$datapath)
        req(file)
        validate(need(ext == "csv", "Please upload a csv file"))
        file<-read.csv(file$datapath)
        
        if(is.na(file$Typical.Chest.Pain)){Typical.Chest.Pain<-round(mean(train.df$Typical.Chest.Pain),digits=0)}
        else{Typical.Chest.Pain<-file$Typical.Chest.Pain}
        
        if(is.na(file$Age)){Age<-round(mean(train.df$Age),digits=0)}
        else{Age<-file$Age}
        
        if(is.na(file$Atypical)){Atypical<-round(mean(train.df$Atypical),digits=0)}
        else{Atypical<-file$Atypical}
        
        if(is.na(file$FBS)){FBS<-round(mean(train.df$FBS),digits=0)}
        else{FBS<-file$FBS}
        
        if(is.na(file$HTN)){HTN<-round(mean(train.df$HTN),digits=0)}
        else{HTN<-file$HTN}
        
        if(is.na(file$DM)){DM<-round(mean(train.df$DM),digits=0)}
        else{DM<-file$DM}
        
        if(is.na(file$EF.TTE)){EF.TTE<-round(mean(train.df$EF.TTE),digits=0)}
        else{EF.TTE<-file$EF.TTE}
        
        if(is.na(file$K)){K<-round(mean(train.df$K),digits=0)}
        else{K<-file$K}
        
        if(is.na(file$PR)){PR<-round(mean(train.df$PR),digits=0)}
        else{PR<-file$PR}
        
        if(is.na(file$ESR)){ESR<-round(mean(train.df$ESR),digits=0)}
        else{ESR<-file$ESR}
        
        if(is.na(file$TG)){TG<-round(mean(train.df$TG),digits=0)}
        else{TG<-file$TG}
        
        if(is.na(file$Tinversion)){Tinversion<-round(mean(train.df$Tinversion),digits=0)}
        else{Tinversion<-file$Tinversion}
        
        
        if(is.na(file$Lymph)){Lymph<-round(mean(train.df$Lymph),digits=0)}
        else{Lymph<-file$Lymph}
        
        if(is.na(file$Neut)){Neut<-round(mean(train.df$Neut),digits=0)}
        else{Neut<-file$Neut}
        
        if(is.null(file$St.Depression)){St.Depression<-round(mean(train.df$St.Depression),digits=0)}
        else{St.Depression<-file$St.Depression}
        
        
        if(is.na(file$Dyspnea)){Dyspnea<-round(mean(train.df$Dyspnea),digits=0)}
        else{Dyspnea<-file$Dyspnea}
        
        if(is.na(file$Nonanginal)){Nonanginal<-round(mean(train.df$Nonanginal),digits=0)}
        else{Nonanginal<-file$Nonanginal}
        
        
        if(is.na(file$Region.RWMA2)){Region.RWMA2<-round(mean(train.df$Region.RWMA2),digits=0)}
        else{Region.RWMA2<-file$Region.RWMA2}
        
        if(is.na(file$VHD.Mild)){VHD.Mild<-round(mean(train.df$VHD.Mild),digits=0)}
        else{VHD.Mild<-file$VHD.Mild}
        
        if(is.na(file$PLT)){PLT<-round(mean(train.df$PLT),digits=0)}
        else{PLT<-file$PLT}
        
        
        if(is.na(file$BMI)){BMI<-round(mean(train.df$BMI),digits=0)}
        else{BMI<-file$BMI}
        
        if(is.na(file$Na)){Na<-round(mean(train.df$Na),digits=0)}
        else{Na<-file$Na}
        
        
        #-------
        new2<-data.frame("Typical.Chest.Pain"=Typical.Chest.Pain,"Age"=Age,
                         "Atypical"=Atypical,"FBS"=FBS,"HTN"=HTN,
                         "DM"=DM,"EF.TTE"=EF.TTE, "K"=K,"PR"=PR,"ESR"=ESR, 
                         "TG"=TG,"Tinversion"=Tinversion,"Lymph"=Lymph,"Neut"=Neut,
                         "St.Depression"=St.Depression,"Dyspnea"=Dyspnea,"Nonanginal"=Nonanginal,
                         "Region.RWMA2"=Region.RWMA2,"VHD.Mild"=VHD.Mild,"PLT"=PLT,"BMI"=BMI,"Na"=Na)
        pr=round(predict(lda.model,new2,type="prob")$Y,2)
        # if(pr=="N"){cat("The reault is: NORMAL")}else if (pr=="Yes"){cat("The reault is: CAD")}
        cat("The case has a ",pr," chance of having Coronary Artery Disease.")
      }
      
    })
  })
  
}