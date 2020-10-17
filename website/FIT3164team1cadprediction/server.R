library(shiny)
library(Hmisc)
library
load(file = "LDAmodel.RData")
load(file = "Featuresselected.RData")

function(input, output) {
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
        pr=round(predict(lda.model,new,type="prob")$Y,2)
        cat("The case has a ",pr," chance of having Coronary Artery Disease.")
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
        pr=round(predict(lda.model,new2,type="prob")$Y,2)
        # if(pr=="N"){cat("The reault is: NORMAL")}else if (pr=="Y"){cat("The reault is: CAD")}
        cat("The case has a ",pr," chance of having Coronary Artery Disease.")
      }
      
    })
  })
  
}