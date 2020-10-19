library(shiny)
library(Hmisc)
library
load(file = "svm_linear.RData")
load(file = "svm_train_data.Rdata")



#to test changes locally set wd to folder containing server.R and ui.R: library(shiny); runapp()
#to push changes: library(rsconnect); deployApp()

function(input, output) {
  observeEvent(input$button,{
    output$pred<-renderPrint({
      #If no input file
      if(is.null(input$file1)){
        
        #Set values to train data averages if no input.
        if(input$Typical.Chest.Pain=="Unknown"){Typical.Chest.Pain<-as.factor(round(mean(as.numeric(svm.train$Typical.Chest.Pain)-1),digits=0))}
        else{Typical.Chest.Pain<-as.factor(as.numeric(ifelse(input$Typical.Chest.Pain=="Yes",1,0)))}#
        
        
        if(input$Age==0 || is.na(input$Age)){Age<-round(mean(svm.train$Age),digits=0)}
        else{Age<-input$Age}#
        
        if(input$Atypical=="Unknown"){Atypical<-"N"}
        else{Atypical<-as.factor( as.numeric(ifelse(input$Atypical=="Yes",1,0)))} #
        
        if(input$FBS==0 || is.na(input$FBS)){FBS<-round(mean(svm.train$FBS),digits=0)}
        else{FBS<-input$FBS} #
        
        if(input$HTN=="Unknown"){HTN<-as.factor(round(mean(as.numeric(svm.train$HTN)-1),digits=0))}
        else{HTN<-as.factor(as.numeric(ifelse(input$HTN=="Yes",1,0)))} #
        
        if(input$DM=="Unknown"){DM<-as.factor(round(mean(as.numeric(svm.train$DM)-1),digits=0))}
        else{DM<-as.factor(as.numeric(ifelse(input$DM=="Yes",1,0)))} #
        
        if(input$EF.TTE==0 || is.na(input$EF.TTE)){EF.TTE<-round(mean(svm.train$EF.TTE),digits=0)}
        else{EF.TTE<-input$EF.TTE} #
        
        if(input$K==0 || is.na(input$K)){K<-round(mean(svm.train$K),digits=0)}
        else{K<-input$K} #
        
        if(input$BP==0 || is.na(input$BP)){BP<-round(mean(svm.train$BP),digits=0)}
        else{BP<-input$BP}  #
        
        if(input$ESR==0 || is.na(input$ESR)){ESR<-round(mean(svm.train$ESR),digits=0)}
        else{ESR<-input$ESR}  #
        
        if(input$TG==0 || is.na(input$TG)){TG<-round(mean(svm.train$TG),digits=0)}
        else{TG<-input$TG} #
        
        if(input$Region.RWMA=="Unknown"){Region.RWMA<-as.factor(round(mean(as.numeric(svm.train$Region.RWMA)-1),digits=0))}
        else{Region.RWMA<-as.factor(input$Region.RWMA)}
        

        new<-data.frame("Typical.Chest.Pain"=as.factor(Typical.Chest.Pain),"Age"=Age,
                        "Atypical"=as.factor(Atypical),"FBS"=FBS,"HTN"=as.factor(HTN),
                        "DM"=as.factor(DM),"EF.TTE"=EF.TTE, "K"=K,"BP"=BP,"ESR"=ESR, 
                        "TG"=TG,"Region.RWMA"=as.factor(Region.RWMA))
        pr=round(predict(svm.l,new[,!names(new) %in% c("Cath")],type = "prob")$Cad,2)
        #predict(svm.l,svm.test[,!names(svm.test) %in% c("Cath")],type = "prob")$Cad
        #pr=round(predict(svm.l,new,type="prob")$Y,2)
        cat("The case has a ",pr," chance of having Coronary Artery Disease.")
        #paste0(pr)

        
        
      }
      #This is for if there is an input file. Check for no entry with is.na()
      else{
        file<-input$file1
        
        ext <- tools::file_ext(file$datapath)
        req(file)
        validate(need(ext == "csv", "Please upload a csv file"))
        file<-read.csv(file$datapath)
        
        if(is.na(file$Typical.Chest.Pain) || is.null(file$Typical.Chest.Pain) ){Typical.Chest.Pain<-as.factor(round(mean(as.numeric(svm.train$Typical.Chest.Pain)-1),digits=0))}
        else{as.factor(Typical.Chest.Pain<-file$Typical.Chest.Pain)}#
        
        if(is.na(file$Age) || is.null(file$Age) ){Age<-round(mean(svm.train$Age),digits=0)}
        else{Age<-file$Age} #
        
        if(is.na(file$Atypical) || is.null(file$Atypical) ){Atypical<-"N"}
        else{Atypical<-as.factor(file$Atypical)} #
        
        if(is.na(file$FBS) || is.null(file$FBS) ){FBS<-round(mean(svm.train$FBS),digits=0)}
        else{FBS<-file$FBS} #
        
        if(is.na(file$HTN) || is.null(file$HTN) ){HTN<-as.factor(round(mean(as.numeric(svm.train$HTN)-1),digits=0))}
        else{HTN<-as.factor(as.numeric(file$HTN))} #
        
        if(is.na(file$DM)  || is.null(file$DM)){DM<-as.factor(round(mean(as.numeric(svm.train$DM)-1),digits=0))}
        else{DM<-as.factor(as.numeric(file$DM))} #
        
        if(is.na(file$EF.TTE) || is.null(file$EF.TTE)){EF.TTE<-round(mean(svm.train$EF.TTE),digits=0)}
        else{EF.TTE<-file$EF.TTE}#
        
        if(is.na(file$K)|| is.null(file$K)){K<-round(mean(svm.train$K),digits=0)}
        else{K<-file$K} #
        
        if(is.na(file$BP) || is.null(file$BP)){BP<-round(mean(svm.train$BP),digits=0)}
        else{BP<-file$BP} #
        
        if(is.na(file$ESR) || is.null(file$ESR)){ESR<-round(mean(svm.train$ESR),digits=0)}
        else{ESR<-file$ESR}  #
        
        if(is.na(file$TG)|| is.null(file$TG)){TG<-round(mean(svm.train$TG),digits=0)}
        else{TG<-file$TG} #
        
        
        if(is.na(file$Region.RWMA)|| is.null(file$Region.RWMA)){Region.RWMA<-as.factor(round(mean(as.numeric(svm.train$Region.RWMA)-1),digits=0))}
        else{Region.RWMA<-as.factor(file$Region.RWMA)}
        
        
        
        #-------
        new2<-data.frame("Typical.Chest.Pain"=as.factor(Typical.Chest.Pain),"Age"=Age,
                        "Atypical"=as.factor(Atypical),"FBS"=FBS,"HTN"=as.factor(HTN),
                        "DM"=as.factor(DM),"EF.TTE"=EF.TTE, "K"=K,"BP"=BP,"ESR"=ESR, 
                        "TG"=TG,"Region.RWMA"=as.factor(Region.RWMA))
        pr=round(predict(svm.l,new2[,!names(new2) %in% c("Cath")],type = "prob")$Cad,2)
      #  pr=round(predict(svm.l,new2,type="prob")$Y,2)
        # if(pr=="N"){cat("The reault is: NORMAL")}else if (pr=="Yes"){cat("The reault is: CAD")}
        cat("The case has a ",pr," chance of having Coronary Artery Disease.")
        #paste0(pr)
      }
      
    })
  })
  
}