#Script name: plumber_api.R

#' @apiTitle Run predictions for Coronary Artery Disease with a SVM model.
#' @apiDescription This API takes as patient data on Coronary artery disease (CAD) and returns the probability of having CAD (between 0 and 1).
#' This was based on the web post https://www.shirin-glander.de/2018/01/plumber/.
#' This has been built as part of a final year project at Monash university, with repo being available here: 
#' https://github.com/cywng/monash-datascience-proj-heartdisease.

# load model
load(file="../Models/svm_linear.RData")

#' Log system time, request method and HTTP user agent of the incoming request
#' @filter logger
function(req){
  cat("System time:", as.character(Sys.time()), "\n",
      "Request method:", req$REQUEST_METHOD, req$PATH_INFO, "\n",
      "HTTP user agent:", req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}

# core function follows below:
# define parameters with type and description
# name endpoint
# return output as html/text
# specify 200 (okay) return

#' predict Coronary Artery Disease with an SVM model.
#' @param Typical.Chest.Pain:numeric Presence of chest pain (1 for yes, 0 for no).
#' @param Age:numeric The age of the patient.
#' @param Atypical:int Atypical pulse (1 for yes, 0 for no).
#' @param FBS:numeric Fasting blood sugar level (mg/dl, 62-400).
#' @param HTN:int Presence of hypertension (1 for yes, 0 for no).
#' @param DM:int Presence of diabetes mellitus (1 for yes, 0 for no).
#' @param EF.TTE:numeric Level of Ejection Fraction (15-60).
#' @param K:numeric Potassium content in blood (mEq/lit; 3.0-6.6).
#' @param ESR:numeric Erythrocyte sedimentation rate (mm/h; 1-90).
#' @param TG:numeric Triclyceride concentration in blood (mg/dl; 37-1050).
#' @param Region.RWMA:int If any region of the heart has regional wall motion abnormality (0-4, 0 for none).
#' @param BP:numeric Blood Pressure (mmHgl 90-190).

#' @get /predict
#' @html
#' @response 200 Returns the class (Y or N) prediction from the LDA model; Y = Coronary Artery Disease
calculate_prediction <- function(Typical.Chest.Pain, Age, Atypical, FBS, HTN, DM, EF.TTE, K, ESR, TG, Region.RWMA, BP) {
  Atypical <- ifelse(Atypical==1,"Y","N")
  
  # make data frame from parameters
  input_data_num <<- data.frame(Age, FBS, EF.TTE, K, ESR, TG, BP, stringsAsFactors = FALSE)
  # and make sure they really are numeric
  input_data_num <<- as.data.frame(t(sapply(input_data_num, as.numeric)))
  
  # make data frame from parameters
  input_data_fac <<- data.frame(Typical.Chest.Pain, Atypical, HTN, DM, Region.RWMA)#, stringsAsFactors = FALSE)
  # convert to factor
  input_data_fac <<- as.data.frame(t(lapply(input_data_fac, as.factor)))
  #Combine into one df
  input_data <<- as.data.frame(c(input_data_num, input_data_fac))
  paste(input_data)
  # validation for parameter
  if (any(is.na(input_data))) {
    res$status <- 400
    res$body <- "Parameters have to be numeric or integers"
  }

  # predict and return result
  pred_svm <- round(predict(svm.l,input_data,type = "prob")$Cad,2)
  
  paste("----------------\nThe case has a ",as.character(pred_svm)," chance of having Coronary Artery Disease.\n----------------\n")
}