#Script name: plumber_api.R

#' @apiTitle Run predictions for Coronary Artery Disease with an LDA model.
#' @apiDescription This API takes as patient data on Coronary artery disease (CAD) and returns a prediction whether the input values
#' indicate CAD (Y) or no CAD (N).
#' This was based on the web post https://www.shirin-glander.de/2018/01/plumber/.
#' This has been built as part of a final year project at monash university, with repo being available here: 
#' https://github.com/cywng/monash-datascience-proj-heartdisease.

# load model
load(file="../Models/LDAmodel.RData")

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

#' predict Coronary Artery Disease with an LDA model.
#' @param Typical.Chest.Pain:numeric Presence of chest pain (1 for yes, 0 for no).
#' @param Age:numeric The age of the patient.
#' @param Atypical:int Atypical pulse (1 for yes, 0 for no).
#' @param FBS:numeric Fasting blood sugar level (mg/dl, 62-400).
#' @param HTN:int Presence of hypertension (1 for yes, 0 for no).
#' @param DM:int Presence of diabetes mellitus (1 for yes, 0 for no).
#' @param EF.TTE:numeric Level of Ejection Fraction (15-60).
#' @param K:numeric Potassium content in blood (mEq/lit; 3.0-6.6).
#' @param PR:numeric Resting heart rate (bpm; 50-110).
#' @param ESR:numeric Erythrocyte sedimentation rate (mm/h; 1-90).
#' @param TG:numeric Triclyceride concentration in blood (mg/dl; 37-1050).
#' @param Tinversion:int Presence of Tinversion in ECG reading (1 for yes, 0 for no).
#' @param Lymph:numeric Lymphocyte % (7-60).
#' @param Neut:numeric Neutrophil % (32-89).
#' @param St.Depression:int Presence of ST Depression in ECG (1 for yes, 0 for no).
#' @param Dyspnea:int Presence of dyspnea (1 for yes, 0 for no).
#' @param Nonanginal:int Presence of nonanginal chest pain (1 for yes, 0 for no).
#' @param Region.RWMA2:int If the second region of the heart has regional wall motion abnormality (1 for yes, 0 for no).
#' @param VHD.Mild:int If they have 'mild' valvular heart disease (1 for yes, 0 for no).
#' @param PLT:numeric Platelet count (1000/ml; 25-742).
#' @param BMI:numeric Body Mass Index (18-41).
#' @param Na:numeric Sodium content in blood (mEQ/lit; 128-156).

#RWMA 1 not 2, BP.
#' @get /predict
#' @html
#' @response 200 Returns the class (Y or N) prediction from the LDA model; Y = Coronary Artery Disease
calculate_prediction <- function(Typical.Chest.Pain, Age, Atypical, FBS, HTN, DM, EF.TTE, K, PR, ESR, TG, Tinversion, Lymph, Neut, 
                                 St.Depression, Dyspnea, Nonanginal, Region.RWMA2, VHD.Mild, PLT, BMI, Na) {
  
  # make data frame from parameters
  input_data <<- data.frame(Typical.Chest.Pain, Age, Atypical, FBS, HTN, DM, EF.TTE, K, PR, ESR, TG, Tinversion, Lymph, Neut, 
                            St.Depression, Dyspnea, Nonanginal, Region.RWMA2, VHD.Mild, PLT, BMI, Na, stringsAsFactors = FALSE)
  # and make sure they really are numeric
  input_data <<- as.data.frame(t(sapply(input_data, as.numeric)))
  
  # validation for parameter
  if (any(is.na(input_data))) {
    res$status <- 400
    res$body <- "Parameters have to be numeric or integers"
  }

  #if (any(input_data < 0) || any(input_data > 1)) {
  #  res$status <- 400
  #  res$body <- "Parameters have to be between 0 and 1"
  #}

  # predict and return result
  pred_rf <<- predict(lda.model, input_data, type="prob")$Y
  paste("----------------\nThe case has a ",as.character(pred_rf)," chance of having Coronary Artery Disease.\n----------------\n")
}