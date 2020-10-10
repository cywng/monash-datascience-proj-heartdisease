#Script name: plumber_api.R

#' @apiTitle Run predictions for Coronary Artery Disease with an LDA model
#' @apiDescription This API takes as patient data on Coronary artery disease (CAD) and returns a prediction whether the input values
#' indicate CAD (Y) or no CAD (N).
#' This was based on the web post https://www.shirin-glander.de/2018/01/plumber/
#' This has been built as part of a final year project at monash university, with repo being available 
#' https://github.com/cywng/monash-datascience-proj-heartdisease

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

#' predict Chronic Kidney Disease of test case with Random Forest model
#' @param Typical.Chest.Pain:numeric Whether or not the patient reports chest pain (1 for yes, 0 for no)
#' @param Age:numeric The age of the patient
#' @param Atypical:int
#' @param FBS:numeric
#' @param HTN:int
#' @param DM:int
#' @param EF.TTE:numeric
#' @param K:numeric
#' @param PR:numeric
#' @param ESR:numeric
#' @param TG:numeric
#' @param Tinversion:int
#' @param Lymph:numeric
#' @param Neut:numeric
#' @param St.Depression:int
#' @param Dyspnea:int
#' @param Nonanginal:int
#' @param Region.RWMA2:int
#' @param VHD.Mild:int
#' @param PLT:numeric
#' @param BMI:numeric
#' @param Na:numeric

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
  pred_rf <<- predict(lda.model, input_data)
  paste("----------------\nTest case predicted to be", as.character(pred_rf), "\n----------------\n")
}