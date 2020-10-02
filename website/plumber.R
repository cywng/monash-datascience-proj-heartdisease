library(plumber)
r <- plumb(function(){
  cat("System time:", as.character(Sys.time()), "\n",
      "Request method:", req$REQUEST_METHOD, req$PATH_INFO, "\n",
      "HTTP user agent:", req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}
calculate_prediction <- function(Age, BMI) { #...
  input_data_num <<- data.frame(Age, BMI,#...
                                stringsAsFactors = FALSE)
  input_data_num <<- as.data.frame(t(sapply(input_data_num, as.numeric)))
  input_data_int <<- data.frame(Age, BM,
                                stringsAsFactors = FALSE)
  input_data_int <<- as.data.frame(t(sapply(input_data_int, as.integer)))
  input_data <<- as.data.frame(cbind(input_data_num, input_data_int))
  if (any(is.na(input_data))) {
    res$status <- 400
    res$body <- "Parameters have to be numeric or integers"
  }
  
  if (any(input_data < 0) || any(input_data > 1)) {
    res$status <- 400
    res$body <- "Parameters have to be between 0 and 1"
  }
  pred_rf <<- predict(NB_model, input_data)
  paste("----------------\nTest case predicted to be", as.character(pred_rf), "\n----------------\n")
}
)
r$run(port = 8080)
