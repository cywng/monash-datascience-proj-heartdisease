library(shiny)
if (interactive()) {
  ui<-shinyUI(fluidPage(
    h1("Heart Disease Prediction",align = "center"),#titlePanel
    br(),
    numericInput("age","Age:",1),
    textInput("sex","Sex:"),
    textInput("lymph","Lymph:"),
    verbatimTextOutput("number"),
    fileInput("file1", "Choose CSV File", accept = ".csv"),
    submitButton("Submit"),
    
    verbatimTextOutput("value"),
    
  ))
  server <- function(input, output) {
    output$contents <- renderTable({
      file <- input$file1
      #ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      
      read.csv(file$datapath, header = input$header)
    })
  }
  
  app<- shinyApp(ui, server)
  runApp(app,port=getOption("shiny.port",8080),host = getOption("shiny.host", "127.0.0.1"))
}