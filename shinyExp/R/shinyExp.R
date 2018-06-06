#include libraries

library(shinyWidgets)

library(formatR)
library(miniUI)
library(rstudioapi)
library(shiny)

#display line number



shinyExp <- function() {
  
  library(shiny)
  library("devtools")
  #setwd("~/R/")
  #library(RDataTracker)
  
  source("./RDataTracker_development/R/RDataTracker.R")
  source("./RDataTracker_development/R/DDGHash.R")
  source("./RDataTracker_development/R/DDGStatement.R")
  source("./RDataTracker_development/R/OutputJSON.R")
  source("./debugR/retriveValue1.R")
 
  # Define UI for data upload app ----
  ui <- fluidPage(
    
    # App title ----
    titlePanel("Uploading Files"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        # Input: Select a file ----
        fileInput("file1", "Choose R File",
                  multiple = TRUE,
                  accept = c(
                    ".R", 
                    ".Rmd")),
        
        
        
        # Horizontal line ----
        tags$hr(),
        
        #variable selection
        selectInput("variable", "Variable:",
                    c("Default" = "def"))
       
        
        
        
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        
        # Output: Data file ----
        #tableOutput("contents")
        htmlOutput("contents"),
        
        # Horizontal line ----
        tags$hr(),
        htmlOutput("text")
        
      )
      
    )
  )
  
  # Define server logic to read selected file ----
  server <- function(input, output, session) {
    
    output$contents <- renderUI({
    #output$contents <- renderText({ "foo" })
    #visFun()
    
      #print(input$file1)
      
    
      if (is.null(input$file1))
        return("")
    
      inFile <- file(input$file1$datapath, encoding = "UTF-8")
      readLin <<- readLines(inFile)
      
      #print(getVarSet("x",input$file1$datapath))
      
      return(HTML(paste(readLines(inFile), collapse='<br/>')))
      #ddg.run(input$file1$datapath)
      #on.exit(close(inFile))
      #debugging window and click on the x change value on line 
      #go to that line
    
    })
    
    
    
    output$text <- renderUI({
      
      if (is.null(input$file1))
        return("")
      
      inFile <- file(input$file1$datapath, encoding = "UTF-8")
      readLin <<- readLines(inFile)
      
      ddg.run(input$file1$datapath)
      #get the variable names
      datalist <<- .ddg.get("ddg.data.nodes")
      
      #print("inputvar:")
      #print(input$variable)
      
      if (is.null(input$variable) || identical(input$variable, "def") || identical(input$variable, ""))
        return("")
      #get the history of user in put variable
      varname = input$variable
      historyT = getVarSet(varname,input$file1$datapath)
      tmp <- sapply(historyT, as.character)
      print("return")
      print(getVarSet(varname,input$file1$datapath))
      on.exit(close(inFile))
      #if(is.null(datalist))
        #return("")
      #(paste(readLines(inFile), collapse='<br/>'))
      return (HTML(tmp))
      
    })
    
    observe({
    
      if (is.null(input$file1))
        return("")
    
      inFile <- file(input$file1$datapath, encoding = "UTF-8")
      readLin <<- readLines(inFile)
    
      ddg.run(input$file1$datapath)
      #get the variable names
      datalist <<- .ddg.get("ddg.data.nodes")
      if(is.null(datalist))
        return("")
      namedvars = unique(datalist$ddg.name)
      names(namedvars) <- namedvars
      # Can also set the label and select items
      updateSelectInput(session, "variable",
                      label = "Select input variable: ",
                      choices = namedvars,
                      selected = tail(namedvars, 1)
      )
      
    })
    
    
    
  }
  
  viewer <- dialogViewer("Check Variable History", width = 1000, height = 800)
  runGadget(ui, server, viewer = viewer)
  
}
