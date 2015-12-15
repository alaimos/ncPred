########################################################
# Developed by S. Alaimo (alaimos at dmi . unict . it) #
# Released under licence GNU GENERAL PUBLIC LICENSE 3  #
# Date: 2015-06-01                                     #
########################################################

library(shiny)
source("utils.R")

shinyServer(function(input, output) {
  
  mtxAOT    <- NULL
  mtxATD    <- NULL
  preds     <- NULL
  cv        <- NULL
  runNCP    <- 0
  runCV     <- 0
  
  inputData <- reactive({
    list(run=(input$runNCP[1] > 0),
         mtxAOT=input$mtxAOT,
         mtxATD=input$mtxATD,
         lambda1=input$lambda1,
         lambda2=input$lambda2,
         runNCP=input$runNCP[1],
         runCV=input$runCV[1])
  })
  
  output$results <- renderUI({
    data <- inputData()
    res  <- NULL
    if (data$run && data$runNCP > runNCP) {
      runNCP <<- data$runNCP
      cv     <<- NULL
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Please wait...", value = 0)
      mtxAOT <<- read.input.file(data$mtxAOT, "Error while reading ncRNA-Target matrix: ")
      progress$inc(1/3, detail="Step 1 of 3")
      mtxATD <<- read.input.file(data$mtxATD, "Error while reading Target-Disease matrix: ")
      progress$inc(1/3, detail="Step 2 of 3")
      preds  <<- computeRecommendation(AOT=mtxAOT, ATD=mtxATD, 
                                       lambda1=input$lambda1, 
                                       lambda2=input$lambda2)
      progress$inc(1/3, detail="Step 3 of 3")
    }
    if (data$run && data$runNCP >= runNCP) {
      res <- list(
        fluidRow(column(12, h3("Your results are ready."))),
        fluidRow(column(12, p(
          "You can download them by clicking on the button below."))),
        fluidRow(column(12, style="text-align: center", downloadButton("downloadResults", "Download Your Results"))),
        fluidRow(column(12, style="text-align: center", "")),
        fluidRow(column(12, style="text-align: center", actionButton("runCV", "Run 10-Fold Cross-Validation")))
      )      
    }
    if (is.list(res) && !is.null(data$runCV) && data$runCV > runCV) {
      runCV <<- data$runCV
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Please wait...", value = 0)
      cv <<- test.crossvalidation(mtxAOT, mtxATD,
                                  algorithm=computeRecommendation,
                                  progress=progress,
                                  lambda1=input$lambda1, lambda2=input$lambda2)
      colnames(cv) <<- c("Recovery", "Precision Enhancement", "Recall Enhancement", "Personalization", "Surprisal")
    }
    if (!all(is.null(cv))) {
      res <- c(res, list(
        fluidRow(column(12, h3("10-Fold Cross Validation Results"))),
        fluidRow(column(12, 
                        HTML("<center>"),
                        output.table(cv), 
                        HTML("</center>")))
      ))
    }
    if (is.null(res)) {
      return (list(
        fluidRow(column(12, h3("Instructions"))),
        fluidRow(column(12, p(
          strong("Step 1."),
          "Prepare your data and save them in a format compatible with our website",
          "(Text File or R Single Object file). ",
          "If you're working with R you can save your matrices in RDS format with ",
          shiny::a(href="http://stat.ethz.ch/R-manual/R-devel/library/base/html/readRDS.html", "saveRDS().")))),
        fluidRow(column(12, p(
          strong("Step 2."),
          "Upload your files."))),
        fluidRow(column(12, p(
          strong("Step 3."),
          "Select the values for parameters",
          HTML("&lambda;<sub>1</sub>"),
          "and",
          HTML("&lambda;<sub>2</sub>."),
          "In most cases the default parameters are sufficient."))),
        fluidRow(column(12, p(
          strong("Step 4."),
          "Click on \"Launch ncPred\" to start the computation.")))
      ))
    }
    return (res)
  })
  
  output$downloadResults <- downloadHandler(
    filename = function() { "results.txt" },
    content = function(file) {
      data   <- inputData()
      hasRun <- (data$run && 
                   !all(is.null(preds)) && 
                   runNCP == data$runNCP)
      if (hasRun) {
        write.table(preds, file, sep="\t", row.names=TRUE, col.names=TRUE)
      } else {
        cat("No data available!", file=file)
      }
    },contentType="text/plain")
  
  output$downloadChenAOTmtx <- downloadHandler(
    filename = function() { "chen.nctarg.matrix.txt" },
    content = function(file) {
      write.table(readRDS("data/chen.aot.rds"), file, sep="\t", row.names=TRUE, col.names=TRUE)
    },contentType="text/plain")
  
  output$downloadChenATDmtx <- downloadHandler(
    filename = function() { "chen.targdis.matrix.txt" },
    content = function(file) {
      write.table(readRDS("data/chen.atd.rds"), file, sep="\t", row.names=TRUE, col.names=TRUE)
    },contentType="text/plain")
  
  output$downloadHelwakAOTmtx <- downloadHandler(
    filename = function() { "helwak.nctarg.matrix.txt" },
    content = function(file) {
      write.table(readRDS("data/helwak.aot.rds"), file, sep="\t", row.names=TRUE, col.names=TRUE)
    },contentType="text/plain")
  
  output$downloadHelwakATDmtx <- downloadHandler(
    filename = function() { "helwak.targdis.matrix.txt" },
    content = function(file) {
      write.table(readRDS("data/helwak.atd.rds"), file, sep="\t", row.names=TRUE, col.names=TRUE)
    },contentType="text/plain")
  
})