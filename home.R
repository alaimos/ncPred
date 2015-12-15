########################################################
# Developed by S. Alaimo (alaimos at dmi . unict . it) #
# Released under licence GNU GENERAL PUBLIC LICENSE 3  #
# Date: 2015-06-01                                     #
########################################################

build.home.panel <- function () {
  return (tabPanel(
    "Home", 
    icon=icon("home"),
    fluidRow(column(12, wellPanel(
      p("ncPred is an algorithm for the inference of ncRNA-disease association ",
        "based on recommendation technique. We represent our knowledge through ",
        "a tripartite network, whose nodes are ncRNAs, targets, or diseases. ",
        "Interactions in such a network associate each ncRNA with a disease ",
        "through its targets. Our algorithm, starting from such a network, ",
        "computes weights between each ncRNA-disease pair using a multi-level ",
        "resource transfer technique that at each step takes into account the ",
        "resource transferred in the previous one."),
      p("Our algorithm is available for download, along with test data, by clicking",
        "on \"Download\".")
    )))                  
  ))
}

build.job.panel <- function () {
  return (tabPanel(
    "Prediction Tool",
    icon=icon("cloud"),
    titlePanel("ncRNA-disease association Prediction Tool"),
    sidebarLayout(
      
      sidebarPanel(
        style="text-align: center",
        fileInput("mtxAOT",HTML(paste0("Upload a file<sup>*</sup> containing an adjacency matrix ",
                                       "of ncRNA-target interactions:"))),                            
        fileInput("mtxATD",HTML(paste0("Upload a file<sup>*</sup> containing an adjacency matrix ",
                                       "of target-disease associations:"))),
        HTML("<center>"),
        sliderInput("lambda1", HTML("&lambda;<sub>1</sub>"), 
                    min = 0, max = 1, value = 0.5, step= 0.01),
        sliderInput("lambda2", HTML("&lambda;<sub>2</sub>"), 
                    min = 0, max = 1, value = 0.5, step= 0.01),
        HTML("</center>"),
        actionButton("runNCP", "Launch ncPred"),
        br(),
        HTML("&nbsp;"),
        br(),
        p(style="font-size: x-small; text-align: left;",
          "* You can upload either a text file or a matrix in RDS format (single R object).")
      ),
      
      mainPanel(
        uiOutput("results")#,
        #tableOutput("resultsViewer")
      )
    )
  ))
}