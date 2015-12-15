########################################################
# Developed by S. Alaimo (alaimos at dmi . unict . it) #
# Released under licence GNU GENERAL PUBLIC LICENSE 3  #
# Date: 2015-06-01                                     #
########################################################

build.references.panel <- function () {
  
  return (tabPanel("References", icon=icon("bookmark"), 
                   titlePanel("References"),
                   fluidRow(column(12, h4("If you are using this service in your own work, please cite:"))),
                   fluidRow(
                     column(12, 
                            wellPanel(tags$ul(
                              tags$li("Alaimo S, Giugno R and Pulvirenti A (2014)", strong("ncPred: ncRNA-disease association prediction through tripartite network-based inference."), "Front. Bioeng. Biotechnol. 2:71. doi: 10.3389/fbioe.2014.00071")
                            ))
                     )
                   )
  ));
  
}