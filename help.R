########################################################
# Developed by S. Alaimo (alaimos at dmi . unict . it) #
# Released under licence GNU GENERAL PUBLIC LICENSE 3  #
# Date: 2015-06-01                                     #
########################################################

build.help.panel <- function () {
  
  return (tabPanel("Help", icon=icon("question-circle"),
                   titlePanel("Help"),
                   navlistPanel(
                     tabPanel("Why?", h3("Why?")),
                     tabPanel("Definitions", h3("Definitions")),
                     tabPanel("Usage", h3("Usage"))
                   )
  ))
  
}