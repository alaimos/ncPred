########################################################
# Developed by S. Alaimo (alaimos at dmi . unict . it) #
# Released under licence GNU GENERAL PUBLIC LICENSE 3  #
# Date: 2015-06-01                                     #
########################################################

build.download.panel <- function () {
  return (tabPanel("Downloads", icon=icon("download"),
                   titlePanel("Downloads"),
                   tags$p(class="lead","From this page you can download our datasets and algorithm."),
                   fluidRow(column(12, h2("Datasets"))),
                   fluidRow(
                     column(
                       6,
                       fluidRow(
                         column(12,h3("Chen et al. (2013)"))
                       ),
                       wellPanel(
                         h4("ncRNA-Targets Matrix"),
                         downloadButton("downloadChenAOTmtx", "Download")
                       ),
                       wellPanel(
                         h4("Targets-Disease Matrix"),
                         downloadButton("downloadChenATDmtx", "Download")
                       )
                     ),
                     column(
                       6,
                       fluidRow(
                         column(12,h3("Helwak et al. (2013)"))
                       ),
                       wellPanel(
                         h4("ncRNA-Targets Matrix"),
                         downloadButton("downloadHelwakAOTmtx", "Download")
                       ),
                       wellPanel(
                         h4("Targets-Disease Matrix"),
                         downloadButton("downloadHelwakATDmtx", "Download")
                       )
                     )
                   ),
                   fluidRow(column(12, h2("Web Application"))),
                   fluidRow(column(12, wellPanel(h4(
                     "You can download this web application ",
                     a("here.", href="https://github.com/alaimos/ncPred")
                   ))))
  ))
}