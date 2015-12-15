########################################################
# Developed by S. Alaimo (alaimos at dmi . unict . it) #
# Released under licence GNU GENERAL PUBLIC LICENSE 3  #
# Date: 2015-06-01                                     #
########################################################

library(shiny)
source("utils.R")
source("home.R")
source("download.R")
source("help.R")
source("references.R")
source("contacts.R")

shinyUI(navbarPage("ncPred",
                   build.home.panel(),
                   build.job.panel(),
                   build.download.panel(),
                   build.help.panel(),
                   build.references.panel(),
                   build.contacts.panel(),
                   theme="bootstrap.css"
))