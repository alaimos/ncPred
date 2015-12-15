########################################################
# Developed by S. Alaimo (alaimos at dmi . unict . it) #
# Released under licence GNU GENERAL PUBLIC LICENSE 3  #
# Date: 2015-06-01                                     #
########################################################

build.single.address <- function (name, dept, uni, mail) {
  return (tags$address(
    tags$span(class="text-info", strong(name)),
    br(),
    dept,
    br(),
    uni,
    br(),
    tags$a(href=paste0("mailto:", mail), mail)
  ))
}

build.contacts.panel <- function () {
  
  return (tabPanel("Contacts", icon=icon("envelope"),
                   titlePanel("Contacts"),
                   fluidRow(column(12, "For bugs reporting, please send an email to:",
                                   tags$a(href="mailto:alaimos@dmi.unict.it", "alaimos@dmi.unict.it"),
                                   style="text-align: center")),
                   fluidRow(column(12, h3("Authors"))),
                   fluidRow(
                     column(12, 
                            build.single.address("S. Alaimo", 
                                                 "Dept. of Mathematics and Computer Science", 
                                                 "University of Catania", 
                                                 "alaimos@dmi.unict.it"),
                            build.single.address("R. Giugno", 
                                                 "Dept. of Clinical and Molecular Biomedicine", 
                                                 "University of Catania", 
                                                 "giugno@dmi.unict.it"),
                            build.single.address("A. Pulvirenti", 
                                                 "Dept. of Clinical and Molecular Biomedicine", 
                                                 "University of Catania", 
                                                 "apulvirenti@dmi.unict.it"))
                   )
  ))
  
}