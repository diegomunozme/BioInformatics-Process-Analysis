library(shiny)
library(maps)
library(ggplot2)
library(leaflet)
library(dplyr)
library(tidyverse)
library(tidygeocoder)
library(ggmap)
library(reshape2)
library(shinydashboard)
library(datasets)
library(gsheet)
library(tidyverse)
library(caTools)
library(car)
library(MASS)

#########################################
#    User Interface, Sidebar Menu     ###
#########################################
header <- dashboardHeader(title = "Decipher Bioscience")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Rescue Steps (A.1)", tabName = "Rescue", icon = icon("archive")),
    menuItem(HTML("Concordance/Repeatability<br/>(B.1+B.2)"), tabName = "Concordance", icon = icon("archive")),
    menuItem("Z Score (B.4)", tabName = "z_Score", icon = icon("dashboard"))
    
    
  ))

########################################
##  RESCUE STEP KPI's + VENN DIAGRAM  ##  
##         QUESTION 1A                ##
########################################
frow3_1 <- fluidRow(
  sidebarPanel(
    selectInput("error_mean", 
                label = "Choose rescue step",
                choices = list("Blue Line"="blue_line_mean", 
                              "Red Line" = "red_line_mean", 
                              "Yellow Line" = "yellow_line_mean"),
                selected = "blue_line_mean"),
    selectInput("venn_PN", 
                label = "Visualize overlaps in successful or failed samples",
                choices = list("Successful Sample"=1, 
                               "Failed Sample" = 0),
                selected = 0),
    submitButton("Apply", 
                 icon = icon("play"))
  ),
  valueBoxOutput("success_means"), 
  valueBoxOutput("fail_means")
)
frow3_2 <- fluidRow(
  tabsetPanel(
    tabPanel("Venn Diagram",
             plotOutput("venn_D", height = "600px")
    ))
  
)


################################
## CONCORDANCE/REPEATABILITY  ##
##     QUESTIONS  B2&B3       ##
################################
frow2_1 <- fluidRow( 
    sidebarPanel(
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = list("Concordance", 
                                 "Repeatability"),
                  selected = "Concordance"), 
      submitButton("Apply", 
                   icon = icon("play"))
  ),
   valueBoxOutput("total_Errors"),
   valueBoxOutput("concordance_Perc"))
 frow2_2 <- fluidRow( 
  #column(valueBoxOutput("concordance_Perc"))
  )

frow2 <- fluidRow(tabsetPanel(
  tabPanel("Visualizing Errors",
           plotOutput("concrep", height = "400px")
  )))


##############################
##       Z SCORE            ##
##############################
frow1_1 <-  fluidRow(
  valueBoxOutput("z_Value"))

frow1_2 <- fluidRow(
  sidebarPanel(
    textInput("textInput1", label = h3("Enter Pipeline ID"),
              value = "AC579CCFDC0"), 
    submitButton("Apply", 
                 icon = icon("play")), 
    paste("Example ID's: F981070B6D0, AC579CCFDC0,
        4E27AE3AB6F")
  )
  
)
frow1_3 <- fluidRow(
  tabsetPanel(
    tabPanel("Plot",
             plotOutput("hist", height = "400px")
    )))



###########################################
#                                         #
#       User Interface: Dashboard Body    #
#                                         #
###########################################
body <- dashboardBody(
  tabItems(
    tabItem(tabName= "z_Score",
            frow1_1, frow1_2, frow1_3),
    tabItem(tabName = "Concordance",
            frow2_1, frow2_2, frow2),
    tabItem(tabName = "Rescue",
            frow3_1, frow3_2)
  )
)


ui <- dashboardPage(header, sidebar, body, skin='blue')