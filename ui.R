# This is the interface CSAR
# Corn Stunt Analysis RAting
# Chris Simoes
# chris.simoes@syngenta.com


library(shinyjs)
library(DT)
library(shinydashboard)
library(dplyr)
library(ggplot2)


# This is the interface CSAR
# Corn Stunt Analysis RAting
# Chris Simoes
# chris.simoes@syngenta.com


ui <- dashboardPage(
  dashboardHeader(title="Reporte Semanal", titleWidth=230),
  dashboardSidebar(
    sidebarMenu(
      p("Por favor, selecione as opções abaixo:"),
      selectInput("", "TPP",
                  c("","TPP09","TPP10","TPP11","TPP12")
                  ),
      selectInput("", "PLC",
                  c("",4,5,6)
      )
    )
  ),
  dashboardBody(
    fluidPage(
      tabsetPanel(type = "tabs",
                  tabPanel("resumoHB",
                           mainPanel(
                             h3("Resumo dos Híbridos"),
                             column(width = 4, 
                                    fluidRow(valueBoxOutput("locN", width = 12))),
                             column(width = 4, 
                                    fluidRow(valueBoxOutput("trialN", width = 12))),
                             column(width = 4, 
                                    fluidRow(valueBoxOutput("datapN", width = 12))),
                             selectInput("select", h3("Select box"), 
                                         choices = list("Choice 1" = 1, "Choice 2" = 2,
                                                        "Choice 3" = 3), selected = 1),
                             br(),
                             h3("Enfezamento"),
                             br(),
                             column(width = 3, 
                                    fluidRow(valueBoxOutput("locTPP09", width = 12))),
                             column(width = 3, 
                                    fluidRow(valueBoxOutput("locTPP10", width = 12))),
                             
                             column(width = 3, 
                                    fluidRow(valueBoxOutput("locTPP11", width = 12))),
                             column(width = 3, 
                                    fluidRow(valueBoxOutput("locTPP12", width = 12))),
                             br(),
                             column(width = 6, 
                                    fluidRow(valueBoxOutput("jointLoc", width = 12))),
                             column(width = 6,
                                    selectInput("", "Selecione o Hibrido",
                                                 c("",4,5,6)),
                                    fluidRow(valueBoxOutput("compHB", width = 12))),
                             br(),
                             
                             column(width = 3,
                                    h4("Mancha Branca - LFSTR"),
                                    fluidRow(valueBoxOutput("lfstr", width = 12))),
                            
                             column(width = 3,
                                    h4("Cercospora - GRLSR"),
                                    fluidRow(valueBoxOutput("grlsr", width = 12))),
                             column(width = 3,
                                    h4("Turcicum - HELMR"),
                                    fluidRow(valueBoxOutput("helmr", width = 12))),
                             br(),
                             
                             column(width = 3, 
                                    fluidRow( h4("Bipolaris - SCLBR"),
                                              valueBoxOutput("sclbr", width = 12))),
                             column(width = 3, 
                                    fluidRow( h4("Ferrugem - SRSTR"),
                                              valueBoxOutput("srstr", width = 12))),
                             column(width = 3, 
                                    fluidRow( h4("Diplodia - DLLFR"),
                                              valueBoxOutput("dllfr", width = 12))),
                             
                           ))
                  )
      )
  )
)
