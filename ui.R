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

source("code/support.R")
source("code/graficos.R")

carga()# Carregando os dados

ui <- dashboardPage(
  dashboardHeader(title="Reporte Semanal", titleWidth=230),
  dashboardSidebar(
    sidebarMenu(
      p("Por favor, selecione as opções abaixo:"),
      selectInput("tpp", "TPP",
                  c("","TPP09","TPP10","TPP11","TPP12")
                  ),
      selectInput("plc", "PLC",
                  c("",4,5,6)
      )
    )
  ),
  dashboardBody(
    fixedPage(
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
                             selectInput("myHB1", 
                                         label = "Selecione o Hibrido",
                                         choices = unique(myDF$genotipo)),
                             br(),
                             h3("Enfezamento"),
                             br(),
                             column(width = 6, 
                                    fluidRow(plotOutput("locTPP09"))),
                             column(width = 6, 
                                    fluidRow(plotOutput("locTPP10"))),
                             br(),
                             column(width = 6, 
                                    fluidRow(plotOutput("locTPP11"))),
                             
                             column(width = 6, 
                                    fluidRow(plotOutput("locTPP12"))),
                             br(),
              
                
                             column(width = 6,
                                    fluidRow(plotOutput("jointLoc"))),

                             column(width = 6,
                                    selectInput("myHB2",
                                                label = "Selecione o Hibrido",
                                                choices = unique(myDF$genotipo)),
                                    fluidRow(plotOutput("compHB"))),
                             br(),
                             
                             column(width = 4,
                                    h4("Mancha Branca"),
                                    fluidRow(valueBoxOutput("lfspr", width = 12))),
                            
                             column(width = 4,
                                    h4("Cercospora"),
                                    fluidRow(valueBoxOutput("grlsr", width = 12))),
                             column(width = 4,
                                    h4("Turcicum"),
                                    fluidRow(valueBoxOutput("helmr", width = 12))),
                             br(),
                             
                             column(width = 4,
                                    h4("Bipolaris"),
                                    fluidRow(valueBoxOutput("sclbr", width = 12))),
                             column(width = 4,
                                    h4("Ferrugem"),
                                    fluidRow(valueBoxOutput("srstr", width = 12))),
                             column(width = 4,
                                    h4("Diplodia "),
                                    fluidRow(valueBoxOutput("dllfr", width = 12))),
                             
                           ))
                  )
      )
  )
)
