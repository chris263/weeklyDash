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
                             column(width = 3, 
                                    fluidRow(valueBoxOutput("locN", width = 12))),
                             column(width = 3, 
                                    fluidRow(valueBoxOutput("trialN", width = 12))),
                             column(width = 3, 
                                    fluidRow(valueBoxOutput("datapN", width = 12))),
                             selectInput("select", h3("Select box"), 
                                         choices = list("Choice 1" = 1, "Choice 2" = 2,
                                                        "Choice 3" = 3), selected = 1),
                             br(),
                           ))
                  )
      )
  )
)
