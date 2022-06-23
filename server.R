# Weekly Dashboard
# Christiano Simoes
# June, 2022
# chris.simoes@syngenta.com

myDF <- readxl::read_xlsx("data/blupDoencas2022-06-17.xlsx",sheet = "Sheet1")
myCount <- readxl::read_xlsx("data/TPP09_count.xlsx",sheet = "Sheet1")

myGenLoc <- readxl::read_xlsx("data/TPP09_genoLoca-2022-06-23.xlsx",sheet = "Sheet1")
myChecks <- readxl::read_xlsx("data/renataMilho21062022.xlsx",sheet = "Checks")

server <- function(input, output, session) {
  
  observe({
    
    inTPP <- input$tpp
    inPLC <- input$plc
    selHB <- input$myHB
    
    # inTPP <- "TPP09"
    # inPLC <- 6
    
    myDF2 <- myGenLoc %>% filter(TPP == inTPP, stage==inPLC)
    
    
    reactLocN <- myCount %>% filter(TPP %in% inTPP, stage %in% inPLC, codigo == "Total")
    reactTrialN <- 100
    reactDatapN <- 25000
    
    # Can also set the label and select items
    updateSelectInput(session, "myHB1",
                      choices = unique(myDF2$genotipo),
                      selected = selHB
    )
    
    updateSelectInput(session, "myHB2",
                      choices = unique(myDF2$genotipo),
                      selected = selHB
    )
    
    output$locN <- renderValueBox({
      valueBox(value = reactLocN$BU,
               subtitle = "Locais",
               color = "green")
    })
    
    output$trialN <- renderValueBox({
      valueBox(value = reactLocN$experimentos,
               subtitle = "Trials",
               color = "light-blue")
    })
    
    output$datapN <- renderValueBox({
      valueBox(value = reactLocN$dataPoints,
               subtitle = "Data Points",
               color = "orange")
    })
    
    output$locTPP09 <- renderValueBox({
      valueBox(value = reactLocN,
               subtitle = "Grafico TPP09",
               color = "green")
    })
    
    output$locTPP10 <- renderValueBox({
      valueBox(value = reactTrialN,
               subtitle = "Grafico TPP10",
               color = "light-blue")
    })
    
    output$locTPP11 <- renderValueBox({
      valueBox(value = reactDatapN,
               subtitle = "Grafico TPP11",
               color = "orange")
    })
    output$locTPP12 <- renderValueBox({
      valueBox(value = reactDatapN,
               subtitle = "Grafico TPP12",
               color = "red")
    })
    
    output$jointLoc <- renderValueBox({
      valueBox(value = reactDatapN,
               subtitle = "Grafico analise conjunta",
               color = "green")
    })
    
    output$compHB <- renderValueBox({
      valueBox(value = reactDatapN,
               subtitle = "Grafico analise conjunta",
               color = "orange")
    })
    
    output$lfstr <- renderValueBox({
      valueBox(value = reactDatapN,
               subtitle = "Grafico Mancha Branca",
               color = "blue")
    })
    
    output$grlsr <- renderValueBox({
      valueBox(value = reactDatapN,
               subtitle = "Grafico Cercospora",
               color = "light-blue")
    })
    
    output$helmr <- renderValueBox({
      valueBox(value = reactDatapN,
               subtitle = "Grafico Turcicum",
               color = "green")
    })
    
    output$sclbr <- renderValueBox({
      valueBox(value = reactDatapN,
               subtitle = "Grafico Bipolaris",
               color = "green")
    })
    
    output$srstr <- renderValueBox({
      valueBox(value = reactDatapN,
               subtitle = "Grafico Ferrugem",
               color = "orange")
    })
    
    output$dllfr <- renderValueBox({
      valueBox(value = reactDatapN,
               subtitle = "Grafico Diploadia",
               color = "blue")
    })
    
    
    
  })
  
  }