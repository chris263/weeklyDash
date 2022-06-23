# Weekly Dashboard
# Christiano Simoes
# June, 2022
# chris.simoes@syngenta.com

myDF <- readxl::read_xlsx("data/blupDoencas2022-06-17.xlsx",sheet = "Sheet1")

server <- function(input, output, session) {
  
  observe({
    
    
    reactLocN <- 30
    reactTrialN <- 100
    reactDatapN <- 25000
    
    output$locN <- renderValueBox({
      valueBox(value = reactLocN,
               subtitle = "Locais",
               color = "green")
    })
    
    output$trialN <- renderValueBox({
      valueBox(value = reactTrialN,
               subtitle = "Trials",
               color = "light-blue")
    })
    
    output$datapN <- renderValueBox({
      valueBox(value = reactDatapN,
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