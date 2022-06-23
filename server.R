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
    
  })
  
  }