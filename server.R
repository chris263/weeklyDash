# Weekly Dashboard
# Christiano Simoes
# June, 2022
# chris.simoes@syngenta.com


server <- function(input, output, session) {
  
  observe({
    
    inTPP <- input$tpp
    inPLC <- input$plc

    
    # inTPP <- "TPP09"
    # inPLC <- 6
    
    myDF2 <- myGenLoc %>% filter(TPP == inTPP, stage==inPLC)
    myDF2$genotipo <- toupper(myDF2$genotipo)
    
    reactLocN <- myCount %>% filter(TPP %in% inTPP, stage %in% inPLC, codigo == "Total")
    reactTrialN <- 100
    reactDatapN <- 25000
    
    # Can also set the label and select items
    updateSelectInput(session, "myHB1",
                      choices = unique(myDF2$genotipo),
                      selected = ""
    )
    
    updateSelectInput(session, "myHB2",
                      choices = unique(myDF2$genotipo),
                      selected = ""
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
    
    output$locTPP09 <- renderPlot({
      makeG0(myGenLoc,input$myHB1, "TPP09", input$plc)
    }, heigh = 350, width = 300)
    
    output$locTPP10 <- renderPlot({
      makeG0(myGenLoc,input$myHB1, "TPP10", input$plc)
    }, heigh = 350, width = 300)
    
    output$locTPP11 <- renderPlot({
      makeG0(myGenLoc,input$myHB1, "TPP11", input$plc)
    }, heigh = 350, width = 300)
    
    output$locTPP12 <- renderPlot({
      makeG0(myGenLoc,input$myHB1, "TPP12", input$plc)
    }, heigh = 350, width = 300)
    
    output$jointLoc <- renderPlot({
      makeG01(myJoint,myChecks, input$myHB1)
    }, heigh = 450, width = 300)
    
    output$compHB <- renderPlot({
      makeG02(myJoint,input$myHB1,input$myHB2)
    }, heigh = 350, width = 300 )
    
    output$lfspr <- renderValueBox({
      myV1 <- myDD %>% filter(genotipo == input$myHB1, trait == "LFSPR") %>% select(blupNota)
      numb1 <- round(max(myV1$blupNota),0)
      if(nrow(myV1) == 0){ numb1 = NA}
      valueBox(value = NA,
               subtitle = "Mancha Branca",
               color = colorFun(numb1))
    })
    
    output$grlsr <- renderValueBox({
      myV2 <- myDD %>% filter(genotipo == input$myHB1, trait == "GRLSR") %>% select(blupNota)
      numb2 <- round(max(myV2$blupNota),0)
      if(nrow(myV2) == 0){ numb2 = NA}
      
      valueBox(value = numb2,
               subtitle = "Cercospora",
               color = colorFun(numb2))
    })
    
    output$helmr <- renderValueBox({
      myV3 <- myDD %>% filter(genotipo == input$myHB1, trait == "HELMR") %>% select(blupNota)
      numb3 <- round(max(myV3$blupNota),0)
      if(nrow(myV3) == 0){ numb3 = NA}
      valueBox(value = numb3,
               subtitle = "Grafico Turcicum",
               color = colorFun(numb3))
    })
    
    output$sclbr <- renderValueBox({
      myV4 <- myDD %>% filter(genotipo == input$myHB1, trait == "SCLBR") %>% select(blupNota)
      numb4 <- round(max(myV4$blupNota),0)
      if(nrow(myV4) == 0){ numb4 = NA}
      
      valueBox(value = numb4,
               subtitle = "Grafico Bipolaris",
               color = colorFun(numb4))
    })
    
    output$srstr <- renderValueBox({
      myV5 <- myDD %>% filter(genotipo == input$myHB1, trait == "SRSTR") %>% select(blupNota)
      numb5 <- round(max(myV5$blupNota),0)
      if(nrow(myV5) == 0){ numb5 = NA}
      
      valueBox(value = numb5,
               subtitle = "Grafico Ferrugem",
               color = colorFun(numb5))
    })
    
    output$dllfr <- renderValueBox({
      myV6 <- myDD %>% filter(genotipo == input$myHB1, trait == "DLLFR") %>% select(blupNota)
      numb6 <- round(max(myV6$blupNota),0)
      if(nrow(myV6) == 0){ numb6 = NA}
      
      valueBox(value = numb6,
               subtitle = "Grafico Diploadia",
               color = colorFun(numb6))
    })
    
    
    
  })
  
  }