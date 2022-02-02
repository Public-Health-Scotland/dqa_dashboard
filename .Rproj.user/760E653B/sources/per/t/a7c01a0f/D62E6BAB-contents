
###Nested filters
shinyServer(function(input, output, session){

  SMRaudit <- reactive({
    filter(hb_mean, Audit == input$SMRaudit)
  }) 
  observeEvent(SMRaudit(), {
    choices <- unique(SMRaudit()$Year)
    updateSelectInput(inputId = "Year", choices = choices) 
  })
  
  
  
  Year <- reactive({
    req(input$Year)
    filter(SMRaudit(), Year == input$Year)
  }) 
  observeEvent(Year(), {
    choices <- unique(Year()$Healthboard)
    updateSelectInput(inputId = "Healthboard", choices = choices, selected = character()) 
  })
  
  Healthboard <- reactive({
    req(input$Healthboard)
    filter(Year(), Healthboard == input$Healthboard)
  })
  observeEvent(Healthboard(), {
    choices <- unique(Healthboard()$DataItemName)
    updateSelectInput(inputId = "DataItemName", choices=choices)
  })
  
  selected <- reactive({
    req(input$DataItemName)
    Healthboard()%>%
      filter(DataItemName == input$DataItemName) %>%
      select(Audit, Year, Healthboard, Hospital, DataItemName, MeanAccuracy, Accuracy)%>%
      rename("SMR" = "Audit", "Health Board" = "Healthboard", "Data Item" = "DataItemName", "Mean Accuracy Scotland" = "MeanAccuracy",
             "Mean Accuracy Hospital" = "Accuracy")
  })
  
  output$data <- renderTable(selected())
  
  })

