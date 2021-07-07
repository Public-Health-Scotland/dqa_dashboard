shinyServer(function(input, output, session) {


### SMR Completeness --------------------------------------------------------

  ## Setting the main filters for SMR, HB and Percentage flag
  main_filters_completeness <- reactive({ 
    
  smr_completeness %>%
      
  filter(case_when(input$smr_in %in% unique(smr_completeness$smr)
                   ~smr == input$smr_in,
                   
                   TRUE ~ smr == smr),
         
        case_when(input$hb_in %in% unique(smr_completeness$hb_name)
                  ~ hb_name == input$hb_in,
                  
                       TRUE ~ hb_name==hb_name),
        
        
        case_when(input$percentage_in %in% unique(smr_completeness$flag) 
                  ~ flag == input$percentage_in,
                  
                  TRUE ~ flag == flag)
        )
      })
  
  ## Set a filter for data item (the data item filter is nested and depends on the user's SMR selection)
   
  #update Data Item selection list based on SMR selection
  observeEvent(input$smr_in, {
    updateSelectInput(session, inputId = "data_item_in",
                      choices = c("(All)", unique(main_filters_completeness()$data_item)))
  })
  
  #implement filter
  data_item_completeness <- reactive({
    main_filters_completeness()%>%
    filter(case_when(input$data_item_in %in% unique(main_filters_completeness()$data_item) ~
                       data_item == input$data_item_in,
                     TRUE ~ data_item == data_item))
  })
  
  ## Render the final table
  output$completeness_table <- DT::renderDataTable({
    
    cb <- htmlwidgets::JS('function(){debugger;HTMLWidgets.staticRender();}')
    
    data <- data_item_completeness()%>%
      select(smr, hb_name, data_item, percent_complete_month, 
             mini_plot, change_symbol, flag_symbol)
    
    dtable_completeness <- datatable(data = data,
                                     escape = FALSE,
                                     rownames = FALSE,
                                     selection = 'none',
                                     options = list(
                                       rowsGroup = list(0),
                                       drawCallback =  cb
                                       # rowCallback = JS(tooltips)
                                                    )
                                     )%>%
                          spk_add_deps()
    
    dtable_completeness
    
  })

  

### SMR Audit ---------------------------------------------------------------


  ##Filters for SMR and health board
  filters1 <- reactive({
    smr_audit %>%
      filter(case_when(input$SMRaudit %in% unique(smr_audit$audit) ~ audit==input$SMRaudit,
                       TRUE ~ audit == audit),
             case_when(input$Healthboard %in% unique(smr_audit$healthboard) ~ healthboard == input$Healthboard,
                       TRUE ~ healthboard == healthboard))
  })
  
  ##Filters for the Year and Data Item Name, they both depend on the SMR and HB chosen by user
  
    #Since Data Item Name and SMR depend on multiple inputs, we store the inputs they rely on in a reactive list
  to_listen_audit <- reactive({
    list(input$SMRaudit, input$Healthboard)
  })
    #Update Year selection based on inputs
  observeEvent(to_listen_audit(), {
    updateSelectInput(session, inputId = "Year", choices = c("(All)",unique(filters1()$year)))
  })
    #Update Data Item Name selection based on inputs
  observeEvent(to_listen_audit(), {
    updateSelectInput(session,"DataItemName", choices = c("(All)",unique(filters1()$data_item_name)))
  })  
    #Apply Year and Data Item Name filters to data
  filters2 <- reactive({
    filters1() %>%
      filter(case_when(input$Year %in% unique(filters1()$year) ~ year == input$Year,
                       TRUE ~ year == year),
             case_when(input$DataItemName %in% unique(filters1()$data_item_name) ~ data_item_name == input$DataItemName,
                       TRUE ~ data_item_name == data_item_name))
  })
  
  ##Render final table
  output$audit_data <- DT::renderDataTable({
    filters2() %>%
      select(audit, year, healthboard, hospital, data_item_name, accuracy_scotland, accuracy_hospital)%>%
      rename("SMR" = "audit", "Health Board" = "healthboard", "Data Item" = "data_item_name", "Accuracy Scotland" = "accuracy_scotland",
             "Accuracy Hospital" = "accuracy_hospital")
    
  })
  
  

# Clinical Coding Discrepancies SMR02 -------------------------------------

  output$error_1 <- DT::renderDataTable({
    DT::datatable(error_1_table, options = list(lengthMenu = c(15, 30, 50), pageLength = 15)) #default the displayed rows
  },
  filter = 'top',
  rownames = FALSE)    
  
  output$error_2 <- DT::renderDataTable({
    DT::datatable(error_2_table, options = list(lengthMenu = c(15, 30, 50), pageLength = 15))
  },
  filter = 'top',
  rownames = FALSE) 
  
  
  output$error_3 <- DT::renderDataTable({
    DT::datatable(error_3_table, options = list(lengthMenu = c(15, 30, 50), pageLength = 15))
  },
  filter = 'top',
  rownames = FALSE)
  
  
  output$error_4 <- DT::renderDataTable({
    DT::datatable(error_4_table, options = list(lengthMenu = c(15, 30, 50), pageLength = 15))
  },
  filter = 'top',
  rownames = FALSE)  
  
  
  output$error_5 <- DT::renderDataTable({
    DT::datatable(error_5_table, options = list(lengthMenu = c(15, 30, 50), pageLength = 15))
  },
  filter = 'top',
  rownames = FALSE)
  
  output$error_6 <- DT::renderDataTable({
    DT::datatable(error_6_table, options = list(lengthMenu = c(15, 30, 50), pageLength = 15))
  },
  filter = 'top',
  rownames = FALSE)
  
  output$query <- DT::renderDataTable({
    DT::datatable(query_1_table, options = list(lengthMenu = c(15, 30, 50), pageLength = 15))
  },
  filter = 'top',
  rownames = FALSE)
  
  output$split_1 <- DT::renderDataTable({
    DT::datatable(split_1_table, options = list(lengthMenu = c(15, 30, 50), pageLength = 15))
  },
  filter = 'top',
  rownames = FALSE)
  
  output$split_2 <- DT::renderDataTable({
    DT::datatable(split_2_table, options = list(lengthMenu = c(15, 30, 50), pageLength = 15))
  },
  filter = 'top',
  rownames = FALSE)
  
  output$split_3 <- DT::renderDataTable({
    DT::datatable(split_3_table, options = list(lengthMenu = c(15, 30, 50), pageLength = 15))
  },
  filter = 'top',
  rownames = FALSE)
  
  output$split_4 <- DT::renderDataTable({
    DT::datatable(split_4_table, options = list(lengthMenu = c(15, 30, 50), pageLength = 15))
  },
  filter = 'top',
  rownames = FALSE)
  
  output$split_5 <- DT::renderDataTable({
    DT::datatable(split_5_table, options = list(lengthMenu = c(15, 30, 50), pageLength = 15))
  },
  filter = 'top',
  rownames = FALSE)
  
  output$error1map <- renderLeaflet({ #create maps
    error1map
  })
  
  output$error2map <- renderLeaflet({
    error2map
  })
  
  output$error3map <- renderLeaflet({
    error3map
  })
  
  output$error4map <- renderLeaflet({
    error4map
  })
  
  output$error5map <- renderLeaflet({
    error5map
  })
  
  output$error6map <- renderLeaflet({
    error6map
  })
  
  output$querymap <- renderLeaflet({
    query_map
  })
  
  
})
