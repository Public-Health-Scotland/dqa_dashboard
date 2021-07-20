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
       flag %in% input$percentage_in 
        )
      })
  
  ## Set a filter for data item (the data item filter is nested and depends on the user's SMR selection)
   
  #update Data Item selection list based on SMR selection
  #if a user selects a data item and then changes their smr input,
  #their data item selection is preserved if it is a valid combination
  observeEvent(input$smr_in, {
    updateSelectInput(session, inputId = "data_item_in",
                      choices = c("(All)", unique(main_filters_completeness()$data_item)),
                      selected = if_else(input$data_item_in %in% 
                                           c("(All)", unique(main_filters_completeness()$data_item)),
                                        input$data_item_in, "(All)"
                                  )
                      )
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
             mini_plot, change_symbol, flag_symbol)%>%
      rename("SMR"="smr", "Health Board" = "hb_name", "Data Item" = "data_item",
             "Percentage Completeness" = "percent_complete_month",
             "Percentage Trend" = "mini_plot", "Change" = "change_symbol",
             "Percentage Threshhold" = "flag_symbol")
    
    dtable_completeness <- datatable(data = data,
                                     escape = FALSE,
                                     rownames = FALSE,
                                     class="compact stripe hover",
                                     selection = 'none',
                                     options = list(
                                             rowsGroup = list(0),
                                             drawCallback =  cb,
                                             columnDefs = list(
                                               list(className = 'dt-center', targets = "_all")
                                                          )
                                              )
                                     )%>%
                          spk_add_deps()
    
    dtable_completeness
    
  })


  

### SMR Timeliness ----------------------------------------------------------

  timeliness_filters <- reactive({
    timeliness %>%
      filter(smr == input$timeliness_smr_in, event_month_name == input$timeliness_month_in)
  })
    
  timeliness_long_filters <- reactive({
    timeliness_long %>% 
      filter(smr == input$timeliness_smr_in, event_month_name == input$timeliness_month_in) %>% 
      pivot_longer(cols = c(on_time, late), names_to = "submission_status", values_to = "submission_split")
  })

  
  output$timeliness_plot <- renderPlotly({
    
    plot <- ggplot(data=timeliness_long_filters(), aes(x=hb_name, y=submission_split, fill=submission_status, customdata=submission_status))+
      geom_col(data = timeliness_filters(), aes(x=hb_name, y=expected_submissions),fill = "#8FBFC2")+
      geom_col(position = "stack",width = 0.3)+
      scale_fill_manual(values = c("#D26146", "#3393DD"))+
      coord_flip()
    
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
      updateSelectInput(session, inputId = "Year", 
                        choices = c("(All)",unique(filters1()$year)),
                        selected = if_else(input$Year %in% c("(All)",unique(filters1()$year)),
                                                             input$Year, "(All)")
                        )
      
  })
  
    #Update Data Item Name selection based on inputs
  observeEvent(to_listen_audit(), {
      updateSelectInput(session,"DataItemName", 
                        choices = c("(All)",unique(filters1()$data_item_name)),
                        selected = if_else(input$DataItemName %in% 
                                           c("(All)",unique(filters1()$data_item_name)),
                                           input$DataItemName, "(All)" 
                                    )
      )
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
    data2 <- filters2() %>%
      select(audit, year, healthboard, hospital, data_item_name, accuracy_scotland, 
             accuracy_hospital)%>%
      rename("SMR" = "audit", "Year"="year", "Health Board" = "healthboard",
             "Hospital" = "hospital","Data Item" = "data_item_name", 
             "Accuracy Scotland" = "accuracy_scotland", 
             "Accuracy Hospital" = "accuracy_hospital")
    dtable_audit <- datatable(data = data2,
                              escape = FALSE,
                              rownames = FALSE,
                              class="compact stripe hover",
                              selection = 'none',
                              options = list(
                                rowsGroup = list(0),
                                columnDefs = list(
                                list(className = 'dt-center', targets = "_all")
                                 )
                               )
                              )
    
  })
  
  

# Clinical Coding Discrepancies SMR02 -------------------------------------

  
###the following lines relate to SMR02 coding discrepancies

  output$error_1 <- DT::renderDataTable({
    error1_filter <- error_1_table %>%
      filter(year == input$year1)%>%
      rename("Healthboard"="HBName", "Error Count"="error1",
             "Year"="year", "Percentage"="percentage_1")
    
    dtable_error1 <- datatable(data = error1_filter,
                               escape = FALSE,
                               rownames = FALSE,
                               class="compact stripe hover",
                               selection = 'none',
                               options = list(
                                 rowsGroup = list(0),
                                 columnDefs = list(
                                   list(className = 'dt-center', targets = "_all")
                                              )
                                          )
                               )
  })
  
  output$error_2 <- DT::renderDataTable({
    error2_filter <- error_2_table %>%
      filter(year == input$year2)%>%
      rename("Healthboard"="HBName", "Error Count"="error2",
             "Year"="year", "Percentage"="percentage_2")
    
    dtable_error2 <- datatable(data = error2_filter,
                               escape = FALSE,
                               rownames = FALSE,
                               class="compact stripe hover",
                               selection = 'none',
                               options = list(
                                 rowsGroup = list(0),
                                 columnDefs = list(
                                   list(className = 'dt-center', targets = "_all")
                                 )
                               )
    )
  })

  
  
  output$error_3 <- DT::renderDataTable({
   error3_filter <- error_3_table %>%
      filter(year == input$year3)%>%
      rename("Healthboard"="HBName", "Error Count"="error3",
             "Year"="year", "Percentage"="percentage_3")
   dtable_error3 <- datatable(data = error3_filter,
                              escape = FALSE,
                              rownames = FALSE,
                              class="compact stripe hover",
                              selection = 'none',
                              options = list(
                                rowsGroup = list(0),
                                columnDefs = list(
                                  list(className = 'dt-center', targets = "_all")
                                            )
                              )
                    )
  })
  
  
  output$error_4 <- DT::renderDataTable({
    error4_filter <- error_4_table %>%
      filter(year == input$year4)%>%
      rename("Healthboard"="HBName", "Error Count"="error4",
             "Year"="year", "Percentage"="percentage_4")
    dtable_error4 <- datatable(data = error4_filter,
                               escape = FALSE,
                               rownames = FALSE,
                               class="compact stripe hover",
                               selection = 'none',
                               options = list(
                                 rowsGroup = list(0),
                                 columnDefs = list(
                                   list(className = 'dt-center', targets = "_all")
                                 )
                               )
                      )
  })  
  
  
  output$error_5 <- DT::renderDataTable({
    error5_filter <- error_5_table %>%
      filter(year == input$year5)%>%
      rename("Healthboard"="HBName", "Error Count"="error5",
             "Year"="year", "Percentage"="percentage_5")
    dtable_error5 <- datatable(data = error5_filter,
                               escape = FALSE,
                               rownames = FALSE,
                               class="compact stripe hover",
                               selection = 'none',
                               options = list(
                                 rowsGroup = list(0),
                                 columnDefs = list(
                                   list(className = 'dt-center', targets = "_all")
                                 )
                               )
                    )
  })
  
  output$error_6 <- DT::renderDataTable({
   error6_filter <- error_6_table %>%
      filter(year == input$year6)%>%
      rename("Healthboard"="HBName", "Error Count"="error6",
             "Year"="year", "Percentage"="percentage_6")
   dtable_error6 <- datatable(data = error6_filter,
                              escape = FALSE,
                              rownames = FALSE,
                              class="compact stripe hover",
                              selection = 'none',
                              options = list(
                                rowsGroup = list(0),
                                columnDefs = list(
                                  list(className = 'dt-center', targets = "_all")
                                )
                              )
                    )
    
  })
  
  output$query <- DT::renderDataTable({
    query_filter <- query_1_table %>%
      filter(year == input$yearQ)%>%
      rename("Healthboard"="HBName", "Query Count"="query_count",
             "Year"="year", "Percentage"="query_percentage")
    dtable_query <- datatable(data = query_filter,
                              escape = FALSE,
                              rownames = FALSE,
                              class="compact stripe hover",
                              selection = 'none',
                              options = list(
                                rowsGroup = list(0),
                                columnDefs = list(
                                  list(className = 'dt-center', targets = "_all")
                                )
                              )
                    )
  })
  
  output$RCodes <- DT::renderDataTable({
    rcodes_filter <- RCodes_table %>%
      filter(year == input$yearR)%>%
      rename("Healthboard"="HBName", "Year"="year", 
             "Respiratory and Chest"="resp_chest", 
             "Abdominal Pain and Vomiting" = "APV",
             "Collapse and Convulsions" = "collapse_convuls",
             "All R codes" = "all"
             )
    dtable_rcodes <- datatable(data = rcodes_filter,
                               escape = FALSE,
                               rownames = FALSE,
                               class="compact stripe hover",
                               selection = 'none',
                               options = list(
                                 rowsGroup = list(0),
                                 columnDefs = list(
                                   list(className = 'dt-center', targets = "_all")
                                 )
                               )
    )

  })
  
  # output$split_1 <- DT::renderDataTable({
  #   DT::datatable(split_1_table, options = list(lengthMenu = c(15, 30, 50), pageLength = 15))
  # },
  # rownames = FALSE)
  # 
  # output$split_2 <- DT::renderDataTable({
  #   DT::datatable(split_2_table, options = list(lengthMenu = c(15, 30, 50), pageLength = 15))
  # },
  # filter = 'top',
  # rownames = FALSE)
  # 
  # output$split_3 <- DT::renderDataTable({
  #   DT::datatable(split_3_table, filter = 'top', options = list(lengthMenu = c(15, 30, 50), pageLength = 15))
  # },
  # rownames = FALSE)
  # 
  # output$split_4 <- DT::renderDataTable({
  #   DT::datatable(split_4_table, options = list(lengthMenu = c(15, 30, 50), pageLength = 15))
  # },
  # filter = 'top',
  # rownames = FALSE)
  # 
  # output$split_5 <- DT::renderDataTable({
  #   DT::datatable(split_5_table, options = list(lengthMenu = c(15, 30, 50), pageLength = 15))
  # },
  # filter = 'top',
  # rownames = FALSE)
  # 
  # output$error1map <- renderLeaflet({ #create maps
  #   error1map
  # })
  # 
  # output$error2map <- renderLeaflet({
  #   error2map
  # })
  # 
  # output$error3map <- renderLeaflet({
  #   error3map
  # })
  # 
  # output$error4map <- renderLeaflet({
  #   error4map
  # })
  # 
  # output$error5map <- renderLeaflet({
  #   error5map
  # })
  # 
  # output$error6map <- renderLeaflet({
  #   error6map
  # })
  # 
  # output$querymap <- renderLeaflet({
  #   query_map
  # })
  
  
})
