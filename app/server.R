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
                                     extensions = 'Buttons',
                                     options = list(
                                       rowsGroup = list(0),
                                       drawCallback =  cb,
                                       columnDefs = list(
                                         list(className = 'dt-center', targets = "_all")
                                       ),
                                       pageLength = 10,
                                       dom = 'Bfrtip',
                                       buttons = c('copy', 'csv', 'excel', 'pdf')
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
    timeliness %>% 
      filter(smr == input$timeliness_smr_in, event_month_name == input$timeliness_month_in) %>% 
      pivot_longer(cols = c(before_deadline, after_deadline), names_to = "submission_status", 
                   values_to = "submission_count_split")
  })
  
  
  
  output$timeliness_mean_on_time <- renderText({
    mean_on_time <- round(mean(timeliness_filters()$before_deadline), 2)
    paste("Average number of records submitted on time:",mean_on_time, sep = " ")
  })
  
  output$timeliness_mean_late <- renderText(({
    mean_late <- round(mean(timeliness_filters()$after_deadline),2)
    paste("Average number of records submitted after the deadline:",mean_late, sep = " ")
    
  }))


  output$timeliness_plot <- renderPlotly({

     plot <- ggplot(data=timeliness_long_filters()
                   )+
      geom_col(data = timeliness_filters(),
               aes(x=hb_name, y=expected_submissions, fill = "expected_submissions"),
               alpha = 0.6, name = "expected submissions", show.legend = TRUE)+
      geom_col(position = "stack",width = 0.3, 
               aes(x=hb_name, y=submission_count_split, fill=submission_status))+
      scale_fill_manual(values = c("#C73918", "#0078D4","#80BCEA")
                        )+
      labs(x = "Health Board", y= "Submission Counts")+
      coord_flip()

    plotly::ggplotly(plot) %>%
      layout(legend = list(x = 0.72, y = 0.95))%>%
      layout(legend=list(title=list(text='<b> Legend </b>')))
    
    

  })


 output$timeliness_rows <- DT::renderDataTable({ 
   
   timeliness_data <- timeliness %>%
     filter(smr == input$timeliness_smr_in_2, 
            event_month_name == input$timeliness_month_in_2) %>% 
     select(smr, hb_name, event_year, event_month_name,
            before_deadline, after_deadline, expected_submissions)
   
    dtable_timeliness <- datatable(data = timeliness_data,
                                   escape = FALSE,
                                   rownames = FALSE,
                                   class="compact stripe hover",
                                   selection = 'none',
                                   extensions = 'Buttons',
                                   options = list(
                                     rowsGroup = list(0),
                                     columnDefs = list(
                                       list(className = 'dt-center', targets = "_all")
                                     ),
                                     pageLength = 10,
                                     dom = 'Bfrtip',
                                     buttons = c('copy', 'csv', 'excel', 'pdf')
                                   )
                          )
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
  output$audit_table <- DT::renderDataTable({
    audit_data <- filters2() %>%
      select(audit, year, healthboard, hospital, data_item_name, accuracy_scotland, 
             accuracy_hospital)%>%
      rename("SMR" = "audit", "Year"="year", "Health Board" = "healthboard",
             "Hospital" = "hospital","Data Item" = "data_item_name", 
             "Accuracy Scotland" = "accuracy_scotland", 
             "Accuracy Hospital" = "accuracy_hospital")
    dtable_audit <- datatable(data = audit_data,
                              escape = FALSE,
                              rownames = FALSE,
                              class="compact stripe hover",
                              selection = 'none',
                              extensions = 'Buttons',
                              options = list(
                                rowsGroup = list(0),
                                columnDefs = list(
                                list(className = 'dt-center', targets = "_all")
                                 ),
                                pageLength = 10,
                                dom = 'Bfrtip',
                                buttons = c('copy', 'csv', 'excel', 'pdf')
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
                               extension = 'Buttons',
                               options = list(
                                 rowsGroup = list(0),
                                 columnDefs = list(
                                   list(className = 'dt-center', targets = "_all")
                                              ),
                                 pageLength = 15,
                                 dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'pdf')
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
                               extension = 'Buttons',
                               options = list(
                                 rowsGroup = list(0),
                                 columnDefs = list(
                                   list(className = 'dt-center', targets = "_all")
                                 ),
                                 pageLength = 15,
                                 dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'pdf')

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
                              extension = 'Buttons',
                              options = list(
                                rowsGroup = list(0),
                                columnDefs = list(
                                  list(className = 'dt-center', targets = "_all")
                                            ),
                                pageLength = 15,
                                dom = 'Bfrtip',
                                buttons = c('copy', 'csv', 'excel', 'pdf')
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
                               extension = 'Buttons',
                               options = list(
                                 rowsGroup = list(0),
                                 columnDefs = list(
                                   list(className = 'dt-center', targets = "_all")
                                 ),
                                 pageLength = 15,
                                 dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'pdf')
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
                               extension = 'Buttons',
                               options = list(
                                 rowsGroup = list(0),
                                 columnDefs = list(
                                   list(className = 'dt-center', targets = "_all")
                                 ),
                                 pageLength = 15,
                                 dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'pdf')
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
                              extension = 'Buttons',
                              options = list(
                                rowsGroup = list(0),
                                columnDefs = list(
                                  list(className = 'dt-center', targets = "_all")
                                ),
                                pageLength = 15,
                                dom = 'Bfrtip',
                                buttons = c('copy', 'csv', 'excel', 'pdf')
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
                              extension = 'Buttons',
                              options = list(
                                rowsGroup = list(0),
                                columnDefs = list(
                                  list(className = 'dt-center', targets = "_all")
                                ),
                                pageLength = 15,
                                dom = 'Bfrtip',
                                buttons = c('copy', 'csv', 'excel', 'pdf')
                              )
                    )
  })
  
  rcodes_filter <- reactive({
    
    if (input$yearR %in% sort(unique(RCodes_table$year))){
      RCodes_table %>%
      filter(year == input$yearR)
    }

    else {
      RCodes_table[order(-RCodes_table$year), ]
    }
  })
  
  
  output$RCodes <- DT::renderDataTable({
    
    rcodes_data <- rcodes_filter()%>%
      rename("Healthboard"="HBName", "Year"="year",
             "Respiratory and Chest"="resp_chest",
             "Abdominal Pain and Vomiting" = "APV",
             "Collapse and Convulsions" = "collapse_convuls",
             "All R codes" = "all",
             "All Multi-episode Stays" = "n..")

    dtable_rcodes <- datatable(data = rcodes_data,
                               escape = FALSE,
                               rownames = FALSE,
                               class="compact stripe hover",
                               selection = 'none',
                               extension = 'Buttons',
                               options = list(
                                 rowsGroup = list(0),
                                 columnDefs = list(
                                   list(className = 'dt-center', targets = "_all")
                                 ),
                                 pageLength = 15,
                                 dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'pdf')
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
