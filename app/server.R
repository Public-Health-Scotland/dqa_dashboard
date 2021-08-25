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
      scale_fill_manual(values = c("#AF69A9","#3F3685", "#80BCEA")
                        )+
      labs(x = "Health Board", y= "Submission Counts")+
      coord_flip()+
      theme(panel.grid.major.x = element_line(colour = "light grey"),
            panel.background = element_blank())

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
                                pageLength = 15,
                                dom = 'Bfrtip',
                                buttons = c('copy', 'csv', 'excel', 'pdf')
                               )
                              )
    
  })
  
  
# Clinical Coding Discrepancies SMR02 -------------------------------------

  
###the following lines relate to SMR02 coding discrepancies
  
  error1_filter <- reactive({
    
    if (input$year1 %in% unique(error_1_table$year)){
      error_1_table %>%
        filter(year == input$year1)
    }
    
    else {
      error_1_table[order(-error_1_table$year), ]
    }
  })

  output$error_1 <- DT::renderDataTable({
    error1_data <- error1_filter() %>% 
      rename("Healthboard"="HBName", "Error Count"="error1",
             "Year"="year", "Percentage"="percentage_1")
    
    dtable_error1 <- datatable(data = error1_data,
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
  
  error2_filter <- reactive({
    
    if (input$year2 %in% unique(error_2_table$year)){
      error_2_table %>%
        filter(year == input$year2)
    }
    
    else {
      error_2_table[order(-error_2_table$year), ]
    }
  })
  
  output$error_2 <- DT::renderDataTable({
    error2_data <- error2_filter() %>%
      rename("Healthboard"="HBName", "Error Count"="error2",
             "Year"="year", "Percentage"="percentage_2")
    
    dtable_error2 <- datatable(data = error2_data,
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

  error3_filter <- reactive({
    
    if (input$year3 %in% unique(error_3_table$year)){
      error_3_table %>%
        filter(year == input$year3)
    }
    
    else {
      error_3_table[order(-error_3_table$year), ]
    }
  })
  
  output$error_3 <- DT::renderDataTable({
   error3_data <- error3_filter() %>%
      rename("Healthboard"="HBName", "Error Count"="error3",
             "Year"="year", "Percentage"="percentage_3")
   dtable_error3 <- datatable(data = error3_data,
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
  error4_filter <- reactive({
    
    if (input$year4 %in% unique(error_4_table$year)){
      error_4_table %>%
        filter(year == input$year4)
    }
    
    else {
      error_4_table[order(-error_4_table$year), ]
    }
  })
  
  output$error_4 <- DT::renderDataTable({
    error4_data <- error4_filter() %>%
      rename("Healthboard"="HBName", "Error Count"="error4",
             "Year"="year", "Percentage"="percentage_4")
    dtable_error4 <- datatable(data = error4_data,
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
  
  error5_filter <- reactive({
    
    if (input$year5 %in% unique(error_5_table$year)){
      error_5_table %>%
        filter(year == input$year5)
    }
    
    else {
      error_5_table[order(-error_5_table$year), ]
    }
  })
  
  
  output$error_5 <- DT::renderDataTable({
    error5_data <- error5_filter() %>%
      rename("Healthboard"="HBName", "Error Count"="error5",
             "Year"="year", "Percentage"="percentage_5")
    dtable_error5 <- datatable(data = error5_data,
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
  error6_filter <- reactive({
    
    if (input$year6 %in% unique(error_6_table$year)){
      error_6_table %>%
        filter(year == input$year6)
    }
    
    else {
      error_6_table[order(-error_6_table$year), ]
    }
  })
  
  output$error_6 <- DT::renderDataTable({
   error6_data <- error6_filter() %>%
      rename("Healthboard"="HBName", "Error Count"="error6",
             "Year"="year", "Percentage"="percentage_6")
   dtable_error6 <- datatable(data = error6_data,
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
  
  query1_filter <- reactive({
    
    if (input$yearQ %in% unique(query_1_table$year)){
      query_1_table %>%
        filter(year == input$yearQ)
    }
    
    else {
      query_1_table[order(-query_1_table$year), ]
    }
  })
  
  output$query <- DT::renderDataTable({
    query1_data <- query1_filter() %>%
      rename("Healthboard"="HBName", "Query Count"="query_count",
             "Year"="year", "Percentage"="query_percentage")
    dtable_query <- datatable(data = query1_data,
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
 
  
  # R Codes -----------------------------------------------------------------
  
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
  

# Text outputs ------------------------------------------------------------

  output$text1 <- renderText({
    paste("This map shows the percentage of general/acute inpatient day cases (SMR01) in the last year (06/20-06/21) across Scotland.", 
          "It is a flexible functionality that can accommodate many types of data to suit user needs.", sep="\n")
  })
  
  output$SMRtext <- renderText({
    paste(
        'Healthcare data for individual patients is collected as a series of Scottish Morbidity Records (SMR).',
        'The record type denotes the general type of healthcare received during an episode and/or the nature or status of the patient.',
        ' ',
        'SMR01 -  is an episode-based patient record relating to all inpatients and day cases discharged from non-obstetric and non-psychiatric specialties.', 
        'A record is generated when a patient completes an episode of inpatient or day case care.', 
        'Examples include discharge home from hospital, transfer to another clinician (either at the same or a different hospital),',
        'a change of specialty (either under the same or a different clinician), or death.', 
        'There are now over 1,000,000 SMR01s generated each year. Data collected include patient identifiable and demographic details,',
        'episode management details and general clinical information.', 
        'Currently diagnoses are recorded using the ICD-10 classification and operations are recorded using the OPCS-4 classification.', 
        'Information such as waiting time for inpatient/day case admission and length of stay may be derived from the episode management data.',
        ' ',
        'SMR00 - relates to all outpatients (new and follow-up) in specialties other than Accident & Emergency (A&E), and Genito-Urinary Medicine.',
        'SMR00 includes follow-up as well as new attendances and DNAs where computer recording is in place.',
        ' ',
        
        'SMR02 - is an episode based patient record relating to all inpatients and day cases discharged from Obstetric specialties in the NHS Scotland.',
        'A record is generated for each episode, of which there are about 125,000 each year; about 50% are non-delivery episodes.',
        'The basic data set for the mother includes Patient Identification and Demographic information, Episode Management information and',
        'Maternity Clinical information. The basic data set for the baby includes birthweight, sex, apgar score and neonatal indicator', 
        '(Refer to SMR Data Manual for codes and values). New data items including drug misuse during current pregnancy and alcohol consumption',
        'will be added to the SMR02 dataset in October 2002.',
        ' ',
        'SMR04 - is an episode based patient record relating to all inpatients and day cases admitted to and discharged from Mental Health specialties.',
        'SMR04 consists of two parts:',
          '•	Admission information including status on admission and diagnosis information.',
          '•	Discharge information (which includes a copy of the admission details) including diagnosis information.',
        'About 32,000 admission, and 32,000 discharge records are generated each year. Analyses can be carried out on admissions,' ,
        'discharges and residents from the available data.',
        sep="\n")
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
  output$countmap <- renderLeaflet({
    countmap
  })
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
