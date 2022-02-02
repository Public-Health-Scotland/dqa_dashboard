#SERVER

credentials <- readRDS(here::here("app/admin", "credentials.rds"))

shinyServer(function(input, output, session) {

  # Shinymanager Auth
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
### SMR Completeness --------------------------------------------------------

  ## Setting the main filters for SMR, HB and Percentage flag
  main_filters_completeness <- reactive({
    req(input$smr_in)
    req(input$hb_in)
    req(input$percentage_in)
    
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
    req(input$data_item_in)
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
                                       ),
                                       pageLength = 10,
                                       dom = 'Bfrtip'
                                     )
    )%>%
      spk_add_deps()
    
    dtable_completeness
    
  })

  #Completeness key
  output$completeness_key <- renderText({
    paste0("<h4>How to read this table</h4>",
    "<p> The 'Percentage Completeness' column contains figures for the month of ",
    comp_barchart_dates$max_month, " ", comp_barchart_dates$max_year,
    ". The barcharts show percentage completenes trends from ",
    comp_barchart_dates$min_month, " ", comp_barchart_dates$min_year,
    " to ", comp_barchart_dates$max_month, " ", comp_barchart_dates$max_year,
    ". </p>",
    as.character(icon("arrow-up", lib = "glyphicon")),
    " Increase from last month &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
    as.character(icon("arrow-down", lib = "glyphicon")),
    " Decrease from last month &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
    as.character(icon("minus", lib = "glyphicon")),
    " No change from last month </p>", br(),
    as.character(icon("ok", lib = "glyphicon")), 
    " Above 60% complete &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
    as.character(icon("warning-sign", lib = "glyphicon")),
    " Between 40% and 60% complete &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
    as.character(icon("flag", lib = "glyphicon")), " Below 40% complete "
    )
  })

  #format data for download
  completeness_download_data <- reactive({
    data_item_completeness() %>% 
      select(smr, hb_name, data_item, percent_complete_month,
             change, flag) %>%
      mutate(change = case_when(change == 0 ~ 
                                  "No change since previous month",
                                change == 1 ~
                                  "Decrease since previous month",
                                TRUE ~
                                  "Increase since previous month"
      ),
      flag = case_when(flag == 1 ~ "Above 60% complete",
                       flag == 2 ~ "Between 40% and 60% complete",
                       TRUE ~ "Below 40% complete"
      )
      )%>%
      
      rename("SMR"="smr", "Health Board" = "hb_name", "Data Item" = "data_item",
             "Percentage Completeness" = "percent_complete_month",
             "Change" = "change", "Percentage Threshhold" = "flag")
  })
  
  #Downloadable CSV of selected completeness dataset
  output$download_completeness <- downloadHandler(
    filename =function() {
      paste0("completeness_",str_to_lower(unique(data_item_completeness()$month_name)),".csv")
    },
    content = function(file){
      write.csv(completeness_download_data(), row.names = FALSE, file)
    }
  )

### SMR Timeliness ----------------------------------------------------------

  timeliness_filters <- reactive({
    req(input$timeliness_smr_in)
    req(input$timeliness_month_in)
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

  #filter and select rows to display in the timeliness Data tab
  timeliness_table_filter_smr <- reactive({
    req(input$timeliness_smr_in_2)
    if(input$timeliness_smr_in_2 %in% unique(timeliness$smr)){
      timeliness %>%
        filter(smr == input$timeliness_smr_in_2)
    }
    else{
      timeliness
    }
  })
  
  timeliness_table_filter_month <- reactive({
    req(input$timeliness_month_in_2)
    if(input$timeliness_month_in_2 %in% unique(timeliness$event_month_name)){
      timeliness_table_filter_smr() %>% 
        filter(event_month_name == input$timeliness_month_in_2) %>% 
        select(smr, hb_name, event_year, event_month_name, before_deadline, after_deadline, expected_submissions)
    }
    else{
      timeliness_table_filter_smr()%>% 
      select(smr, hb_name, event_year, event_month_name, before_deadline, after_deadline, expected_submissions)
    }
  })
  
  #Update the default selection on the data tab to be the same as the bullet chart user selection
  observeEvent(input$timeliness_smr_in,
               updateSelectInput(session, "timeliness_smr_in_2", choices = c(unique(timeliness$smr)),
                                 selected = input$timeliness_smr_in)
  )
  
  observeEvent(input$timeliness_month_in, 
               updateSelectInput(session, "timeliness_month_in_2", choices = c(unique(timeliness$event_month_name)),
                                 selected = input$timeliness_month_in)
  )
  


#render final table to display
 output$timeliness_rows <- DT::renderDataTable({ 
   datatable(data = timeliness_table_filter_month(),
             escape = FALSE,
             rownames = FALSE,
             class="compact stripe hover",
             selection = 'none',
             options = list(
               rowsGroup = list(0),
               columnDefs = list(
                 list(className = 'dt-center', targets = "_all")
               ),
               pageLength = 10,
               dom = 'Bfrtip'
             )
    )
  })
#downloadable csv of selected timeliness dataset
  output$download_timeliness <- downloadHandler(
    filename = function(){
      paste0("timeliness_", 
             ifelse(input$timeliness_month_in_2 %in% unique(timeliness$event_month_name), 
                    str_to_lower(unique(timeliness_table_filter_month()$event_month_name)), "all_months"),
             ".csv")
    },
    content = function(file){
      write.csv(timeliness_table_filter_month(), row.names = FALSE, file)
    }
  )

### SMR Audit ---------------------------------------------------------------


  ##Filters for SMR and health board
  filters1 <- reactive({
    req(input$SMRaudit)
    req(input$Healthboard)
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
    req(input$Year)
    req(input$DataItemName)
    filters1() %>%
      filter(case_when(input$Year %in% unique(filters1()$year) ~ year == input$Year,
                       TRUE ~ year == year),
             case_when(input$DataItemName %in% unique(filters1()$data_item_name) ~ data_item_name == input$DataItemName,
                       TRUE ~ data_item_name == data_item_name))
  })
  
  #filter rows of table to display
  audit_data <- reactive ({
    filters2() %>%
      select(audit, year, healthboard, hospital, data_item_name, accuracy_scotland, 
             accuracy_hospital)%>%
      rename("SMR" = "audit", "Year"="year", "Health Board" = "healthboard",
             "Hospital" = "hospital","Data Item" = "data_item_name", 
             "Accuracy Scotland" = "accuracy_scotland", 
             "Accuracy Hospital" = "accuracy_hospital")
  })
  
  #Render final table
  output$audit_table <- DT::renderDataTable({
      datatable(data = audit_data(),
      escape = FALSE,
      rownames = FALSE,
      class="compact stripe hover",
      selection = 'none',
      options = list(
        rowsGroup = list(0),
        columnDefs = list(
        list(className = 'dt-center', targets = "_all")
         ),
        pageLength = 15,
        dom = 'Bfrtip'
       )
      )
  })
  
  #download selected audit accuracy scores dataset
  output$download_audit <- downloadHandler(
    filename = function(){
      paste0("audit_accuracy_scores",".csv")
    },
    content = function(file){
      write.csv(audit_data(), row.names = FALSE, file)
    }
  )
  
# Clinical Coding Discrepancies SMR02 -------------------------------------

  
###the following lines relate to SMR02 coding discrepancies
 
  #Error 1 table filters, display and download functions 
  error1_filter <- reactive({
    req(input$year1)
    if (input$year1 %in% unique(smr02_diabetes$year)){
      smr02_diabetes %>%
        filter(source == "error_1_table", year == input$year1)
    }
    else {
      smr02_diabetes %>% 
        filter(source=="error_1_table") %>% 
        arrange(desc(year))
    }
  })
  error1_data <- reactive({
    error1_filter() %>% 
      rename("Healthboard"="HBName", "Error Count"="error",
             "Year"="year", "Percentage"="percentage")%>% 
      select(-source)
  })
  
  output$error_1 <- DT::renderDataTable({
    datatable(data = error1_data(),
    escape = FALSE,
    rownames = FALSE,
    class="compact stripe hover",
    selection = 'none',
    options = list(
      rowsGroup = list(0),
      columnDefs = list(
        list(className = 'dt-center', targets = "_all")
                   ),
      pageLength = 15,
      dom = 'Bfrtip'
               )
      )
  })
  
  output$download_smr02_error1 <- downloadHandler(
    filename = function(){
      paste0("smr02_error1_", ifelse(input$year1 %in% unique(smr02_diabetes$year), 
                                     unique(error1_data()$Year), "all_years"),
             ".csv")
    },
    content = function(file){
      write.csv(error1_data(), row.names = FALSE, file)
    }
  )
  
  #Error 2 table filters, display and download functions 
  error2_filter <- reactive({
    req(input$year2)
    if (input$year2 %in% unique(smr02_diabetes$year)){
      smr02_diabetes %>%
        filter(source == "error_2_table", year == input$year2)
    }
    
    else {
      smr02_diabetes %>%
        filter(source == "error_2_table") %>% 
        arrange(desc(year))
    }
  })
  
  error2_data <- reactive({
    error2_filter() %>%
      rename("Healthboard"="HBName", "Error Count"="error",
             "Year"="year", "Percentage"="percentage")%>% 
      select(-source)
  })
  
  output$error_2 <- DT::renderDataTable({
    datatable(data = error2_data(),
              escape = FALSE,
              rownames = FALSE,
              class="compact stripe hover",
              selection = 'none',
              options = list(
                rowsGroup = list(0),
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all")
                ),
                pageLength = 15,
                dom = 'Bfrtip'
              )
    )
  })

  output$download_smr02_error2 <- downloadHandler(
    filename = function(){
      paste0("smr02_error2_", ifelse(input$year2 %in% unique(smr02_diabetes$year), 
                                     unique(error2_data()$Year), "all_years"),".csv")
    },
    content = function(file){
      write.csv(error2_data(), row.names = FALSE, file)
    }
  )
  
  #Error 3 table filters, display and download functions 
  error3_filter <- reactive({
    req(input$year3)
    if (input$year3 %in% unique(smr02_diabetes$year)){
      smr02_diabetes %>%
        filter(source=="error_3_table", year == input$year3)
    }
    
    else {
      smr02_diabetes %>%
        filter(source=="error_3_table") %>% 
        arrange(desc(year))
    }
  })
  
  error3_data <-  reactive({
    error3_filter() %>%
      rename("Healthboard"="HBName", "Error Count"="error",
             "Year"="year", "Percentage"="percentage")%>% 
      select(-source)
  })
  
  output$error_3 <- DT::renderDataTable({
    datatable(data = error3_data(),
              escape = FALSE,
              rownames = FALSE,
              class="compact stripe hover",
              selection = 'none',
              options = list(
                rowsGroup = list(0),
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all")
                            ),
                pageLength = 15,
                dom = 'Bfrtip'
              )
    )
  })
  
  output$download_smr02_error3 <- downloadHandler(
    filename = function(){
      paste0("smr02_error3_", ifelse(input$year3 %in% unique(smr02_diabetes$year), 
                                     unique(error3_data()$Year), "all_years"), ".csv")
    },
    content = function(file){
      write.csv(error3_data(), row.names = FALSE, file)
    }
  )
  
  #Error 4 table filters, display and download functions 
  error4_filter <- reactive({
    req(input$year4)
    if (input$year4 %in% unique(smr02_diabetes$year)){
      smr02_diabetes %>%
        filter(source == "error_4_table",year == input$year4)
    }
    
    else {
      smr02_diabetes %>%
        filter(source == "error_4_table") %>% 
        arrange(desc(year))
    }
  })
  
  error4_data <- reactive({
    error4_filter() %>%
      rename("Healthboard"="HBName", "Error Count"="error",
             "Year"="year", "Percentage"="percentage") %>% 
      select(-source)
  })
  
  output$error_4 <- DT::renderDataTable({
    datatable(data = error4_data(),
              escape = FALSE,
              rownames = FALSE,
              class="compact stripe hover",
              selection = 'none',
              options = list(
                rowsGroup = list(0),
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all")
                ),
                pageLength = 15,
                dom = 'Bfrtip'
              )
      )
  })
  
  output$download_smr02_error4 <- downloadHandler(
    filename = function(){
      paste0("smr02_error4_", ifelse(input$year4 %in% unique(smr02_diabetes$year), 
                                     unique(error4_data()$Year), "all_years"),".csv")
    },
    content = function(file){
      write.csv(error4_data(), row.names = FALSE, file)
    }
  )
  
  #Error 5 table filters, display and download functions 
  error5_filter <- reactive({
    req(input$year5)
    if (input$year5 %in% unique(smr02_diabetes$year)){
      smr02_diabetes %>%
        filter(source=="error_5_table",year == input$year5)
    }
    
    else {
      smr02_diabetes %>%
        filter(source=="error_5_table") %>% 
        arrange(desc(year))
    }
  })
  
  error5_data <- reactive({
    error5_filter() %>%
      rename("Healthboard"="HBName", "Error Count"="error",
             "Year"="year", "Percentage"="percentage")%>% 
      select(-source)
  })
  
  output$error_5 <- DT::renderDataTable({
    datatable(data = error5_data(),
              escape = FALSE,
              rownames = FALSE,
              class="compact stripe hover",
              selection = 'none',
              options = list(
                rowsGroup = list(0),
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all")
                ),
                pageLength = 15,
                dom = 'Bfrtip'
              )
    )
  })
  
  output$download_smr02_error5 <- downloadHandler(
    filename = function(){
      paste0("smr02_error5_", ifelse(input$year5 %in% unique(smr02_diabetes$year), 
                                     unique(error5_data()$Year), "all_years"),".csv")
    },
    content = function(file){
      write.csv(error5_data(), row.names = FALSE, file)
    }
  )
  
  #Error 6 table filters, display and download functions 
  error6_filter <- reactive({
    req(input$year6)
    if (input$year6 %in% unique(smr02_diabetes$year)){
      smr02_diabetes %>%
        filter(source=="error_6_table", year == input$year6)
    }
    
    else {
      smr02_diabetes %>%
        filter(source=="error_6_table") %>% 
        arrange(desc(year))
    }
  })
  
  error6_data <- reactive({
    error6_filter() %>%
      rename("Healthboard"="HBName", "Error Count"="error",
             "Year"="year", "Percentage"="percentage")%>% 
      select(-source)
  })
  
  output$error_6 <- DT::renderDataTable({
    datatable(data = error6_data(),
              escape = FALSE,
              rownames = FALSE,
              class="compact stripe hover",
              selection = 'none',
              options = list(
                rowsGroup = list(0),
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all")
                ),
                pageLength = 15,
                dom = 'Bfrtip'
              )
        )
  })
  
  output$download_smr02_error6 <- downloadHandler(
    filename = function(){
      paste0("smr02_error6_", 
             ifelse(input$year6 %in% unique(smr02_diabetes$year), unique(error6_data()$Year), "all_years"),
             ".csv")
    },
    content = function(file){
      write.csv(error6_data(), row.names = FALSE, file)
    }
  )
  
  #Query 1 table filters, display and download functions 
  query1_filter <- reactive({
    req(input$yearQ)
    if (input$yearQ %in% unique(smr02_diabetes$year)){
      smr02_diabetes %>%
        filter(source == "query_1_table", year == input$yearQ)
    }
    
    else {
      smr02_diabetes %>%
        filter(source == "query_1_table") %>% 
        arrange(desc(year))
    }
  })
  
  query1_data <- reactive({
    query1_filter() %>%
      rename("Healthboard"="HBName", "Query Count"="error",
             "Year"="year", "Percentage"="percentage") %>% 
      select(-source)
  }) 
  
  output$query <- DT::renderDataTable({
    datatable(data = query1_data(),
              escape = FALSE,
              rownames = FALSE,
              class="compact stripe hover",
              selection = 'none',
              options = list(
                rowsGroup = list(0),
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all")
                ),
                pageLength = 15,
                dom = 'Bfrtip'
              )
    )
  })
 
  output$download_smr02_query1 <- downloadHandler(
    filename = function(){
      paste0("smr02_query1_", 
             ifelse(input$yearQ %in% unique(smr02_diabetes$year), unique(smr0()$Year), "all_years"),
             ".csv")
    },
    content = function(file){
      write.csv(query1_data(), row.names = FALSE, file)
    }
  )
  
  # R Codes -----------------------------------------------------------------
  
  rcodes_filter <- reactive({
    req(input$yearR)
    if (input$yearR %in% sort(unique(RCodes_table$year))){
      RCodes_table %>%
      filter(year == input$yearR)
    }

    else {
      RCodes_table[order(-RCodes_table$year), ]
    }
  })

  rcodes_data <- reactive({
    rcodes_filter()%>%
      rename("Healthboard"="HBName", "Year"="year",
             "Respiratory and Chest"="resp_chest",
             "Abdominal Pain and Vomiting" = "APV",
             "Collapse and Convulsions" = "collapse_convuls",
             "All R codes" = "all",
             "All Multi-episode Stays" = "n..") 
  })

  output$RCodes <- DT::renderDataTable({
    datatable(data = rcodes_data(),
              escape = FALSE,
              rownames = FALSE,
              class="compact stripe hover",
              selection = 'none',
              options = list(
                rowsGroup = list(0),
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all")
                ),
                pageLength = 15,
                dom = 'Bfrtip'
              )
    )
  })
  
  output$download_smr01_rcodes <- downloadHandler(
    filename = function(){
      paste0("smr02_rcodes_", 
             ifelse(input$yearR %in% unique(RCodes_table$year), unique(rcodes_data()$Year), "all_years"),
             ".csv")
    },
    content = function(file){
      write.csv(rcodes_data(), row.names = FALSE, file)
    }
  )
  
  
  
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
})
