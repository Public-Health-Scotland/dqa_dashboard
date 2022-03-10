#SERVER

credentials <- readRDS(here::here("admin", "credentials.rds"))

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
  completeness_main_filters <- reactive({
    req(input$smr_in)
    # req(input$hb_in)
    req(input$percentage_in)
    
      smr_completeness %>%
      filter(case_when(input$smr_in %in% unique(smr_completeness$smr)
                       ~smr == input$smr_in,
                       TRUE ~ smr == smr),
       flag %in% input$percentage_in 
        )
      })

  #update HB dropdown list based on SMR selection
  observeEvent(input$smr_in,
               updateSelectInput(session, "hb_in", 
                                 choices = c("(All)", unique(completeness_main_filters()$hb_name))[order(c("(All)", unique(completeness_main_filters()$hb_name)))],
                                 selected = if_else(input$hb_in %in% 
                                                      unique(completeness_main_filters()$hb_name),
                                                    input$hb_in, "(All)")
                                 )
  )
  
  #update data item dropdown list based on SMR selection
  observeEvent(input$smr_in, {
    updateSelectInput(session, inputId = "data_item_in",
                      choices = c("(All)", unique(completeness_main_filters()$data_item))[order(c("(All)", unique(completeness_main_filters()$data_item)))],
                      selected = if_else(input$data_item_in %in% 
                                           unique(completeness_main_filters()$data_item),
                                        input$data_item_in, "(All)")
                      )
  })
  
  #implement hb filters
  completeness_hb_filter <- reactive({
    req(input$hb_in)
    
    if(input$hb_in %in% unique(completeness_main_filters()$hb_name)){
      completeness_main_filters()%>%
        filter(hb_name == input$hb_in)
    }
    
    else{
      completeness_main_filters()
    }
  })
  
  #implement data item filters
  completeness_data_item_filter <- reactive({
    req(input$data_item_in)
    
    if(input$data_item_in %in% unique(completeness_hb_filter()$data_item)){
      completeness_hb_filter() %>% 
        filter(data_item == input$data_item_in)
    }
    
    else{
      completeness_hb_filter()
    }
  })
  
  ## Render the final table
  output$completeness_table <- DT::renderDataTable({
    
    cb <- htmlwidgets::JS('function(){debugger;HTMLWidgets.staticRender();}')
    
    data <- completeness_data_item_filter()%>%
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

#the bullet chart is composed of two superimposed plots, one for expected submissions and one for the actual submissions
  
  #implement smr and year filters
  timeliness_smr_year <- reactive({
    req(input$timeliness_smr_in)
    req(input$timeliness_year_in)
  
    timeliness %>%
      filter(smr == input$timeliness_smr_in, event_year == input$timeliness_year_in)
  })
  
  #update list of choices in the month dropdown based on user year input
  observeEvent(input$timeliness_year_in,
               updateSelectInput(session, "timeliness_month_in", 
                                 choices = c(unique(timeliness_smr_year()$event_month_name))[order(c(unique(timeliness_smr_year()$event_month)))])
  )
  
  #implement hb filter, reactive data for plotting expected submissions
  timeliness_expected <- reactive({
    req(input$timeliness_month_in)
    
    timeliness_smr_year() %>%
      filter(event_month_name == input$timeliness_month_in)
  })
  
  #reactive data for plotting the actual number of submissions
  timeliness_submitted <- reactive({
    timeliness_expected() %>% 
      pivot_longer(cols = c(before_deadline, after_deadline), names_to = "submission_status", 
                   values_to = "submission_count_split")
  })
  
  
  output$timeliness_mean_on_time <- renderText({
    mean_on_time <- round(mean(timeliness_expected()$before_deadline), 2)
    paste("Average number of records submitted on time:",mean_on_time, sep = " ")
  })
  
  output$timeliness_mean_late <- renderText(({
    mean_late <- round(mean(timeliness_expected()$after_deadline),2)
    paste("Average number of records submitted after the deadline:",mean_late, sep = " ")
    
  }))


  output$timeliness_plot <- renderPlotly({

     plot <- ggplot(data=timeliness_submitted()
                   )+
      geom_col(data = timeliness_expected(),
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
      layout(legend = list(x = 1, y = 1))%>%
      layout(legend=list(title=list(text='<b> Legend </b>')))
  })

  #Update the default selection on the data tab to be the same as the bullet chart user selection
  observeEvent(input$timeliness_smr_in,
               updateSelectInput(session, "timeliness_smr_in_2", 
                                 choices = c("(All)", unique(timeliness$smr)[order(unique(timeliness$smr))]),
                                 selected = input$timeliness_smr_in)
  )
  
  observeEvent(input$timeliness_year_in, 
               updateSelectInput(session, "timeliness_year_in_2", 
                                 choices = c("(All)", unique(timeliness$event_year)[order(unique(timeliness$event_year))]),
                                 selected = input$timeliness_year_in)
  )
  
  #filter and select rows to display in the timeliness Data tab
  timeliness_data_smr <- reactive({
    req(input$timeliness_smr_in_2)
    if(input$timeliness_smr_in_2 %in% unique(timeliness$smr)){
      timeliness %>%
        filter(smr == input$timeliness_smr_in_2)
    }
    else{
      timeliness
    }
  })
  
  timeliness_data_year <- reactive({
    req(input$timeliness_year_in_2)
    if(input$timeliness_year_in_2 %in% unique(timeliness_data_smr()$event_year)){
      timeliness_data_smr() %>% 
        filter(event_year == input$timeliness_year_in_2)
    }
    else{
      timeliness_data_smr()
    }
  })
  
  #update list of month choices based on year input
  observeEvent(input$timeliness_month_in,
               updateSelectInput(session, "timeliness_month_in_2", 
                                 choices = c("(All)",unique(timeliness_data_year()$event_month_name)[order(unique(timeliness_data_year()$event_month))]),
                                 selected = input$timeliness_month_in)
  )
  
  
  timeliness_data_month <- reactive({
    req(input$timeliness_month_in_2)
    if(input$timeliness_month_in_2 %in% unique(timeliness_data_year()$event_month_name)){
      timeliness_data_year() %>% 
        filter(event_month_name == input$timeliness_month_in_2) %>% 
        select(smr, hb_name, event_year, event_month_name, before_deadline, after_deadline, expected_submissions)
    }
    else{
      timeliness_data_year()%>% 
      select(smr, hb_name, event_year, event_month_name, before_deadline, after_deadline, expected_submissions)
    }
  })
  

#render final table to display
 output$timeliness_rows <- DT::renderDataTable({ 
   datatable(data = timeliness_data_month(),
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
                    str_to_lower(unique(timeliness_data_month()$event_month_name)), "all_months"),
             ifelse(input$timeliness_year_in_2 %in% unique(timeliness$event_year), 
                    unique(timeliness_data_month()$event_year), "all_years"),
             ".csv")
    },
    content = function(file){
      write.csv(timeliness_data_month(), row.names = FALSE, file)
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
                        choices = c("(All)",unique(filters1()$year)[order(unique(filters1()$year))]),
                        selected = if_else(input$Year %in% c("(All)",unique(filters1()$year)),
                                                             input$Year, "(All)")
                        )
    })
  
  #Update Data Item Name selection based on inputs
  observeEvent(to_listen_audit(), {
      updateSelectInput(session,"DataItemName", 
                        choices = c("(All)",unique(filters1()$data_item_name)[order(unique(filters1()$data_item_name))]),
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
      rename("Healthboard" = "HBName", "Error Count" = "error",
             "Year" = "year", "Percentage" = "percentage") %>% 
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
      rename("Healthboard" = "HBName", "Error Count" = "error",
             "Year" = "year", "Percentage" = "percentage") %>% 
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
        filter(source == "error_3_table", year == input$year3)
    }
    
    else {
      smr02_diabetes %>%
        filter(source == "error_3_table") %>% 
        arrange(desc(year))
    }
  })
  
  error3_data <-  reactive({
    error3_filter() %>%
      rename("Healthboard" = "HBName", "Error Count" = "error",
             "Year" = "year", "Percentage" = "percentage") %>% 
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
      rename("Healthboard" = "HBName", "Error Count" = "error",
             "Year" = "year", "Percentage" = "percentage") %>% 
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
        filter(source == "error_5_table", year == input$year5)
    }
    
    else {
      smr02_diabetes %>%
        filter(source == "error_5_table") %>% 
        arrange(desc(year))
    }
  })
  
  error5_data <- reactive({
    error5_filter() %>%
      rename("Healthboard" = "HBName", "Error Count" = "error",
             "Year" = "year", "Percentage" = "percentage") %>% 
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
        filter(source == "error_6_table", year == input$year6)
    }
    
    else {
      smr02_diabetes %>%
        filter(source == "error_6_table") %>% 
        arrange(desc(year))
    }
  })
  
  error6_data <- reactive({
    error6_filter() %>%
      rename("Healthboard" = "HBName", "Error Count" = "error",
             "Year" = "year", "Percentage" = "percentage") %>% 
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
      rename("Healthboard" = "HBName", "Query Count" = "error",
             "Year" = "year", "Percentage" = "percentage") %>% 
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

})
