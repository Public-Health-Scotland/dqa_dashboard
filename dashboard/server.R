# shinyServer(function(input, output, session) {
#   saved_choices <- reactiveVal(value = choices)
#   
#   # Reset button rendering (only shown when there are differences to default)
#   output$reset_button <- renderUI({
#     if (!identical(choices, input$checkboxes)) {
#       actionButton(
#         inputId = 'reset',
#         label = 'Reset'
#       )
#     }
#   })
#   
#   # Functionality of reset button
#   observeEvent(input$reset, updatePrettyCheckboxGroup(session, 'checkboxes', selected = choices))
#   
#   # Save button rendering
#   output$save_button <- renderUI({
#     status <- ifelse(identical(input$checkboxes, saved_choices()), 'light', 'primary')
#     actionButton(
#       inputId = 'save',
#       label = 'Save',
#       class = paste0('btn btn-', status)
#     )
#   })
#   
#   # Functionality of save button
#   observeEvent(input$save, saved_choices(input$checkboxes))
#   
#   # Set label for dropdown depending on selected news types
#   observeEvent(saved_choices(), {
#     label <- saved_choices() %>%  when(
#       all(choices %in% .) ~ 'all healthboards selected',
#       length(.) == 0 ~ 'no healthboards selected',
#       TRUE ~ paste(length(.), 'healthboards selected')
#     )
#     updateActionButton(session, inputId = 'dropdown', label = label)
#   }, ignoreInit = FALSE, ignoreNULL = FALSE)
#   
#   # Save settings when dropdown is closed
#   observeEvent(input$dropdown_state, {
#     if (!input$dropdown_state) {
#       if (!identical(saved_choices(), input$checkboxes)) {
#         saved_choices(input$checkboxes)
#       }
#     }
#   })
#   
#   output$table <- renderDT({
#     hb_mean %>%
#       rownames_to_column(var = 'HB') %>%
#       filter(Healthboard %in% saved_choices()) %>%
#       datatable()
#   })
# })



#old code
  #output$gapminder_table <- renderDataTable({ 
    #hb_mean       
  #},
  #filter = 'top',
  #rownames = FALSE)    
#})





###Russell tip
shinyServer(function(input, output, session){

  output_Healthboard <- output %>% unique(hb_mean$Healthboard)
  output_Hospital <- output %>% unique(hb_mean$Hospital)


  # Get user selection
  output$output_Healthboard <- renderUI({
    shinyWidgets::pickerInput("output_Healthboard", 
                              label = "Select Healthboard",
                              choices = unique(hb_mean$Healthboard),
                              selected = "Greater Glasgow & Clyde")
    
  })
  
  output$output_Hospital <- renderUI({
    shinyWidgets::pickerInput("output_Hospital", 
                              label = "Select Hospital",
                              choices = unique(hb_mean$Hospital))
    
  })

  # Create a filtered dataset
  output$table <- reactive({
    hb_mean %>%
      filter(Healthboard %in% input$output_Healthboard
             & Hospital %in% input$output_Hospital)
  })

})

