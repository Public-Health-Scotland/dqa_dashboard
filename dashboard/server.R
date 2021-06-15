shinyServer(function(input, output) {
  
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
   


 })