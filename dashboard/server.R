shinyServer(function(input, output) {
  
  output$error_1 <- renderDataTable({ 
    error_1_table       
  },
  filter = 'top',
  rownames = FALSE)    


   output$error_2 <- renderDataTable({ 
     error_2_table       
   },
   filter = 'top',
   rownames = FALSE)    
  
 
   output$error_3 <- renderDataTable({ 
     error_3_table       
   },
   filter = 'top',
   rownames = FALSE)    
 
 
   output$error_4 <- renderDataTable({ 
     error_4_table       
   },
   filter = 'top',
   rownames = FALSE)    
 
 
   output$error_5 <- renderDataTable({ 
     error_5_table       
   },
   filter = 'top',
   rownames = FALSE)    
 
   output$error_6 <- renderDataTable({ 
   error_6_table       
   },
   filter = 'top',
   rownames = FALSE)    
 })