shinyServer(function(input, output) {
  
  output$gapminder_table <- renderDataTable({ 
    SMR_mean        
  },
  filter = 'top',
  rownames = FALSE)    
})
