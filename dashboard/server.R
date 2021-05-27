shinyServer(function(input, output) {
  
  output$gapminder_table <- renderDataTable({ 
    hb_mean       
  },
  filter = 'top',
  rownames = FALSE)    
})
