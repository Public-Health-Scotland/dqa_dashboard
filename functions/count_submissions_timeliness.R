count_submissions <- function(data, date_col, deadlines){
  
  df_list <- list()
  months <- data %>%
    distinct(month({{date_col}})) %>% 
    pull()
  
  
  for(i in months){
    submissions <- data %>% 
      mutate(event_year=year({{date_col}}), 
             event_month=month({{date_col}}, label=FALSE),
             event_month_name=month({{date_col}}, label=TRUE))%>%
      group_by(hbres_currentdate, event_year,event_month, event_month_name) %>%
      filter(event_month==i) %>% 
      summarize(
        total_submissions = n(),
        before_deadline = sum(date_record_inserted <= deadlines[i]), 
        after_deadline = sum(date_record_inserted > deadlines[i])
      )%>%
      ungroup()
    
    df_list[[i]] <- submissions
    
  }
  
  return(do.call(rbind,df_list))
}
