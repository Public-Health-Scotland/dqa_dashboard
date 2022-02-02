count_submissions <- function(smr_data){

  sub_list <- vector(mode = "list", length = 4)
  
  for(i in 1:length(sub_list)){
    sub_list[[i]] <- smr_data[[i]] %>% 
      mutate(event_year = lubridate::year(event_date), 
             event_month = lubridate::month(event_date, label=FALSE),
             event_month_name = lubridate::month(event_date, label=TRUE))%>%
      group_by(smr, hbtreat_currentdate, event_year,event_month, event_month_name) %>%
      summarize(
        total_submissions = n(),
        before_deadline = sum(date_record_inserted <= sub_deadline), 
        after_deadline = sum(date_record_inserted > sub_deadline)
      )%>%
      ungroup()
    
  }
  
  return(do.call(rbind,sub_list))
}
