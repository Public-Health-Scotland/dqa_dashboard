################################

## Completeness per data item ##

###############################

#completeness() returns a data frame of completeness percentage per data item
#The function takes a dataframe of raw smr data, and a vector of data item names we want to 
#calculate completeness for.

completeness <- function(smr_data, select_cols){
  
  #format our smr data and select columns we want to calculate completeness for
  df <- smr_data %>%
    select(event_date, hbtreat_currentdate, select_cols) %>%
    mutate(event_month = as.Date(paste0(year(event_date),'-', 
                                       month(event_date), '-01')))%>%
    select(-event_date)
  
  #count the number of NAs for each data item of interest
  na_count <- df%>%
    group_by(hbtreat_currentdate, event_month)%>%
    summarise_all(funs(sum(is.na(.)))) %>%
    pivot_longer(all_of(select_cols), names_to = "data_item", values_to = "na_count") %>% 
    ungroup()
  
  #total number of records per month
  total_records_month <- df%>%
    group_by(hbtreat_currentdate, event_month)%>%
    tally()%>%
    rename("month_total"="n") %>% 
    ungroup()
  
  #calculate completeness percentage per data item
  completeness_df <- na_count %>% 
    left_join(total_records_month)%>%
    mutate(percent_complete_month = round((month_total - na_count)/month_total*100, 
                                          digits = 2))
  
  return(completeness_df)
}
