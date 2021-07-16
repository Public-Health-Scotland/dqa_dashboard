################################

## Completeness per data item ##

###############################

#completeness() returns a data frame of completeness percentage per data item
#The function takes a dataframe of raw smr data, and a vector of data item names we want to 
#calculate completeness for.

completeness <- function(smr_data, select_cols){
  
  #format our smr data and select columns we want to calculate completeness for
  df <- smr_data %>%
    select(event_date, hbres_currentdate, select_cols) %>%
    mutate(event_year = year(event_date), 
           event_month = month(event_date))%>%
    select(-event_date)
  
  #count the number of NAs for each data item of interest
  na_count <- df%>%
    group_by(hbres_currentdate, event_year, event_month)%>%
    summarise_all(funs(sum(is.na(.)))) %>%
    pivot_longer(all_of(select_cols), names_to = "data_item", values_to = "na_count")
  
  #total number of records per month
  total_records_month <- df%>%
    group_by(hbres_currentdate, event_year, event_month)%>%
    tally()%>%
    rename("month_total"="n")
  
  #calculate completeness percentage per data item
  completeness_df <- na_count %>% 
    left_join(total_records_month)%>%
    mutate(percent_complete_month = round((month_total - na_count)/month_total*100, digits = 2))
  
  return(completeness_df)
}
