
########################################
##      Create change symbol        ##
########################################

#this function takes the dataframe of completeness percentages per data item, and generates
#symbols indicating if the completeness percentage has increased, decreased,
#or stayed the same compared to the previous month

create_change_symbol <- function(data){
#filter current month and previous month data and convert to wide format
data_wide <- data %>%
    select(smr,hbtreat_currentdate,data_item, event_month, percent_complete_month)%>%
    mutate(month_label = case_when(event_month==max(event_month)~"current_month",
                                   event_month==max(event_month)-months(1)~"previous_month",
                                   TRUE~"other")) %>% 
    filter(month_label %in% c("current_month", "previous_month")) %>% 
    pivot_wider(id_cols = c(smr, hbtreat_currentdate, data_item), 
                names_from = month_label, values_from = percent_complete_month,
                names_prefix="completeness_")

#add a change indicator and change symbol variable
change_df <- data_wide %>% 
  mutate(change_flag = 
           case_when(completeness_current_month > completeness_previous_month
                     ~ 2,
                     completeness_current_month < completeness_previous_month
                     ~ 1,
                     TRUE ~ 0)) %>% 
  mutate(change_symbol = case_when(
    change_flag == 1 ~ paste0(icon("arrow-down", lib = "glyphicon"),
                              tags$span(class = "sr-only", "Decrease from previous month"),
                              sep = " "),
    change_flag == 2 ~ paste0(icon("arrow-up", lib = "glyphicon"),
                              tags$span(class = "sr-only", "Increase from previous month"),
                              sep = " "),
    change_flag == 0 ~ paste0(icon("minus", lib = "glyphicon"),
                              tags$span(class = "sr-only", "No change from previous month"),
                              sep = " ")))
return(change_df)
}
