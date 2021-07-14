
########################################
##      Create change symbol        ##
########################################

#this function takes the dataframe of completeness percentages per data item, and generates
#symbols indicating if the completeness percentage has increased, decreased,
#or stayed the same compared to the previous month

create_change_symbol <- function(data){
  
  data %>%
      select(smr,hb_name,event_year,data_item, event_month, percent_complete_month)%>%
      filter(event_month == month(Sys.Date())-1 | event_month == month(Sys.Date())-2)%>%
      arrange(event_month) %>% 
      pivot_wider(names_from = event_month,
                  values_from = percent_complete_month)%>%
      rename(lag1 = 7, lag2 = 6)%>%
      mutate(change = case_when(lag1 > lag2 ~ 2,
                                lag1 < lag2 ~ 1,
                                TRUE ~0)
      ) %>% 
    filter(!is.na(change))%>%
    mutate(change_symbol = case_when(
      change == 1 ~ str_c(icon("arrow-down", lib = "glyphicon"),
                          tags$span(class = "sr-only", "Decrease from previous month"),
                          sep = " "),
      change == 2 ~ str_c(icon("arrow-up", lib = "glyphicon"),
                          tags$span(class = "sr-only", "Increase from previous month"),
                          sep = " "),
      change == 0 ~ str_c(icon("minus", lib = "glyphicon"),
                          tags$span(class = "sr-only", "No change from previous month"),
                          sep = " ")
                          )
          )
  
}
