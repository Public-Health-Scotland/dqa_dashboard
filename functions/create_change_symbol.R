
########################################
##      Create change symbol        ##
########################################

#this function takes the dataframe of completeness percentages per data item, and generates
#symbols indicating if the completeness percentage has increased, decreased,
#or stayed the same compared to the previous month

create_change_symbol <- function(data){
  
  data %>%
    filter(event_month == month(Sys.Date())-1 | event_month == month(Sys.Date())-2)%>%
    mutate(change = case_when(percent_complete_month > lag(percent_complete_month) ~
                                2,
                              percent_complete_month < lag(percent_complete_month) ~
                                1,
                              percent_complete_month == lag(percent_complete_month) ~
                                0)
    )%>%
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
