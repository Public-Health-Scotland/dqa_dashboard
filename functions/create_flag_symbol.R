######################################

##        Create flag symbol       ##

######################################


#this function takes the dataframe of completeness percentages per data item and generates
#a symbol indicating if the data is within different completeness percentage thresholds
#aim is to flag any data items where completeness is too low.

create_flag_symbol <- function(data){
  data %>%
    
    filter(event_month == month(Sys.Date())-1)%>%
    
    mutate(flag = case_when(percent_complete_month >= 60 ~ 1,
                            percent_complete_month >= 40 ~ 2,
                            percent_complete_month < 40 ~ 3)
    )%>%
    
    filter(!is.na(flag))%>%
    
    mutate(flag_symbol = case_when(
      flag == 1 ~ str_c(icon("ok", lib = "glyphicon"),
                        tags$span(class = "sr-only", "Above 60% complete"),
                        sep = " "),
      flag == 2 ~ str_c(icon("warning-sign", lib = "glyphicon"),
                        tags$span(class = "sr-only", "Between 40% and 60% complete"),
                        sep = " "),
      flag == 3 ~ str_c(icon("flag", lib = "glyphicon"),
                        tags$span(class = "sr-only", "Below 40% complete"),
                        sep = " ")
                        )
    )
}
