# library(RODBC)

library(DBI)
library(odbc)

library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(purrr)
library(lubridate)



# Load Function(s) --------------------------------------------------------

################################

## Completeness per data item ##

###############################

#completeness() returns a data frame of completeness percentage per data item
#The function takes raw smr data, and a vector of data item names we want to calculate completeness for.

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

#########################################

## Bind data frames and append source ##

########################################


#append_source() takes a vector of names of df you want to rbind and appends a source column with the name of the dfs
append_source <- function(df_names) {
  do.call(rbind, lapply(df_names, function(x) {
    cbind(get(x), source = x)
  }))
}



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


create_flag_symbol <- function(data){
  data %>%
    filter(event_month == month(Sys.Date())-1)%>%
    mutate(flag = case_when(percent_complete_month >= 60 ~ 1,
                            percent_complete_month >= 40 ~ 2,
                            percent_complete_month < 40 ~ 3)
           )%>%
    filter(!is.na(flag))%>%
    mutate(flag_symbol = case_when(
      flag == 1 ~ str_c(icon("smile", lib = "glyphicon"),
                  tags$span(class = "sr-only", "Above 60% complete"),
                  sep = " "),
      flag == 2 ~ str_c(icon("meh", lib = "glyphicon"),
                       tags$span(class = "sr-only", "Between 40% and 60% complete"),
                       sep = " "),
      flag == 3 ~ str_c(icon("frown", lib = "glyphicon"),
                       tags$span(class = "sr-only", "Below 40% complete"),
                       sep = " ")
                      )
           )
}

# Import lookup ----------------------------------------------------------


hb2019 <- read_csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/652ff726-e676-4a20-abda-435b98dd7bdc/download/hb14_hb19.csv")%>%
  select(HB, HBName)%>%
  clean_names()

hb_other <- as.data.frame(rbind(c("S08200001", "England/Wales/Northern Ireland"),
                  c("S08200002", "No Fixed Abode"),
                  c("S08200003", "Not Known"),
                  c("S08200004", "Outside U.K."))) %>%
    rename("hb"="V1", "hb_name"="V2")

hb_lookup <- rbind(hb2019, hb_other)



# Pull and Wrangle data ---------------------------------------------------

         
con <- dbConnect(odbc(), dsn = "SMRA", uid = .rs.askForPassword("SMRA Username:"), 
                 pwd = .rs.askForPassword("SMRA Password:"))

#select records  over a specific time period from smr00, smr01, smr02 and smr04

#smr00 contains outpatient data so the records are filtered by clinic date
raw_smr00 <- dbGetQuery(con, statement = "SELECT *
                 FROM ANALYSIS.SMR00_PI
                 WHERE clinic_date BETWEEN {d TO_DATE('2021-01-01', 'YYYY-MM-DD')} 
                 AND {d TO_DATE('2021-06-30', 'YYYY-MM-DD')};" )%>%
  clean_names()%>%
  rename("event_date"="clinic_date")

#for the remaining datasets smr01, 02 and 04, the data is filtered by discharge date
raw_smr01 <- dbGetQuery(con, statement = "SELECT *
                 FROM ANALYSIS.SMR01_PI
                 WHERE discharge_date BETWEEN {d TO_DATE('2021-01-01', 'YYYY-MM-DD')} 
                 AND {d TO_DATE('2021-06-30', 'YYYY-MM-DD')};" )%>%
  clean_names() %>%
  rename("event_date"="discharge_date")

raw_smr02 <- dbGetQuery(con, statement = "SELECT *
                 FROM ANALYSIS.SMR02_PI
           WHERE discharge_date BETWEEN {d TO_DATE('2021-01-01', 'YYYY-MM-DD')} 
           AND {d TO_DATE('2021-06-30', 'YYYY-MM-DD')};" )%>%
  clean_names() %>% 
  rename("event_date"="discharge_date")


raw_smr04 <- dbGetQuery(con, statement = "SELECT *
                 FROM ANALYSIS.SMR04_PI
                 WHERE discharge_date BETWEEN {d TO_DATE('2021-01-01', 'YYYY-MM-DD')} 
                 AND {d TO_DATE('2021-06-30', 'YYYY-MM-DD')};" )%>%
  clean_names() %>% 
  rename("event_date"="discharge_date")




#vectors of names of the SMR data items we want to monitor completeness for

#list of data items for smr00
smr00_cols <- c("dob", "sex", "postcode", "ethnic_group", "main_operation",
                     "mode_of_clinical_interaction", "referral_type", "specialty")

#list of data items for smr01 and smr04
smr01_cols <- c("dob", "sex", "postcode", "ethnic_group", "admission_type",
                     "significant_facility","admission_transfer_from", 
                     "discharge_transfer_to", "management_of_patient", "specialty")

smr02_cols <- c("dob", "sex", "postcode", "ethnic_group", "admission_type",
                "admission_transfer_from", "discharge_transfer_to", 
                "management_of_patient", "specialty",
                "condition_on_discharge")

smr04_cols <- c("dob", "sex", "postcode", "ethnic_group", "admission_type",
                "admission_transfer_from", 
                "discharge_transfer_to", "management_of_patient", "specialty")

#Output completeness for each smr

smr00_completeness <- completeness(raw_smr00, smr00_cols)
smr01_completeness <- completeness(raw_smr01, smr01_cols)
smr02_completeness <- completeness(raw_smr02, smr02_cols)
smr04_completeness <- completeness(raw_smr04, smr04_cols)

df_names <- c("smr00_completeness", "smr01_completeness", "smr02_completeness", "smr04_completeness")


#Bind and format completeness data frame

smr_completeness <- append_source(df_names)%>%
  left_join(hb_lookup, by = c("hbres_currentdate" = "hb")) %>%
  mutate(month_name =
           recode_factor(as.factor(event_month),
                         `1`="January", `2`="February", `3`="March", `4`="April",
                         `5`="May", `6`="June", `7`="July", `8`="August",
                         `9`="September", `10`="October", `11`="November", `12`="December"),
         source = case_when(source=="smr00_completeness" ~ "SMR00", 
                            source=="smr01_completeness" ~ "SMR01", 
                            source == "smr02_completeness" ~ "SMR02", 
                            source == "smr04_completeness" ~ "SMR04"))%>%
  rename("smr"="source")


# Generate sparkline plots for % completeness over previous months
# plots are stored in html and will be executed by the shiny app

completeness_plots <- smr_completeness %>%
  group_by(smr, hb_name, data_item, event_year)%>%
  arrange(event_month)%>%
  summarise(mini_plot = spk_chr(percent_complete_month,
                                height = "40px",
                                width = 100,
                                chartRangeMin = 0,
                                chartRangeMax = max(percent_complete_month),
                                type="bar",
                                numberDigitGroupSep = "",
                                barWidth = 10,
                                barSpacing = 2,
                                tooltipFormat = paste0('{{value}}', '%')
                                )
            )

change_df <- create_change_symbol(smr_completeness)
flag_df <- create_flag_symbol(smr_completeness)

#Join the plots to the main table and filter through last month's figures only
smr_completeness_2 <- smr_completeness %>%
  filter(event_month == month(Sys.Date())-1) %>% 
  left_join(completeness_plots) %>%
  left_join(change_df)%>%
  left_join(flag_df)


# Write the Output --------------------------------------------------------

#write out the output so that it can be imported in global.R
write_csv(smr_completeness_2, here::here("data", "smr_completeness.csv"))

