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

#completeness() returns a data frame of completeness percentage per data item
#The function takes raw smr data,
#a vector of data item names we want to calculate completeness for, 
#and a vector of the mandatory data item names.

completeness <- function(smr_data, select_cols, list_mandatory){
  
  #format our smr data and select columns we want to calculate completeness for
  df <- smr_data %>%
    select(date_record_inserted, hbres_currentdate, select_cols) %>%
    mutate(year_record_inserted = year(date_record_inserted), 
           month_record_inserted = month(date_record_inserted))%>%
    select(-date_record_inserted)
  
  #count the number of NAs for each data item of interest
  na_count <- df%>%
    group_by(hbres_currentdate, year_record_inserted, month_record_inserted)%>%
    summarise_all(funs(sum(is.na(.)))) %>%
    pivot_longer(all_of(select_cols), names_to = "data_item", values_to = "na_count")
  
  #total number of records per month
  total_records_month <- df%>%
    group_by(hbres_currentdate, year_record_inserted, month_record_inserted)%>%
    tally()%>%
    rename("month_total"="n")
  
  #calculate completeness percentage per data item
  completeness_df <- na_count %>% 
    left_join(total_records_month)%>%
    mutate(percent_complete_month = round((month_total - na_count)/month_total*100, digits = 2),
           mandatory = case_when(data_item %in% mandatory_smr00 ~ "mandatory", TRUE ~ "not mandatory"))
  
  return(completeness_df)
}



#append_source() takes a vector of names of df you want to rbind and appends a source column with the name of the dfs
append_source <- function(df_names) {
  do.call(rbind, lapply(df_names, function(x) {
    cbind(get(x), source = x)
  }))
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

#select records uploaded over a specific time period from smr00, smr01, smr02 and smr04
raw_smr00 <- dbGetQuery(con, statement = "SELECT *
                 FROM ANALYSIS.SMR00_PI
                 WHERE DATE_RECORD_INSERTED BETWEEN {d TO_DATE('2021-01-01', 'YYYY-MM-DD')} 
                 AND {d TO_DATE('2021-05-31', 'YYYY-MM-DD')};" )%>%
  clean_names()

raw_smr01 <- dbGetQuery(con, statement = "SELECT *
                 FROM ANALYSIS.SMR01_PI
                 WHERE DATE_RECORD_INSERTED BETWEEN {d TO_DATE('2021-01-01', 'YYYY-MM-DD')} 
                 AND {d TO_DATE('2021-05-31', 'YYYY-MM-DD')};" )%>%
  clean_names()

raw_smr02 <- dbGetQuery(con, statement = "SELECT *
                 FROM ANALYSIS.SMR02_PI
           WHERE DATE_RECORD_INSERTED BETWEEN {d TO_DATE('2021-01-01', 'YYYY-MM-DD')} 
           AND {d TO_DATE('2021-05-31', 'YYYY-MM-DD')};" )%>%
  clean_names()

raw_smr04 <- dbGetQuery(con, statement = "SELECT *
                 FROM ANALYSIS.SMR04_PI
                 WHERE DATE_RECORD_INSERTED BETWEEN {d TO_DATE('2021-01-01', 'YYYY-MM-DD')} 
                 AND {d TO_DATE('2021-05-31', 'YYYY-MM-DD')};" )%>%
  clean_names()



#vectors of names of the SMR data items we want to monitor completeness for
smr00_cols <- c("dob", "sex", "postcode","ethnic_group", "main_operation",
                "significant_facility", "referral_source", "mode_of_clinical_interaction", "referral_type", "specialty")

smr01_cols <- c("dob", "sex", "postcode", "ethnic_group", "significant_facility",
                "admission_type", "admission_transfer_from",
                "admission_transfer_from_loc", "discharge_transfer_to", 
                "discharge_transfer_to_location", "management_of_patient",
                "specialty")

smr02_cols <- c("dob", "sex", "postcode", "ethnic_group", "condition_on_discharge",
                "admission_type", "admission_transfer_from",
                "admission_transfer_from_loc", "discharge_transfer_to", 
                "discharge_transfer_to_location", "management_of_patient",
                "specialty")

smr04_cols <- c("dob", "sex", "postcode", "ethnic_group",
                "admission_type", "admission_transfer_from",
                "admission_transfer_from_loc", "discharge_transfer_to", 
                "discharge_transfer_to_location", "management_of_patient",
                "specialty")


#vectors of the data items that are mandatory out of the ones we've selected to monitor

#list of mandatory data items for smr00
mandatory_smr00 <- c("dob", "sex", "postcode", "ethnic_group", 
                     "mode_of_clinical_interaction", "referral_type", "specialty")

#list of mandatory items for smr01, smr02, and smr04
mandatory_other <- c("dob", "sex", "postcode", "ethnic_group", "admission_type",
                     "significant_facility","admission_transfer_from", 
                     "discharge_transfer_to", "management_of_patient", "specialty",
                     "condition_on_discharge")


#Output completeness for each smr

smr00_completeness <- completeness(raw_smr00, smr00_cols, mandatory_smr00)
smr01_completeness <- completeness(raw_smr01, smr01_cols, mandatory_other)
smr02_completeness <- completeness(raw_smr02, smr02_cols, mandatory_other)
smr04_completeness <- completeness(raw_smr04, smr04_cols, mandatory_other)

df_names <- c("smr00_completeness", "smr01_completeness", "smr02_completeness", "smr04_completeness")


#Bind and format final completeness data frame

smr_completeness <- append_source(df_names)%>%
  left_join(hb_lookup, by = c("hbres_currentdate" = "hb")) %>%
  mutate(month_record_inserted =
           recode_factor(as.factor(month_record_inserted),
                         `1`="January", `2`="February", `3`="March", `4`="April",
                         `5`="May", `6`="June", `7`="July", `8`="August",
                         `9`="September", `10`="October", `11`="November", `12`="December"),
         source = case_when(source=="smr00_completeness" ~ "SMR00", 
                            source=="smr01_completeness" ~ "SMR01", 
                            source == "smr02_completeness" ~ "SMR02", 
                            source == "smr04_completeness" ~ "SMR04"))%>%
  rename("smr"="source")

# Write the Output --------------------------------------------------------

#write out the output so that it can be imported in global.R
write_csv(smr_completeness, here::here("data", "smr_completeness.csv"))

