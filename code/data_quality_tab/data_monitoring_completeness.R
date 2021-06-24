# library(RODBC)

library(DBI)
library(odbc)

library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(purrr)
library(lubridate)


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

#select  records uploaded over a specific time period
df <- dbGetQuery(con, statement = "SELECT *
                 FROM ANALYSIS.SMR00_PI
                 WHERE DATE_RECORD_INSERTED BETWEEN {d TO_DATE('2021-01-01', 'YYYY-MM-DD')} 
                 AND {d TO_DATE('2021-05-31', 'YYYY-MM-DD')};" )%>%
  clean_names()

#vector of names of the SMR00 data items we want to monitor
smr00_cols <- c("dob", "sex", "marital_status", "postcode","ethnic_group", "gp_practice_code", "main_condition",
                "significant_facility", "referral_source", "mode_of_clinical_interaction", "referral_type", "specialty")


#select the data items, add a column of HB names and split the dates by year and month
df <- df %>%
  select(date_record_inserted, hbres_currentdate, smr00_cols)%>%
  left_join(hb_lookup, by = c("hbres_currentdate" = "hb"))%>%
  mutate(year_record_inserted = year(date_record_inserted), month_record_inserted = month(date_record_inserted))%>%
  select(-date_record_inserted)
  
#generate a count of NA values by data item. The count is grouped by health board, year and month.
smr00_count <- df%>%
  group_by(hb_name, year_record_inserted, month_record_inserted)%>%
  summarise_all(funs(sum(is.na(.)))) %>%
  pivot_longer(all_of(smr00_cols), names_to = "data_item", values_to = "na_count")

#generate a count of the total number of records submitted by healthboard, year and month
smr00_total <- df%>%
  group_by(hb_name, year_record_inserted, month_record_inserted)%>%
  tally()%>%
  rename("month_total"="n")


#vector containing names of the SMR00 mandatory data items
mandatory_smr00 <- c("dob", "sex", "postcode", "ethnic_group", "gp_practice_code", 
                     "mode_of_clinical_interaction", "referral_type", "specialty")

#join the NA counts and total counts in a new dataframe and derive the percentage complete per data item
#add a an indicator variable for mandatory data items
smr00_full <- smr00_count %>%
  left_join(smr00_total) %>%
  mutate(percent_complete_month = round((month_total - na_count)/month_total*100, digits = 2)) %>%
  mutate(month_record_inserted = case_when(month_record_inserted == 1 ~ "January",
                                           month_record_inserted == 2 ~ "February",
                                           month_record_inserted == 3 ~ "March",
                                           month_record_inserted == 4 ~ "April",
                                           month_record_inserted == 5 ~ "May")) %>%
  mutate(mandatory = case_when(data_item %in% mandatory_smr00 ~ "mandatory", TRUE ~ "not mandatory"))


# Write the Output --------------------------------------------------------

#write out the output so that it can be imported in global.R
write_csv(smr00_full, here::here("data", "smr_completeness.csv"))

