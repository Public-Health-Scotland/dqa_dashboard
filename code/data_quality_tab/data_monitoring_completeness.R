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

hb_lookup <- rbind(hb2019, hb_other) %>%
  mutate(hb_name = case_when(hb_name == "NHS Greater Glasgow and C" ~ "NHS Greater Glasgow and Clyde",
                             TRUE ~ hb_name))



# Pull and Wrangle data ---------------------------------------------------

         
con <- dbConnect(odbc(), dsn = "SMRA", uid = .rs.askForPassword("SMRA Username:"), 
                 pwd = .rs.askForPassword("SMRA Password:"))

#select  records uploaded over a specific time period
df <- dbGetQuery(con, statement = "SELECT *
                 FROM ANALYSIS.SMR00_PI
                 WHERE DATE_RECORD_INSERTED BETWEEN {d TO_DATE('2021-01-01', 'YYYY-MM-DD')} 
                 AND {d TO_DATE('2021-05-31', 'YYYY-MM-DD')};" )


df <- df %>%
  clean_names()%>%
  select(date_record_inserted, hbres_currentdate, sex, marital_status, ethnic_group, specialty, significant_facility, patient_category,
         referral_type, clinic_attendance, referral_source, attendance_follow_up, 
         mode_of_clinical_interaction)%>%
  left_join(hb_lookup, by = c("hbres_currentdate" = "hb"))%>%
  mutate(year_record_inserted = year(date_record_inserted), month_record_inserted = month(date_record_inserted))%>%
  select(-date_record_inserted)
  


smr00_count <- df%>%
  group_by(hbres_currentdate, hb_name, year_record_inserted, month_record_inserted)%>%
  summarise_all(funs(sum(is.na(.)))) %>%
  pivot_longer(c(5:15), names_to = "data_item", values_to = "na_count")



