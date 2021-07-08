
# Import R libraries ------------------------------------------------------

library(DBI)
library(odbc)

library(readr)
library(dplyr)
library(tidyr)
library(janitor)
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


# Extract data from SMR analysis views ------------------------------------

#open connection to database
con <- dbConnect(odbc(), dsn = "SMRA", uid = .rs.askForPassword("SMRA Username:"), 
                 pwd = .rs.askForPassword("SMRA Password:"))

smr00_raw <- dbGetQuery(con, "SELECT *
                        FROM analysis.smr00_pi
                        WHERE clinic_date 
                        BETWEEN {d TO_DATE('2021-01-01', 'YYYY-MM-DD')}
                        AND {d TO_DATE('2021-06-30', 'YYYY-MM-DD')};")%>%
              clean_names()

smr01_raw <- dbGetQuery(con, "SELECT *
                        FROM analysis.smr01_pi
                        WHERE discharge_date 
                        BETWEEN {d TO_DATE('2021-01-01', 'YYYY-MM-DD')}
                        AND {d TO_DATE('2021-06-30', 'YYYY-MM-DD')};")%>%
            clean_names()

smr02_raw <- dbGetQuery(con, "SELECT *
                        FROM analysis.smr02_pi
                        WHERE discharge_date 
                        BETWEEN {d TO_DATE('2021-01-01', 'YYYY-MM-DD')}
                        AND {d TO_DATE('2021-06-30', 'YYYY-MM-DD')};")%>%
              clean_names()

smr04_raw <- dbGetQuery(con, "SELECT *
                        FROM analysis.smr04_pi
                        WHERE discharge_date 
                        BETWEEN {d TO_DATE('2021-01-01', 'YYYY-MM-DD')}
                        AND {d TO_DATE('2021-06-30', 'YYYY-MM-DD')};")%>%
              clean_names()



# Count -------------------------------------------------------------------

#count of smr00 events by month for each HB
#fyi: the filters are keeping only new outpatients(referral_type) 
#who were seen(clinic_attendance)

smr00_monthly_event <- smr00_raw %>%
  filter(referral_type != 3, clinic_attendance ==1) %>%      
  mutate(event_year = year(clinic_date), event_month = month(clinic_date))%>%
  group_by(hbres_currentdate, event_year, event_month) %>% 
  summarise(event_count = n())

sum(smr00_monthly_event[smr00_monthly_event$event_month==3, 'event_count']$event_count)

#count of smr00 record submissions by month for each HB
# smr00_monthly_sub <- smr00_raw %>% 
#   mutate(sub_year = year(date_record_inserted), 
#          sub_month = month(date_record_inserted))%>%
#   group_by(hbres_currentdate, sub_year, sub_month)%>%
#   summarise(sub_count = n())

#join the event count and sub count
inner_join(smr00_monthly_event, smr00_monthly_sub)

#seq of first day of every month in 2021
first_day_month <- seq.Date(from = as.Date("2021-01-01 00:00:00.00"),
                length.out = 12,
                by = "month")

#seq of last day of every month in 2021
last_day_month <- lubridate::ceiling_date(first_day_month, "month")-1

#seq of 2021 monthly submission deadlines. 
#fyi: submission deadline for records to be uploaded is 6 weeks from the end
#of the month of the clinic_date (smr00) or discharge_date(smr01, smr02, smr04)

sub_deadline <- last_day_month + 42 #6 weeks is 42 days





smr00_jan <- smr00_raw %>%
  filter(month(clinic_date)==1 & year(clinic_date)==2021)%>%
  select(hbres_currentdate, clinic_date, date_record_inserted)
