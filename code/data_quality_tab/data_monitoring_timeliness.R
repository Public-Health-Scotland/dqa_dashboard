
# Import R libraries ------------------------------------------------------

library(DBI)
library(odbc)

library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(lubridate)


# Functions ---------------------------------------------------------------

count_events_month <- function(data, date_col){
  data %>%
    mutate(event_year = year({{date_col}}), event_month = month({{date_col}}))%>%
    group_by(hbres_currentdate, event_year, event_month) %>% 
    summarise(event_count = n())
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




# Selection criteria -----------------------------------------------------------------
# implementing the different selection criteria for each smr


#smr00 selection criteria
smr00_filt <- smr00_raw%>%
  filter(referral_type != 3,
         clinic_attendance ==1,
         current_trust_dmu %in% c('SAA20','SBA20','SDA02',
                                  'SFA20', 'SGA20', 'SHA20',
                                  'SLA20','SNA20','SRA01',
                                  'STA20','SSA20','SWA01',
                                  'SYA20','SVA20', 'SZA01'),
         !str_detect(location, "^(V|J|K)"),
         !is.na(date_record_inserted)
         )   

#smr01 selection criteria
smr01_filt <- smr01_raw%>%
  filter(current_trust_dmu %in% c('SAA20','SBA20','SDA02',
                                  'SFA20', 'SGA20', 'SHA20',
                                  'SLA20','SNA20','SRA01',
                                  'STA20','SSA20','SWA01',
                                  'SYA20','SVA20', 'SZA01'),
         location != 'D201N',
         !str_detect(location, "^(V|J|K)"),
         !is.na(date_record_inserted)
         )

#smr02 selection criteria
smr02_filt <- smr02_raw%>%
  filter(current_trust_dmu %in% c('SAA20','SBA20','SDA02',
                                  'SFA20', 'SGA20', 'SHA20',
                                  'SLA20','SNA20','SRA01',
                                  'STA20','SSA20','SWA01',
                                  'SYA20','SVA20', 'SZA01'),
         condition_on_discharge == 3,
         location != 'D201N',
         !str_detect(location, "^(V|J|K)"),
         !is.na(date_record_inserted)
         )

#smr04 selection criteria
smr04_filt <- smr04_raw%>%
  filter(current_trust_dmu %in% c('SAA20','SBA20','SDA01',
                                  'SDA02','SFA20', 'SGA20',
                                  'SHA20','SLA20','SNA20',
                                  'SRA01','STA20','SSA20',
                                  'SWA01','SYA20','SVA20', 
                                  'SZA01'),
         location != 'D201N',
         !str_detect(location, "^(V|J|K)"),
         !is.na(date_record_inserted)
         )

# Counts ------------------------------------------------------------------


#count of smr events by month for each HB
smr00_monthly_event <- count_events_month(smr00_filt, clinic_date)
smr01_monthly_event <- count_events_month(smr01_filt, discharge_date)
smr02_monthly_event <- count_events_month(smr02_filt, discharge_date)
smr04_monthly_event <- count_events_month(smr04_filt, discharge_date)


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




smr00_march <- smr00_filt %>%
  filter(year(clinic_date)==2021,
         month(clinic_date)==3,
         date_record_inserted <= sub_deadline[3])%>%
  select(clinic_date, date_record_inserted)

sum(smr00_monthly_event[smr00_monthly_event$event_month==3, 'event_count']$event_count)
