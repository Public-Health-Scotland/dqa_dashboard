
# Import R libraries ------------------------------------------------------

library(DBI)
library(odbc)

library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(lubridate)
library(stringr)


# Load Functions ----------------------------------------------------------

source(here::here("functions", "append_source.R"))
source(here::here("functions", "count_submissions_timeliness.R"))


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
         current_trust_dmu %in% c('SAA20', 'SBA20', 'SDA01',
                                  'SDA02', 'SFA20', 'SGA20',
                                  'SHA20', 'SLA20', 'SNA20',
                                  'SRA01', 'SSA20', 'STA20',
                                  'SVA20', 'SWA01', 'SYA20',
                                  'SZA01'),
         !str_detect(location, "(V|J|K)$"), # if location doesnt end with V, J, or K include it.
         !is.na(date_record_inserted) #date when record recieved
         )   

#smr01 selection criteria
smr01_filt <- smr01_raw%>%
  filter(current_trust_dmu %in% c('SAA20','SBA20','SDA02',
                                  'SFA20', 'SGA20', 'SHA20',
                                  'SLA20','SNA20','SRA01',
                                  'STA20','SSA20','SWA01',
                                  'SYA20','SVA20', 'SZA01'),
         location != 'D201N',
         !str_detect(location, "(V|J|K)$"),
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
         !str_detect(location, "(V|J|K)$"),
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
         !str_detect(location, "(V|J|K)$"),
         !is.na(date_record_inserted)
         )

# Calculate submission deadline dates ------------------------------------------


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




# Submission counts -------------------------------------------------------


submissions_smr00 <- count_submissions(smr00_filt, clinic_date, 
                                       sub_deadline)
submissions_smr01 <- count_submissions(smr01_filt, discharge_date,
                                       sub_deadline)
submissions_smr02<- count_submissions(smr02_filt, discharge_date,
                                      sub_deadline)
submissions_smr04 <- count_submissions(smr04_filt, discharge_date,
                                       sub_deadline)

df_names <- c("submissions_smr00", "submissions_smr01",
              "submissions_smr02", "submissions_smr04")

submissions <- append_source(df_names)%>%
  mutate(source = case_when( source == "submissions_smr00" ~ "SMR00",
                             source == "submissions_smr01" ~ "SMR01",
                             source == "submissions_smr02" ~ "SMR02",
                             TRUE ~ "SMR04")
         )%>%
  rename("smr" = "source")%>%
  left_join(hb_lookup, by = c("hbres_currentdate"="hb"))


# Expected submissions & backlog ------------------------------------------

expected_submissions_df <- read_csv(here::here("data", "expected_submissions.csv"))

timeliness <- submissions %>% 
  left_join(expected_submissions_df) %>%
  mutate(diff_obs_exp = total_submissions - expected_submissions,
         percent_on_time = on_time/expected_submissions*100,
         percent_complete = total_submissions/expected_submissions*100
         )%>%
  ungroup()

write_csv(timeliness, here::here("data", "timeliness.csv"))