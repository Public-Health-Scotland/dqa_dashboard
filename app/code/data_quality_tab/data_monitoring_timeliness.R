# libraries are loaded by setup_environment file

# library(DBI)
# library(odbc)

# library(readr)
# library(dplyr)
# library(tidyr)
# library(janitor)
# library(lubridate)
# library(stringr)

# Load Functions ----------------------------------------------------------

# source(here::here("functions", "append_source.R"))
# source(here::here("functions", "count_submissions_timeliness.R"))


# Import lookup ----------------------------------------------------------

hb2019 <- read_csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/652ff726-e676-4a20-abda-435b98dd7bdc/download/hb14_hb19.csv")%>%
  select(HB, HBName)%>%
  clean_names() %>% 
  rbind(c("S08100008", "State Hospital")) #adding the state hosp code to the lookup


# Extract data from SMR analysis views ------------------------------------

#open connection to database, uncomment code below if needed
# con <- dbConnect(odbc(), dsn = "SMRA", uid = .rs.askForPassword("SMRA Username:"),
#                  pwd = .rs.askForPassword("SMRA Password:"))

smr00_raw <- dbGetQuery(con, "SELECT referral_type, clinic_attendance, current_trust_dmu,
                        location, date_record_inserted, clinic_date, hbtreat_currentdate
                        FROM analysis.smr00_pi
                        WHERE clinic_date BETWEEN {d to_date('2019-12-01', 'YYYY-MM-DD')}
                        AND {d to_date('2021-11-30', 'YYYY-MM-DD')};")%>%
              clean_names()

smr01_raw <- dbGetQuery(con, "SELECT current_trust_dmu,
                        location, date_record_inserted, discharge_date, hbtreat_currentdate
                        FROM analysis.smr01_pi
                        WHERE discharge_date BETWEEN {d to_date('2019-12-01', 'YYYY-MM-DD')}
                        AND {d to_date('2021-11-30', 'YYYY-MM-DD')};")%>%
            clean_names()

smr02_raw <- dbGetQuery(con, "SELECT current_trust_dmu,
                        location, date_record_inserted, condition_on_discharge,
                        discharge_date, hbtreat_currentdate
                        FROM analysis.smr02_pi
                        WHERE discharge_date BETWEEN {d to_date('2019-12-01', 'YYYY-MM-DD')}
                        AND {d to_date('2021-11-30', 'YYYY-MM-DD')};")%>%
              clean_names()

smr04_raw <- dbGetQuery(con, "SELECT current_trust_dmu,
                        location, date_record_inserted, discharge_date, hbtreat_currentdate
                        FROM analysis.smr04_pi
                        WHERE discharge_date BETWEEN {d to_date('2019-12-01', 'YYYY-MM-DD')}
                        AND {d to_date('2021-11-30', 'YYYY-MM-DD')};")%>%
              clean_names()

# Selection criteria -----------------------------------------------------------------
# implements the different selection criteria for each smr

#Geo codes we want to exclude (eg. treated outwith scotland, Non-NHS providers, etc)
geo_excl <- c("S08200001", "S08200002", "S08200003", "S08200004", "S27000001", 
              "S27000002", "S08100001") 

#empty list with 4 slots to store each filtered dataframe
ldf <- vector(mode = "list", length = 4)

#smr00 selection criteria
ldf[[1]] <- smr00_raw%>%
  filter(referral_type != 3,
         clinic_attendance ==1,
         current_trust_dmu %in% c('SAA20', 'SBA20', 'SDA01',
                                  'SDA02', 'SFA20', 'SGA20',
                                  'SHA20', 'SLA20', 'SNA20',
                                  'SRA01', 'SSA20', 'STA20',
                                  'SVA20', 'SWA01', 'SYA20',
                                  'SZA01'),
         !str_detect(location, "(V|J|K)$"),
         !is.na(date_record_inserted),
         !(hbtreat_currentdate %in% geo_excl)
         ) %>% 
  rename("event_date" = "clinic_date") %>% 
  mutate(smr = "SMR00")

#smr01 selection criteria
ldf[[2]] <- smr01_raw%>%
  filter(current_trust_dmu %in% c('SAA20','SBA20','SDA02',
                                  'SFA20', 'SGA20', 'SHA20',
                                  'SLA20','SNA20','SRA01',
                                  'STA20','SSA20','SWA01',
                                  'SYA20','SVA20', 'SZA01'),
         location != 'D201N',
         !str_detect(location, "(V|J|K)$"),
         !is.na(date_record_inserted),
         !(hbtreat_currentdate %in% geo_excl)
         ) %>% 
  rename("event_date" = "discharge_date")%>% 
  mutate(smr = "SMR01")

#smr02 selection criteria
ldf[[3]] <- smr02_raw%>%
  filter(current_trust_dmu %in% c('SAA20','SBA20','SDA02',
                                  'SFA20', 'SGA20', 'SHA20',
                                  'SLA20','SNA20','SRA01',
                                  'STA20','SSA20','SWA01',
                                  'SYA20','SVA20', 'SZA01'),
         condition_on_discharge == 3,
         location != 'D201N',
         !str_detect(location, "(V|J|K)$"),
         !is.na(date_record_inserted),
         !(hbtreat_currentdate %in% geo_excl)
         )%>% 
  rename("event_date" = "discharge_date")%>% 
  mutate(smr = "SMR02")

#smr04 selection criteria
ldf[[4]] <- smr04_raw %>%
  filter(current_trust_dmu %in% c('SAA20','SBA20','SDA01',
                                  'SDA02','SFA20', 'SGA20',
                                  'SHA20','SLA20','SNA20',
                                  'SRA01','STA20','SSA20',
                                  'SWA01','SYA20','SVA20', 
                                  'SZA01'),
         location != 'D201N',
         !str_detect(location, "(V|J|K)$"),
         !is.na(date_record_inserted),
         !(hbtreat_currentdate %in% geo_excl)
         ) %>% 
  rename("event_date" = "discharge_date") %>% 
  mutate(smr = "SMR04")

# Count Submissions ------------------------------------------

# The submission dedline is 6 weeks following the end of month of discharge/transfer/death or clinic attendance 

#iterate through each dataframe and add a submission deadline column to the data
for(i in 1:length(ldf)){
  ldf[[i]] <- ldf[[i]] %>% 
    mutate(last_day_month = lubridate::ceiling_date(event_date, "month")-1) %>% 
    mutate(sub_deadline = last_day_month+ lubridate::days(42))
}

# count_submissions() calculates the number of records submitted before and after the deadline
# outputs a dataframe with counts for each SMR, HB and month
submissions <- count_submissions(ldf) %>% 
  left_join(hb2019, by = c("hbtreat_currentdate" = "hb"))


# Expected submissions & backlog ------------------------------------------

expected_submissions_df <- read_csv(
  here::here("data", "expected_submissions.csv"))

timeliness <- submissions %>% 
  left_join(expected_submissions_df, by = c("smr", "hb_name", "event_year", "event_month"))

write_csv(timeliness, here::here("data", "timeliness.csv"))

