library(odbc)       #R library for Open Database Connectivity, used to connect to databases
library(DBI)      #Manage DB connections
library(readr)
library(tidyr)
library(dplyr)
library(janitor)
library(stringr)

###Open a connection to the SMRA database
con <- dbConnect(odbc(), dsn = "SMRA", uid = .rs.askForPassword("SMRA Username:"), 
                 pwd = .rs.askForPassword("SMRA Password:"))

###Extract the main_condition codes for the desired time period:
#hbres_currentdate condains the patient's health board of treatment
#link_no and cis_marker are used to order Continuous Inpatient Stay (CIS) episodes
#main_condition contains the main ICD10 clinical code for an episode

diagnosis1 <- dbGetQuery(con, "SELECT hbres_currentdate, discharge_date, link_no, cis_marker, main_condition
                         FROM analysis.smr01_pi
                         WHERE discharge_date BETWEEN {d TO_DATE('2017-01-01', 'YYYY-MM-DD')} 
                 AND {d TO_DATE('2021-06-30', 'YYYY-MM-DD')};") %>%
  clean_names()


###Filter the last episode of every multi-episode CIS set 
#(we're looking at sets where there's more than 1 episode and the patient has been transfered)
last_episode <- diagnosis1 %>%
  group_by(hbres_currentdate, link_no, cis_marker)%>%
  mutate(epinum = dplyr::row_number(), last_epi = max(epinum))%>% #generate episode number and last_episode
  filter(epinum == last_epi & last_epi > 1) %>% #filter through the last episode record for multi-episode stays (ie. last episode > 1)
  mutate(r_code = case_when(str_detect(main_condition, "^R") ~ 1, 
                            TRUE ~ 0))
last_episode

#Read in hb_lookup file:
hb_lookup <- read_csv(here::here("lookups", "hb_lookup.csv"))

#append hb names to diagnosis2 dataframe
last_episode1 <- left_join(last_episode, hb_lookup[c(1:2)], by = c("hbres_currentdate"="HB"))
last_episode1
unique(last_episode1$main_condition)

last_episode2 <- last_episode1 %>% 
  mutate(
    year = (substr(discharge_date, 1, 4))
  )

all_multi_episodes <- last_episode2 %>% 
  group_by(HBName, year) %>%
   summarise(n())
  

RCodes <- last_episode2 %>% 
  group_by(HBName, year) %>% 
  filter(r_code == 1) %>%
  filter(!is.na(HBName)) %>% 
  mutate(
    resp_chest = case_when(
      str_detect(main_condition, "^R05") | str_detect(main_condition, "^R06") | str_detect(main_condition, "^R07") ~ 'resp/chest',
      TRUE ~ 'other')) %>% 
  mutate(
    APV = case_when(
      str_detect(main_condition, "^R10") | str_detect(main_condition, "^R11") ~ 'AP&V',
      TRUE ~ 'other'
    )) %>% 
  mutate(
    collapse_convuls = case_when(
      str_detect(main_condition, "^R55") | str_detect(main_condition, "^R56") ~ 'collapse/convuls',
      TRUE ~ 'other')) %>% 
  summarise(
    resp_chest = sum(resp_chest == 'resp/chest'), APV = sum(APV == 'AP&V'), collapse_convuls = sum(collapse_convuls == 'collapse/convuls'), 
    all = sum(r_code == 1) 
  )


RCodes_multi <- left_join(RCodes, all_multi_episodes)

  
write_csv(RCodes_multi, here::here("data", "RCodes.csv"))

 
