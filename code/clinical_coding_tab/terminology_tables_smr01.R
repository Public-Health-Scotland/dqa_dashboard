library(odbc)       #R library for Open Database Connectivity, used to connect to databases
library(DBI)      #Manage DB connections
library(readr)
library(tidyr)
library(dplyr)
library(janitor)
library(stringr)

con <- dbConnect(odbc(), dsn = "SMRA", uid = .rs.askForPassword("SMRA Username:"), 
                 pwd = .rs.askForPassword("SMRA Password:"))

diagnosis1 <- dbGetQuery(con, "SELECT link_no, cis_marker, main_condition
                         FROM analysis.smr01_pi
                         WHERE date_record_inserted BETWEEN {d TO_DATE('2020-01-01', 'YYYY-MM-DD')} 
                 AND {d TO_DATE('2021-05-31', 'YYYY-MM-DD')};") %>%
  clean_names()

last_episode <- diagnosis1 %>%
  group_by(link_no, cis_marker)%>%
  mutate(epinum = dplyr::row_number(), last_epi = max(epinum))%>% #generate episode number and last_episode
  filter(epinum == last_epi & last_epi > 1) %>% #filter through the last episode record for multi-episode stays (ie. last episode > 1)
  mutate(r_code = case_when(str_detect(main_condition, "^R") ~ 1, 
                            TRUE ~ 0))
