library(odbc)       #R library for Open Database Connectivity, used to connect to databases
library(DBI)      #Manage DB connections
library(readr)
library(tidyr)
library(dplyr)
library(janitor)
library(stringr)

con <- dbConnect(odbc(), dsn = "SMRA", uid = .rs.askForPassword("SMRA Username:"), 
                 pwd = .rs.askForPassword("SMRA Password:"))

diagnosis1 <- dbGetQuery(con, "SELECT hbres_currentdate, link_no, cis_marker, main_condition
                         FROM analysis.smr01_pi
                         WHERE date_record_inserted BETWEEN {d TO_DATE('2020-01-01', 'YYYY-MM-DD')} 
                 AND {d TO_DATE('2021-05-31', 'YYYY-MM-DD')};") %>%
  clean_names()

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

RCodes <- last_episode1 %>% 
  group_by(HBName) %>% 
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
    resp_chest = sum(resp_chest == 'resp/chest'), APV = sum(APV == 'AP&V'), collapse_convuls = sum(collapse_convuls == 'collapse/convuls')
  )

RCodes

write_csv(RCodes, here::here("data", "RCodes.csv"))

 