library(odbc)       #R library for Open Database Connectivity, used to connect to databases
library(DBI)      #Manage DB connections
library(readr)
library(tidyr)
library(dplyr)
library(janitor)

con <- dbConnect(odbc(), dsn = "SMRA", uid = .rs.askForPassword("SMRA Username:"), 
                 pwd = .rs.askForPassword("SMRA Password:"))

diagnosis1 <- dbGetQuery(con, "SELECT LINK_NO,CIS_MARKER, MAIN_CONDITION
                         FROM analysis.smr01_pi") %>%
              clean_names()

last_episode <- diagnosis1 %>%
  group_by(link_no, cis_marker)%>%
  mutate(epinum = dplyr::row_number(), last_epi = max(epinum))%>% #generate episode number and last_episode
  filter(epinum == last_epi & last_epi > 1) #filter through the last episode record for multi-episode stays (ie. last episode > 1)
  
#saving csv of main condition codes for last episode in mult-episode stays
write_csv(last_episode, here::here("data", "smr01_last_episode.csv"))
