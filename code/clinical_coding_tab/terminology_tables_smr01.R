library(readr)
library(tidyr)
library(dplyr)
library(janitor)
library(stringr)

###Extract patient treatment data for the last episode of multi-episode Continuous Inpatient Stays(CIS)
#hbtreat_currentdate is the patient's health board of treatment
#link_no and cis_marker are used to order Continuous Inpatient Stay (CIS) episodes
#main_condition contains the main ICD10 clinical code for an episode

last_episode <- dbGetQuery(con,
"WITH
cis_data AS(
  SELECT
    hbtreat_currentdate, discharge_date, link_no, cis_marker, main_condition,
    ROW_NUMBER() OVER (PARTITION BY link_no, cis_marker ORDER BY link_no, cis_marker,
    admission_date, record_type, discharge_date, admission, discharge, uri) AS epinum
  FROM
    analysis.smr01_pi
  WHERE
    discharge_date BETWEEN {d to_date('2017-01-01', 'YYYY-MM-DD')} AND SYSDATE)
SELECT *
FROM 
  (SELECT hbtreat_currentdate, discharge_date, link_no, cis_marker, main_condition, epinum,
    MAX(epinum) OVER (PARTITION BY link_no, cis_marker) AS last_epi
  FROM cis_data
  WHERE epinum >1
  )
WHERE epinum = last_epi") %>%
  clean_names()

#Read in hb_lookup file:
hb_lookup <- read_csv(here::here("lookups", "hb_lookup.csv"))

#append hb names, create flag column for R codes in main condition, create year column for discharge year 
last_episode1 <- left_join(last_episode, hb_lookup[c(1:2)], 
                           by = c("hbtreat_currentdate"="HB")) %>% 
  mutate(r_code = if_else(str_detect(main_condition, "^R"), 1, 0),
         year = (substr(discharge_date, 1, 4)))

all_multi_episodes <- last_episode1 %>% 
  group_by(HBName, year) %>%
  summarise(n()) %>% 
  ungroup()

#summarise R code counts by clinical code groupings 
RCodes <- last_episode1 %>% 
  group_by(HBName, year) %>% 
  filter(r_code == 1, !is.na(HBName)) %>%
  mutate(group = case_when(
    str_detect(main_condition, "^(R05|R06|R07)") ~ "resp/chest",
    str_detect(main_condition, c("^(R10|R11)")) ~ "AP&V",
    str_detect(main_condition, c("^(R55|R56)")) ~ "collapse/convuls",
    TRUE ~ "other"
  )) %>% 
  summarise(resp_chest = sum(group == "resp/chest"), APV = sum(group == "AP&V"),
            collapse_convuls = sum(group == 'collapse/convuls'), all = sum(r_code == 1))

RCodes_multi <- left_join(RCodes, all_multi_episodes)

write_csv(RCodes_multi, here::here("data", "r_codes.csv"))
