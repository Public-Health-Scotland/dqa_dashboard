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

map_data <- dbGetQuery(con, "SELECT hbres_currentdate, discharge_date
                         FROM analysis.smr01_pi
                         WHERE discharge_date BETWEEN {d TO_DATE('2020-06-30', 'YYYY-MM-DD')} 
                 AND {d TO_DATE('2021-06-30', 'YYYY-MM-DD')};") %>%
  clean_names()


hb_lookup <- read_csv(here::here("lookups", "hb_lookup.csv"))

map_data <- left_join(map_data, hb_lookup[c(1:2)], by = c("hbres_currentdate"="HB"))
map_data

map_table <- map_data %>% 
  group_by(HBName) %>% 
  filter(!is.na(HBName)) %>% 
  summarise(count = n()) %>% 
  mutate(
    total = nrow(map_data)
    ) %>% 
  mutate(
    percentage = round(count / total *100, digits = 2)
  )
map_table <- map_table[, c('HBName', "percentage")]
map_table

write_csv(map_table, here::here("data", "map_table.csv"))
