library(odbc)       #R library for Open Database Connectivity, used to connect to databases
library(RODBC)      #Manage DB connections

library(tidyr)
library(dplyr)





# Connect to database and extract data -----------------------------------------------------


con <- dbConnect(odbc(), dsn = "SMRA", uid = .rs.askForPassword("SMRA Username:"), 
                 pwd = .rs.askForPassword("SMRA Password:"))



diagnosis2 <- dbGetQuery(con, "SELECT MAIN_CONDITION, OTHER_CONDITION_1, OTHER_CONDITION_2, OTHER_CONDITION_3, 
                         
                         OTHER_CONDITION_4, OTHER_CONDITION_5, DIABETES, 
                         
                         EPISODE_RECORD_KEY, HBTREAT_CURRENTDATE, LOCATION
                         
                         FROM ANALYSIS.SMR02_PI
                         
                         WHERE CONDITION_ON_DISCHARGE = '3'
                         AND 
                         (DIABETES IN ('1', '2', '3') 
                         OR MAIN_CONDITION LIKE 'O24%'
                         OR OTHER_CONDITION_1 LIKE 'O24%'
                         OR OTHER_CONDITION_2 LIKE 'O24%'
                         OR OTHER_CONDITION_3 LIKE 'O24%'
                         OR OTHER_CONDITION_4 LIKE 'O24%'
                         OR OTHER_CONDITION_5 LIKE 'O24%')")

RODBC::odbcCloseAll() #close all open rodbc connections


#Read in hb_lookup file:
hb_lookup <- read_csv(here::here("lookups", "hb_lookup.csv"))

#append hb names to diagnosis2 dataframe
diagnosis2 <- left_join(diagnosis2, hb_lookup[c(1:2)], by = c("HBTREAT_CURRENTDATE"="HB"))



# Error Counts ------------------------------------------------------------





# Write out tables of error counts per HB ------------------------------------------------------


