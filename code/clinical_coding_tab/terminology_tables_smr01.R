library(odbc)       #R library for Open Database Connectivity, used to connect to databases
library(dplyr)
library(stringr)
library(janitor)

#set up the connection to the SMRA database 
con <- dbConnect(odbc(), dsn = "SMRA", uid = .rs.askForPassword("SMRA Username:"), 
                 pwd = .rs.askForPassword("SMRA Password:"))


diagnosis <- dbGetQuery(con, "SELECT LINK_NO, CIS_MARKER, MAIN_CONDITION, HBTREAT_CURRENTDATE
                              FROM ANALYSIS.SMR01_PI") %>%
              clean_names()


#converting col names to lowercase
cis_df<- diagnosis %>%
  group_by(link_no, cis_marker)%>%
  mutate(epinum = dplyr::row_number(), last_epi = max(epinum))
 