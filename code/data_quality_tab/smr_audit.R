library(openxlsx)
library(readxl)   #needed to use excel_sheets() function
library(readr)
library(janitor)

library(tidyverse)




# National(Scotland) SMR Audit data --------------------------------------

scot_accuracy <- read_csv(here::here("data", "Scotland_SMR_accuracy.csv"))%>%

  clean_names()%>%
  rename("accuracy_scotland"="accuracy")%>%
  mutate(year = case_when( year == "2004/06" ~"2004-2006", 
                           year == "2010/11" ~ "2010-2011", 
                           year == "2014/15" ~ "2014-2015", 
                           year == "2019/20" ~ "2019-2020",
                           year == "2017/18" ~ "2017-2018",
                           year == "2008/09" ~ "2008-2009", 
                           year == "2015/16" ~ "2015-2016"))



# Hospital Site SMR Audit data --------------------------------------------


#path to SMR accuracy audit results
hb_path <- here::here("data", "Hospital_SMR_accuracy_2004-Present.xlsx")

#collate and store smr accuracy in a df
hb_accuracy <- hb_path %>%
  excel_sheets()%>%
  set_names()%>%
  map_df(~read_excel(hb_path, sheet = .x), 
         col_types = c("text", "text", "text", "numeric", "text"), .id = "Healthboard")%>%
  select(c(1:6))%>%
  clean_names()%>%
  mutate(year = case_when(year == "2004/2006"~ "2004-2006", TRUE ~ year))%>%
  rename("accuracy_hospital"="accuracy")


# Join hospital site data with national data for output -------------------

col_order <- c("audit","year","healthboard","hospital","data_item_name","accuracy_hospital","accuracy_scotland")

dashboard_data <- left_join(hb_accuracy, scot_accuracy, by=c("data_item_name", "year", "audit"))%>%
  select(all_of(col_order))


# Write data to the data folder ----------------
#the data will then be read into the global.R script that can be found the app folder.

write_csv(dashboard_data, here::here("data", "dashboard_smr_audit_data.csv"))

