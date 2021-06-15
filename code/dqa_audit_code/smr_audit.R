library(readxl)
library(tidyverse)

#path to SMR accuracy audit results
hb_path <- here::here("data", "Hospital_SMR_accuracy_2004-Present.xlsx")

#collate and store smr accuracy in a df
hb_accuracy <- hb_path %>%
  excel_sheets()%>%
  set_names()%>%
  map_df(~read_excel(hb_path, sheet = .x), 
         col_types = c("text", "text", "text", "numeric", "text"), .id = "Healthboard")%>%
  select(c(1:6))
  

