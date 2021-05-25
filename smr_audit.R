library(readxl)
library(tidyverse)

#path to SMR accuracy audit results
path <- "smr_data/Hospital_SMR_accuracy_2004-Present.xlsx"

#collate and store smr accuracy in a df
accuracy <- path %>%
  excel_sheets()%>%
  set_names()%>%
  map_df(~read_excel(path, sheet = .x), 
         col_types = c("text", "text", "text", "numeric", "text"), .id = "Healthboard")%>%
  select(c(1:6))
  

