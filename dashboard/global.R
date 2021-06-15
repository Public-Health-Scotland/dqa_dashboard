# In the line below, import the shiny library so that it's available
# in both ui.R and server.R
library(readxl)
library(shiny)
library(tidyverse)
library(DT)
library(here)

library(odbc)       #R library for Open Database Connectivity, used to connect to databases
library(RODBC)      #Manage DB connections






here()

# In the lines below, import the files and process them.

SMR_accuracy <- read_csv(here("./smr_data/SMR_accuracy.csv"))

#Check the column names

col_names_smr <- colnames(SMR_accuracy)

#Checking all the values in columns, looking for missing ones:

for (column_name in col_names_smr) {
  print("Unique values in the column:")
  print(column_name)
  print(unique(SMR_accuracy[[column_name]]))
  print("")
}

#There are three different SMRs in this dataset, some missing data in accuracy and seven different year ranges. Check the datatype.

for (column_name in colnames) {
  print(typeof(SMR_accuracy[[column_name]]))
}

#Accuracy is a float, the rest are strings (apart from row number). Delete rows with missing data from the accuracy column:

SMR_acc <- SMR_accuracy %>% filter(!(is.na(Accuracy)))

#Compare dimensions before and after

dim(SMR_accuracy)
dim(SMR_acc)

#27 rows were deleted. Drop irrelevant columns (X1 and DataItemName) and add a mean column. I left both options separately just in case.

SMR_ess <- subset(SMR_acc, select = -c(X1, DataItemName))

SMR_mean <- SMR_ess %>%
  group_by(Year, Audit) %>%
  dplyr::summarise(Mean = round(mean(Accuracy, na.rm = T), digits = 2)
)






### HB and Hospital Site Level data -----------------------------------------


#Import data
hb_path <- here("./smr_data/Hospital_SMR_accuracy_2004-Present.xlsx")

hb_accuracy <- hb_path %>%
  excel_sheets()%>%
  set_names()%>%
  map_df(~read_excel(hb_path, sheet = .x), 
         col_types = c("text", "text", "text", "numeric", "text"), .id = "Healthboard")%>%
  select(c(1:6))

#quick formatting cleanup
hb_accuracy[hb_accuracy$Year == "2004/2006", "Year"] <- "2004-2006"

#Create a new accuracy data frame with a column with mean values grouped by SMR, Year and data item 
hb_mean <- hb_accuracy %>%
  group_by(Audit, Year, DataItemName)%>%
  mutate(MeanAccuracy = round(mean(Accuracy, na.rm=TRUE),2))
hb_mean$Accuracy <- round(hb_mean$Accuracy, 2)




### Coding Discrepancies Data -----------------------------------------------

error_1_table <- diagnosis2 %>%
  group_by(HBName) %>%
  filter(DIABETES == 1) %>%
  mutate(
    error_1 = case_when(
      MAIN_CONDITION == 'O240' | MAIN_CONDITION == 'O241' | MAIN_CONDITION == 'O242' |
        MAIN_CONDITION == 'O243' | OTHER_CONDITION_1 == 'O240' | OTHER_CONDITION_1 == 'O241' | OTHER_CONDITION_1 == 'O242' |
        OTHER_CONDITION_1 == 'O243' | OTHER_CONDITION_2 == 'O240' | OTHER_CONDITION_2 == 'O241' | OTHER_CONDITION_2 == 'O242' |
        OTHER_CONDITION_2 == 'O243' | OTHER_CONDITION_3 == 'O240' | OTHER_CONDITION_3 == 'O241' | OTHER_CONDITION_3 == 'O242' |
        OTHER_CONDITION_3 == 'O243'| OTHER_CONDITION_4 == 'O240' | OTHER_CONDITION_4 == 'O241' | OTHER_CONDITION_4 == 'O242' |
        OTHER_CONDITION_4 == 'O243'| OTHER_CONDITION_5 == 'O240' | OTHER_CONDITION_5 == 'O241' | OTHER_CONDITION_5 == 'O242' |
        OTHER_CONDITION_5 == 'O243' & MAIN_CONDITION != 'O244' & MAIN_CONDITION != 'O249' & OTHER_CONDITION_1 != 'O244' &
        OTHER_CONDITION_1 != 'O249' & OTHER_CONDITION_2 != 'O244' & OTHER_CONDITION_2 != 'O249' & OTHER_CONDITION_3 != 'O244' &
        OTHER_CONDITION_3 != 'O249' & OTHER_CONDITION_4 != 'O244' & OTHER_CONDITION_4 != 'O249' & OTHER_CONDITION_5 != 'O244' &
        OTHER_CONDITION_5 != 'O249' ~ 'no error',
      TRUE ~ 'error 1')
  ) %>%
  summarise(error1 = sum(error_1 == "error 1"), denominator = sum(DIABETES == 1))%>%
  mutate(percentage_1 = round(error1/denominator*100, digits = 2))
