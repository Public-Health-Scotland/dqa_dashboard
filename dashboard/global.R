# In the line below, import the shiny library so that it's available
# in both ui.R and server.R

library(readxl)
library(shiny)
library(tidyverse)
library(DT)

library(odbc)       #R library for Open Database Connectivity, used to connect to databases
library(RODBC)      #Manage DB connections

library(leaflet)    #both libraries necessary for creating maps
library(rgdal)
# In the lines below, import the files and process them.


SMR_accuracy <- read_csv("SMR_accuracy.csv")
glimpse(SMR_accuracy)

#Check the column names

colnames <- colnames(SMR_accuracy)
colnames

#Checking all the values in columns, looking for missing ones:

for (column_name in colnames) {
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

#set directory to dashboard folder
# setwd("~/dqa_dashboard/dashboard")

#Import data
# hb_path <- "~/dqa_dashboard/smr_data/Hospital_SMR_accuracy_2004-Present.xlsx"

# hb_accuracy <- hb_path %>%
#   excel_sheets()%>%
#   set_names()%>%
#   map_df(~read_excel(hb_path, sheet = .x), 
#          col_types = c("text", "text", "text", "numeric", "text"), .id = "Healthboard")%>%
#   select(c(1:6))

#quick summary of hb_accuracy layout
#keeping NA values for now, but can be removed with na.omit() if needed
# glimpse(hb_accuracy)
# colnames(hb_accuracy)
# lapply(hb_accuracy, unique)
# 
# #quick formatting cleanup
# hb_accuracy[hb_accuracy$Year == "2004/2006", "Year"] <- "2004-2006"
# 
# #Create a new accuracy data frame with a column with mean values grouped by SMR, Year and data item 
# hb_mean <- hb_accuracy %>%
#   group_by(Audit, Year, DataItemName)%>%
#   mutate(MeanAccuracy = round(mean(Accuracy, na.rm=TRUE),2))
# hb_mean$Accuracy <- round(hb_mean$Accuracy, 2)




### Coding Discrepancies Data -----------------------------------------------


con <- dbConnect(odbc(), dsn = "SMRA", uid = .rs.askForPassword("SMRA Username:"), 
                 pwd = .rs.askForPassword("SMRA Password:"))


# ##SMR01
# odbcPreviewObject(con, table="ANALYSIS.SMR01_PI", rowLimit=0)
# 
# diagnosis1 <- dbGetQuery(con, "SELECT MAIN_CONDITION,DISCHARGE_TYPE, DISCHARGE_TRANSFER_TO_LOCATION, CIS_MARKER FROM ANALYSIS.SMR01_PI" )




## SMR02


#select diagnosis records where diabetes during pregnancy has either been clinically coded or hard coded, and where the baby was delivered (ie. condition on discharge==3)
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



glimpse(diagnosis2) #there should be at least one 024% code or a diabetes value in (1,2,3) in each row

unique(diagnosis2$DIABETES) #we should have only numerical values or NAs

unique(diagnosis2$HBTREAT_CURRENTDATE) #HB_CURRENTDATE is the HB of treatment
unique(diagnosis2$LOCATION) #only a handful of unique location codes for our data


#Read in hb_lookup file:
hb_lookup <- read_csv("copyoflookups/hb_lookup.csv")

#append hb names to diagnosis2 dataframe
diagnosis2 <- left_join(diagnosis2, hb_lookup[c(1:2)], by = c("HBTREAT_CURRENTDATE"="HB"))

glimpse(diagnosis2)
diagnosis2$HBName <- as.factor(diagnosis2$HBName)

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
  filter(error_1 == "error 1") %>%
  summarise(error1 = n()) %>%
  mutate(percentage_1 = round(error1/nrow(filter(diagnosis2, DIABETES == 1))*100, digits = 2))
error_1_table

error_2_table <- diagnosis2 %>%
  group_by(HBName) %>%
  filter(DIABETES == 2) %>%
  mutate(
    error_2 = case_when(
      MAIN_CONDITION == 'O244' | OTHER_CONDITION_1 == 'O244' | OTHER_CONDITION_2 == 'O244' |
        OTHER_CONDITION_3 == 'O244' | OTHER_CONDITION_4 == 'O244' | OTHER_CONDITION_5 == 'O244' ~ 'no error',
      TRUE ~ 'error 2')
  ) %>%
  filter(error_2 == "error 2")%>%
  summarise(error2 = n())%>%
  mutate(percentage_2 = round(error2/nrow(filter(diagnosis2, DIABETES == 2))*100, digits = 2))
error_2_table

error_3_table <- diagnosis2 %>%
  group_by(HBName) %>%
  filter(DIABETES == 3) %>%
  mutate(
    error_3 = case_when(
      MAIN_CONDITION == 'O249' | OTHER_CONDITION_1 == 'O249' | OTHER_CONDITION_2 == 'O249' |
        OTHER_CONDITION_3 == 'O249' | OTHER_CONDITION_4 == 'O249' | OTHER_CONDITION_5 == 'O249' ~ 'no error',
      TRUE ~ 'error 3')
  ) %>%
  filter(error_3 == "error 3")%>%
  summarise(error3 = n())%>%
  mutate(percentage_3 = round(error3/nrow(filter(diagnosis2, DIABETES == 3))*100, digits = 2))
error_3_table


error_4_table <- diagnosis2 %>%
  group_by(HBName) %>%
  filter(DIABETES == 4) %>%
  mutate(
    error_4 = case_when(
      MAIN_CONDITION == 'O240' | MAIN_CONDITION == 'O241' | MAIN_CONDITION == 'O242' | MAIN_CONDITION == 'O243' | MAIN_CONDITION == 'O244' |
      MAIN_CONDITION == 'O245' | MAIN_CONDITION == 'O246' | MAIN_CONDITION == 'O247' | MAIN_CONDITION == 'O248' | MAIN_CONDITION == 'O249' |
      OTHER_CONDITION_1 == 'O240' | OTHER_CONDITION_1 == 'O241' | OTHER_CONDITION_1 == 'O242' & OTHER_CONDITION_1 == 'O243' | OTHER_CONDITION_1 == 'O244' |
      OTHER_CONDITION_1 == 'O245' | OTHER_CONDITION_1 == 'O246' | OTHER_CONDITION_1 == 'O247' & OTHER_CONDITION_1 == 'O248' | OTHER_CONDITION_1 == 'O249' |
      OTHER_CONDITION_2 =='O240' | OTHER_CONDITION_2 == 'O241' | OTHER_CONDITION_2 == 'O242' & OTHER_CONDITION_2 == 'O243' | OTHER_CONDITION_2 == 'O244' |
      OTHER_CONDITION_2 == 'O245' | OTHER_CONDITION_2 == 'O246' | OTHER_CONDITION_2 == 'O247' & OTHER_CONDITION_2 == 'O248' | OTHER_CONDITION_2 == 'O249' |
      OTHER_CONDITION_3 == 'O240' | OTHER_CONDITION_3 == 'O241' | OTHER_CONDITION_3 == 'O242' & OTHER_CONDITION_3 == 'O243' | OTHER_CONDITION_3 == 'O244' |
      OTHER_CONDITION_3 == 'O245' | OTHER_CONDITION_3 == 'O246' | OTHER_CONDITION_3 == 'O247' & OTHER_CONDITION_3 == 'O248' | OTHER_CONDITION_3 == 'O249' |
      OTHER_CONDITION_4 == 'O240' | OTHER_CONDITION_4 == 'O241' | OTHER_CONDITION_4 == 'O242' & OTHER_CONDITION_4 == 'O243' | OTHER_CONDITION_4 == 'O244' |
      OTHER_CONDITION_4 == 'O245' | OTHER_CONDITION_4 == 'O246' | OTHER_CONDITION_4 == 'O247' & OTHER_CONDITION_4 == 'O248' | OTHER_CONDITION_4 == 'O249' |
      OTHER_CONDITION_5 == 'O240' | OTHER_CONDITION_5 == 'O241' | OTHER_CONDITION_5 == 'O242' & OTHER_CONDITION_5 == 'O243' | OTHER_CONDITION_5 == 'O244' |
      OTHER_CONDITION_5 == 'O245' | OTHER_CONDITION_5 == 'O246' | OTHER_CONDITION_5 == 'O247' & OTHER_CONDITION_5 == 'O248' | OTHER_CONDITION_5 == 'O249' ~ 'no error',
    T ~ 'error 4')
    ) %>%
  filter(error_4 == "error 4")%>%
  summarise(error4 = n())%>%
  mutate(percentage_4 = round(error4/nrow(filter(diagnosis2, DIABETES == 4))*100, digits = 2))
error_4_table

error_5_table <- diagnosis2 %>%
  group_by(HBName) %>%
  mutate(
    error_5 = case_when(
      !is.na(DIABETES) ~ 'no error',
      TRUE ~ 'error 5')
  ) %>%
  filter(error_5 == "error 5")%>%
  summarise(error5 = n())%>%
  mutate(percentage_5 = round(error5/nrow(diagnosis2)*100, digits = 2))
error_5_table

error_6_table <- diagnosis2 %>%
  group_by(HBName) %>%
  mutate(
    error_6 = case_when(
      MAIN_CONDITION == 'E100' | MAIN_CONDITION == 'E101' | MAIN_CONDITION == 'E102' | MAIN_CONDITION == 'E103' | MAIN_CONDITION == 'E104' |
        MAIN_CONDITION == 'E105' | MAIN_CONDITION == 'E106' | MAIN_CONDITION == 'E107' | MAIN_CONDITION == 'E108' | MAIN_CONDITION == 'E109' |
        OTHER_CONDITION_1 == 'E100' | OTHER_CONDITION_1 == 'E101' | OTHER_CONDITION_1 == 'E102' & OTHER_CONDITION_1 == 'E103' | OTHER_CONDITION_1 == 'E104' |
        OTHER_CONDITION_1 == 'E105' | OTHER_CONDITION_1 == 'E106' | OTHER_CONDITION_1 == 'E107' & OTHER_CONDITION_1 == 'E108' | OTHER_CONDITION_1 == 'E109' |
        OTHER_CONDITION_2 =='E100' | OTHER_CONDITION_2 == 'E101' | OTHER_CONDITION_2 == 'E102' & OTHER_CONDITION_2 == 'E103' | OTHER_CONDITION_2 == 'E104' |
        OTHER_CONDITION_2 == 'E105' | OTHER_CONDITION_2 == 'E106' | OTHER_CONDITION_2 == 'E107' & OTHER_CONDITION_2 == 'E108' | OTHER_CONDITION_2 == 'E109' |
        OTHER_CONDITION_3 == 'E100' | OTHER_CONDITION_3 == 'E101' | OTHER_CONDITION_3 == 'E102' & OTHER_CONDITION_3 == 'E103' | OTHER_CONDITION_3 == 'E104' |
        OTHER_CONDITION_3 == 'E105' | OTHER_CONDITION_3 == 'E106' | OTHER_CONDITION_3 == 'E107' & OTHER_CONDITION_3 == 'E108' | OTHER_CONDITION_3 == 'E109' |
        OTHER_CONDITION_4 == 'E100' | OTHER_CONDITION_4 == 'E101' | OTHER_CONDITION_4 == 'E102' & OTHER_CONDITION_4 == 'E103' | OTHER_CONDITION_4 == 'E104' |
        OTHER_CONDITION_4 == 'E105' | OTHER_CONDITION_4 == 'E106' | OTHER_CONDITION_4 == 'E107' & OTHER_CONDITION_4 == 'E108' | OTHER_CONDITION_4 == 'E109' |
        OTHER_CONDITION_5 == 'E100' | OTHER_CONDITION_5 == 'E101' | OTHER_CONDITION_5 == 'E102' & OTHER_CONDITION_5 == 'E103' | OTHER_CONDITION_5 == 'E104' |
        OTHER_CONDITION_5 == 'E105' | OTHER_CONDITION_5 == 'E106' | OTHER_CONDITION_5 == 'E107' & OTHER_CONDITION_5 == 'E108' | OTHER_CONDITION_5 == 'E109' |
        MAIN_CONDITION == 'E110' | MAIN_CONDITION == 'E111' | MAIN_CONDITION == 'E112' | MAIN_CONDITION == 'E113' | MAIN_CONDITION == 'E114' |
        MAIN_CONDITION == 'E115' | MAIN_CONDITION == 'E116' | MAIN_CONDITION == 'E117' | MAIN_CONDITION == 'E118' | MAIN_CONDITION == 'E119' |
        OTHER_CONDITION_1 == 'E110' | OTHER_CONDITION_1 == 'E111' | OTHER_CONDITION_1 == 'E112' & OTHER_CONDITION_1 == 'E113' | OTHER_CONDITION_1 == 'E114' |
        OTHER_CONDITION_1 == 'E115' | OTHER_CONDITION_1 == 'E116' | OTHER_CONDITION_1 == 'E117' & OTHER_CONDITION_1 == 'E118' | OTHER_CONDITION_1 == 'E119' |
        OTHER_CONDITION_2 =='E110' | OTHER_CONDITION_2 == 'E111' | OTHER_CONDITION_2 == 'E112' & OTHER_CONDITION_2 == 'E113' | OTHER_CONDITION_2 == 'E114' |
        OTHER_CONDITION_2 == 'E115' | OTHER_CONDITION_2 == 'E116' | OTHER_CONDITION_2 == 'E117' & OTHER_CONDITION_2 == 'E118' | OTHER_CONDITION_2 == 'E119' |
        OTHER_CONDITION_3 == 'E110' | OTHER_CONDITION_3 == 'E111' | OTHER_CONDITION_3 == 'E112' & OTHER_CONDITION_3 == 'E113' | OTHER_CONDITION_3 == 'E114' |
        OTHER_CONDITION_3 == 'E115' | OTHER_CONDITION_3 == 'E116' | OTHER_CONDITION_3 == 'E117' & OTHER_CONDITION_3 == 'E118' | OTHER_CONDITION_3 == 'E119' |
        OTHER_CONDITION_4 == 'E110' | OTHER_CONDITION_4 == 'E111' | OTHER_CONDITION_4 == 'E112' & OTHER_CONDITION_4 == 'E113' | OTHER_CONDITION_4 == 'E114' |
        OTHER_CONDITION_4 == 'E115' | OTHER_CONDITION_4 == 'E116' | OTHER_CONDITION_4 == 'E117' & OTHER_CONDITION_4 == 'E118' | OTHER_CONDITION_4 == 'E119' |
        OTHER_CONDITION_5 == 'E110' | OTHER_CONDITION_5 == 'E111' | OTHER_CONDITION_5 == 'E112' & OTHER_CONDITION_5 == 'E113' | OTHER_CONDITION_5 == 'E114' |
        OTHER_CONDITION_5 == 'E115' | OTHER_CONDITION_5 == 'E116' | OTHER_CONDITION_5 == 'E117' & OTHER_CONDITION_5 == 'E118' | OTHER_CONDITION_5 == 'E119' |
        MAIN_CONDITION == 'E120' | MAIN_CONDITION == 'E121' | MAIN_CONDITION == 'E122' | MAIN_CONDITION == 'E123' | MAIN_CONDITION == 'E124' |
        MAIN_CONDITION == 'E125' | MAIN_CONDITION == 'E126' | MAIN_CONDITION == 'E127' | MAIN_CONDITION == 'E128' | MAIN_CONDITION == 'E129' |
        OTHER_CONDITION_1 == 'E120' | OTHER_CONDITION_1 == 'E121' | OTHER_CONDITION_1 == 'E122' & OTHER_CONDITION_1 == 'E123' | OTHER_CONDITION_1 == 'E124' |
        OTHER_CONDITION_1 == 'E125' | OTHER_CONDITION_1 == 'E126' | OTHER_CONDITION_1 == 'E127' & OTHER_CONDITION_1 == 'E128' | OTHER_CONDITION_1 == 'E129' |
        OTHER_CONDITION_2 =='E120' | OTHER_CONDITION_2 == 'E121' | OTHER_CONDITION_2 == 'E122' & OTHER_CONDITION_2 == 'E123' | OTHER_CONDITION_2 == 'E124' |
        OTHER_CONDITION_2 == 'E125' | OTHER_CONDITION_2 == 'E126' | OTHER_CONDITION_2 == 'E127' & OTHER_CONDITION_2 == 'E128' | OTHER_CONDITION_2 == 'E129' |
        OTHER_CONDITION_3 == 'E120' | OTHER_CONDITION_3 == 'E121' | OTHER_CONDITION_3 == 'E122' & OTHER_CONDITION_3 == 'E123' | OTHER_CONDITION_3 == 'E124' |
        OTHER_CONDITION_3 == 'E125' | OTHER_CONDITION_3 == 'E126' | OTHER_CONDITION_3 == 'E127' & OTHER_CONDITION_3 == 'E128' | OTHER_CONDITION_3 == 'E129' |
        OTHER_CONDITION_4 == 'E120' | OTHER_CONDITION_4 == 'E121' | OTHER_CONDITION_4 == 'E122' & OTHER_CONDITION_4 == 'E123' | OTHER_CONDITION_4 == 'E124' |
        OTHER_CONDITION_4 == 'E125' | OTHER_CONDITION_4 == 'E126' | OTHER_CONDITION_4 == 'E127' & OTHER_CONDITION_4 == 'E128' | OTHER_CONDITION_4 == 'E129' |
        OTHER_CONDITION_5 == 'E120' | OTHER_CONDITION_5 == 'E121' | OTHER_CONDITION_5 == 'E122' & OTHER_CONDITION_5 == 'E123' | OTHER_CONDITION_5 == 'E124' |
        OTHER_CONDITION_5 == 'E125' | OTHER_CONDITION_5 == 'E126' | OTHER_CONDITION_5 == 'E127' & OTHER_CONDITION_5 == 'E128' | OTHER_CONDITION_5 == 'E129' |
        MAIN_CONDITION == 'E130' | MAIN_CONDITION == 'E131' | MAIN_CONDITION == 'E132' | MAIN_CONDITION == 'E133' | MAIN_CONDITION == 'E134' |
        MAIN_CONDITION == 'E135' | MAIN_CONDITION == 'E136' | MAIN_CONDITION == 'E137' | MAIN_CONDITION == 'E138' | MAIN_CONDITION == 'E139' |
        OTHER_CONDITION_1 == 'E130' | OTHER_CONDITION_1 == 'E131' | OTHER_CONDITION_1 == 'E132' & OTHER_CONDITION_1 == 'E133' | OTHER_CONDITION_1 == 'E134' |
        OTHER_CONDITION_1 == 'E135' | OTHER_CONDITION_1 == 'E136' | OTHER_CONDITION_1 == 'E137' & OTHER_CONDITION_1 == 'E138' | OTHER_CONDITION_1 == 'E139' |
        OTHER_CONDITION_2 =='E130' | OTHER_CONDITION_2 == 'E131' | OTHER_CONDITION_2 == 'E132' & OTHER_CONDITION_2 == 'E133' | OTHER_CONDITION_2 == 'E134' |
        OTHER_CONDITION_2 == 'E135' | OTHER_CONDITION_2 == 'E136' | OTHER_CONDITION_2 == 'E137' & OTHER_CONDITION_2 == 'E138' | OTHER_CONDITION_2 == 'E139' |
        OTHER_CONDITION_3 == 'E130' | OTHER_CONDITION_3 == 'E131' | OTHER_CONDITION_3 == 'E132' & OTHER_CONDITION_3 == 'E133' | OTHER_CONDITION_3 == 'E134' |
        OTHER_CONDITION_3 == 'E135' | OTHER_CONDITION_3 == 'E136' | OTHER_CONDITION_3 == 'E137' & OTHER_CONDITION_3 == 'E138' | OTHER_CONDITION_3 == 'E139' |
        OTHER_CONDITION_4 == 'E130' | OTHER_CONDITION_4 == 'E131' | OTHER_CONDITION_4 == 'E132' & OTHER_CONDITION_4 == 'E133' | OTHER_CONDITION_4 == 'E134' |
        OTHER_CONDITION_4 == 'E135' | OTHER_CONDITION_4 == 'E136' | OTHER_CONDITION_4 == 'E137' & OTHER_CONDITION_4 == 'E138' | OTHER_CONDITION_4 == 'E139' |
        OTHER_CONDITION_5 == 'E130' | OTHER_CONDITION_5 == 'E131' | OTHER_CONDITION_5 == 'E132' & OTHER_CONDITION_5 == 'E133' | OTHER_CONDITION_5 == 'E134' |
        OTHER_CONDITION_5 == 'E135' | OTHER_CONDITION_5 == 'E136' | OTHER_CONDITION_5 == 'E137' & OTHER_CONDITION_5 == 'E138' | OTHER_CONDITION_5 == 'E139' |
        MAIN_CONDITION == 'E140' | MAIN_CONDITION == 'E141' | MAIN_CONDITION == 'E142' | MAIN_CONDITION == 'E143' | MAIN_CONDITION == 'E144' |
        MAIN_CONDITION == 'E145' | MAIN_CONDITION == 'E146' | MAIN_CONDITION == 'E147' | MAIN_CONDITION == 'E148' | MAIN_CONDITION == 'E149' |
        OTHER_CONDITION_1 == 'E140' | OTHER_CONDITION_1 == 'E141' | OTHER_CONDITION_1 == 'E142' & OTHER_CONDITION_1 == 'E143' | OTHER_CONDITION_1 == 'E144' |
        OTHER_CONDITION_1 == 'E145' | OTHER_CONDITION_1 == 'E146' | OTHER_CONDITION_1 == 'E147' & OTHER_CONDITION_1 == 'E148' | OTHER_CONDITION_1 == 'E149' |
        OTHER_CONDITION_2 =='E140' | OTHER_CONDITION_2 == 'E141' | OTHER_CONDITION_2 == 'E142' & OTHER_CONDITION_2 == 'E143' | OTHER_CONDITION_2 == 'E144' |
        OTHER_CONDITION_2 == 'E145' | OTHER_CONDITION_2 == 'E146' | OTHER_CONDITION_2 == 'E147' & OTHER_CONDITION_2 == 'E148' | OTHER_CONDITION_2 == 'E149' |
        OTHER_CONDITION_3 == 'E140' | OTHER_CONDITION_3 == 'E141' | OTHER_CONDITION_3 == 'E142' & OTHER_CONDITION_3 == 'E143' | OTHER_CONDITION_3 == 'E144' |
        OTHER_CONDITION_3 == 'E145' | OTHER_CONDITION_3 == 'E146' | OTHER_CONDITION_3 == 'E147' & OTHER_CONDITION_3 == 'E148' | OTHER_CONDITION_3 == 'E149' |
        OTHER_CONDITION_4 == 'E140' | OTHER_CONDITION_4 == 'E141' | OTHER_CONDITION_4 == 'E142' & OTHER_CONDITION_4 == 'E143' | OTHER_CONDITION_4 == 'E144' |
        OTHER_CONDITION_4 == 'E145' | OTHER_CONDITION_4 == 'E146' | OTHER_CONDITION_4 == 'E147' & OTHER_CONDITION_4 == 'E148' | OTHER_CONDITION_4 == 'E149' |
        OTHER_CONDITION_5 == 'E140' | OTHER_CONDITION_5 == 'E141' | OTHER_CONDITION_5 == 'E142' & OTHER_CONDITION_5 == 'E143' | OTHER_CONDITION_5 == 'E144' |
        OTHER_CONDITION_5 == 'E145' | OTHER_CONDITION_5 == 'E146' | OTHER_CONDITION_5 == 'E147' & OTHER_CONDITION_5 == 'E148' | OTHER_CONDITION_5 == 'E149' ~ 'error 6',
      T ~ 'no error')
  ) %>%
  filter(error_6 == "error 6")%>%
  summarise(error6 = n())%>%
  mutate(percentage_6 = round(error6/nrow(diagnosis2)*100, digits = 2))
error_6_table

#MAPS

#read in the shapefile downloaded from the web and add the healthboard borders (polygons)
ShapeFile = readOGR(dsn=("~/dashboard/dqa_dashboard/dashboard/map_files/HBShapefile.shp"), layer="HBShapefile")
ShapeFile <- spTransform(ShapeFile, CRS("+init=epsg:4326"))
leaflet() %>%
  addPolygons(data = ShapeFile)

ShapeFile@data <- ShapeFile@data %>%
  rownames_to_column(var = "ID") %>% # Change row names to be an ID column
  mutate(HBName = paste0("NHS ", HBName)) # add the NHS prefix to the names of the healthboards so they are uniform with the data files
ShapeFile@data <- ShapeFile@data %>%
     left_join(error_1_table) %>% #add each table one by one
  left_join(error_2_table) %>%
  left_join(error_3_table) %>%
  left_join(error_4_table) %>%
  left_join(error_5_table) %>%
  left_join(error_6_table) 
colourpal1 <- colorNumeric("RdPu", domain = ShapeFile@data$percentage_1) #create a colour palette for each map, can be simplified
colourpal2 <- colorNumeric("RdPu", domain = ShapeFile@data$percentage_2)
colourpal3 <- colorNumeric("RdPu", domain = ShapeFile@data$percentage_3)
colourpal4 <- colorNumeric("RdPu", domain = ShapeFile@data$percentage_4)
colourpal5 <- colorNumeric("RdPu", domain = ShapeFile@data$percentage_5)
colourpal6 <- colorNumeric("RdPu", domain = ShapeFile@data$percentage_6)


error1map <- leaflet(ShapeFile) %>%
  addPolygons(fillColor = ~colourpal1(percentage_1), # our colour palette function
              fillOpacity = 0.7, # the opacity of the fill colour
              color = "#2e2e30", # colour of shape outlines
              weight = 2) %>% # thickness of the shape outlines
  addLegend("bottomright", pal = colourpal1, values = ~percentage_1,
            title = "Coding discrepancy 1",
            labFormat = labelFormat(suffix = " %"), #add the percentage suffix
            opacity = 1)

error2map <- leaflet(ShapeFile) %>%
  addPolygons(fillColor = ~colourpal2(percentage_2), # our colour palette function
              fillOpacity = 0.7, # the opacity of the fill colour
              color = "#2e2e30", # colour of shape outlines
              weight = 2) %>% # thickness of the shape outlines
  addLegend("bottomright", pal = colourpal2, values = ~percentage_2,
            title = "Coding discrepancy 2",
            labFormat = labelFormat(suffix = " %"),
            opacity = 1)

error3map <- leaflet(ShapeFile) %>%
  addPolygons(fillColor = ~colourpal3(percentage_3), # our colour palette function
              fillOpacity = 0.7, # the opacity of the fill colour
              color = "#2e2e30", # colour of shape outlines
              weight = 2) %>% # thickness of the shape outlines
  addLegend("bottomright", pal = colourpal3, values = ~percentage_3,
            title = "Coding discrepancy 3",
            labFormat = labelFormat(suffix = " %"),
            opacity = 1)

error4map <- leaflet(ShapeFile) %>%
  addPolygons(fillColor = ~colourpal4(percentage_4), # our colour palette function
              fillOpacity = 0.7, # the opacity of the fill colour
              color = "#2e2e30", # colour of shape outlines
              weight = 2) %>% # thickness of the shape outlines
  addLegend("bottomright", pal = colourpal4, values = ~percentage_4,
            title = "Coding discrepancy 4",
            labFormat = labelFormat(suffix = " %"),
            opacity = 1)


error5map <- leaflet(ShapeFile) %>%
  addPolygons(fillColor = ~colourpal5(percentage_5), # our colour palette function
              fillOpacity = 0.7, # the opacity of the fill colour
              color = "#2e2e30", # colour of shape outlines
              weight = 2) %>% # thickness of the shape outlines
  addLegend("bottomright", pal = colourpal5, values = ~percentage_5,
            title = "Coding discrepancy 5",
            labFormat = labelFormat(suffix = " %"),
            opacity = 1)

error6map <- leaflet(ShapeFile) %>%
  addPolygons(fillColor = ~colourpal6(percentage_6), # our colour palette function
              fillOpacity = 0.7, # the opacity of the fill colour
              color = "#2e2e30", # colour of shape outlines
              weight = 2) %>% # thickness of the shape outlines
  addLegend("bottomright", pal = colourpal6, values = ~percentage_6,
            title = "Coding discrepancy 6",
            labFormat = labelFormat(suffix = " %"),
            opacity = 1)
