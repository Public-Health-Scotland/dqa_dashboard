# In the line below, import the shiny library so that it's available
# in both ui.R and server.R
library(readxl)
library(shiny)
library(tidyverse)
library(DT)

library(odbc)       #R library for Open Database Connectivity, used to connect to databases
library(RODBC)      #Manage DB connections

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
setwd("~/dqa_dashboard/dashboard")

#Import data
hb_path <- "~/dqa_dashboard/smr_data/Hospital_SMR_accuracy_2004-Present.xlsx"

hb_accuracy <- hb_path %>%
  excel_sheets()%>%
  set_names()%>%
  map_df(~read_excel(hb_path, sheet = .x), 
         col_types = c("text", "text", "text", "numeric", "text"), .id = "Healthboard")%>%
  select(c(1:6))

#quick summary of hb_accuracy layout
#keeping NA values for now, but can be removed with na.omit() if needed
glimpse(hb_accuracy)
colnames(hb_accuracy)
lapply(hb_accuracy, unique)

#quick formatting cleanup
hb_accuracy[hb_accuracy$Year == "2004/2006", "Year"] <- "2004-2006"

#Create a new accuracy data frame with a column with mean values grouped by SMR, Year and data item 
hb_mean <- hb_accuracy %>%
  group_by(Audit, Year, DataItemName)%>%
  mutate(MeanAccuracy = round(mean(Accuracy, na.rm=TRUE),2))
hb_mean$Accuracy <- round(hb_mean$Accuracy, 2)




### Coding Discrepancies Data -----------------------------------------------


# con <- dbConnect(odbc(), dsn = "SMRA", uid = .rs.askForPassword("SMRA Username:"), 
#                  pwd = .rs.askForPassword("SMRA Password:"))



# ##SMR01
# odbcPreviewObject(con, table="ANALYSIS.SMR01_PI", rowLimit=0)
# 
# diagnosis1 <- dbGetQuery(con, "SELECT MAIN_CONDITION,DISCHARGE_TYPE, DISCHARGE_TRANSFER_TO_LOCATION, CIS_MARKER FROM ANALYSIS.SMR01_PI" )



## SMR02

#select diagnosis records where diabetes during pregnancy has either been clinically coded or hard coded, and where the baby was delivered (ie. condition on discharge==3)
# diagnosis2 <- dbGetQuery(con, "SELECT MAIN_CONDITION, OTHER_CONDITION_1, OTHER_CONDITION_2, OTHER_CONDITION_3, 
# 
#                               OTHER_CONDITION_4, OTHER_CONDITION_5, DIABETES, 
#                               
#                               EPISODE_RECORD_KEY, HBTREAT_CURRENTDATE, LOCATION
# 
#                               FROM ANALYSIS.SMR02_PI
# 
#                               WHERE CONDITION_ON_DISCHARGE = '3'
#                                     AND 
#                                       (DIABETES IN ('1', '2', '3') 
#                                       OR MAIN_CONDITION LIKE 'O24%'
#                                       OR OTHER_CONDITION_1 LIKE 'O24%'
#                                       OR OTHER_CONDITION_2 LIKE 'O24%'
#                                       OR OTHER_CONDITION_3 LIKE 'O24%'
#                                       OR OTHER_CONDITION_4 LIKE 'O24%'
#                                       OR OTHER_CONDITION_5 LIKE 'O24%')")
# 
# RODBC::odbcCloseAll() #close all open rodbc connections
# 
# glimpse(diagnosis2) #there should be at least one 024% code or a diabetes value in (1,2,3) in each row
# 
# 
# unique(diagnosis2$DIABETES) #we should have only numerical values or NAs





