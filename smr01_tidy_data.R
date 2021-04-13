
### Description -------------------------------------------------------------

#This code retrieves and tidies accuracy percentage results from national audits conducted on SMR data.
#The percentages are averages of accuracy data produced for hospitals accross all 14 healthboards of Scotland. 
#The data is split by data item and audit.

### 1 - Libraries -----------------------------------------------------------

library(readxl) #read in excel files
library(tidyr)  
library(dplyr)
library(tabulizer) #extract tables from pdf documents
library(stringr)


### 2 - Load SMR Audit Data -------------------------------------------------

SMR01_accuracy <- read_excel("~/dqa dashboard/SMR01 accuracy by data item and hospital.xls", sheet = 2)

SMR02_accuracy <- read_excel("smr02_accuracy_summary_by_year.xlsx")

pdf_path <- "https://www.isdscotland.org/Products-and-Services/Data-Quality/docs/Assessment-of-SMR04-Data-Scotland-2015-2016.pdf"
tables <- extract_tables(pdf_path, output="data.frame", pages=c(28,29))

SMR04_df1 <- tables[[1]]
SMR04_df2 <- tables[[2]]


### 3 - Housekeeping --------------------------------------------------------

#rounding percentages to the same decimal point
SMR01_accuracy$`2019/20` <- round(SMR01_accuracy$`2019/20`, 1)
SMR02_accuracy$`2017/18`<- round(SMR02_accuracy$`2017/18`, 1)

#replacing na strings with NA value
na_index <- SMR02_accuracy$`2008/09`=="Not assessed"
SMR02_accuracy$`2008/09`[na_index] <- NA

SMR02_accuracy$`2008/09`<-as.numeric(SMR02_accuracy$`2008/09`)



# 4 - Convert Data to Tidy Format -----------------------------------------

SMR01_tidy <- SMR01_accuracy %>%
  pivot_longer(cols=2:5, names_to="Year", values_to = "Accuracy")%>%
  rename("DataItemName"= "...1") %>%
  mutate(Audit="SMR01")


SMR02_tidy <- SMR02_accuracy %>%
  pivot_longer(cols=2:3, names_to="Year", values_to = "Accuracy")%>%
  rename("DataItemName"= "Data item") %>%
  mutate(Audit="SMR02")

SMR04_df1 <- SMR04_df1[c(3:4), c(1,5:6)]%>%
  pivot_longer(cols =2:3, values_to="Accuracy")%>%
  rename("DataItemName" = "Data.item")%>%
  mutate(Year="2015/16")%>%
  mutate(Audit="SMR04")

SMR04_df1$DataItemName<-paste(SMR04_df1$DataItemName, c("(3-digits)", "(4-digits)", "(3-digits)", "(4-digits)"), sep = " ")

SMR04_df1 <- SMR04_df1[,-2]

SMR04_df2<- SMR04_df2[c(2:9), c(1,4)] %>%
  rename(c("DataItemName"="Non.clinical.data.item", "Accuracy"="Percentage"))%>%
  mutate(Year="2015/16")%>%
  mutate(Audit="SMR04")

# Bind all of our tidy dataframes together
SMR_tidy <- rbind(SMR01_tidy,SMR02_tidy,SMR04_df1,SMR04_df2)
SMR_tidy$Accuracy <- as.numeric(SMR_tidy$Accuracy)


# 5 - List data item name by smr ------------------------------------------
name_smr <- SMR_tidy %>% 
  select(Audit, DataItemName)%>%
  distinct()

# 6 - Write csv outputs ---------------------------------------------------
write.csv(SMR_tidy, "SMR_accuracy.csv")
write.csv(name_smr, "data_item_by_audit_list.csv")
