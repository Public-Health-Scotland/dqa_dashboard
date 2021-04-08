library(readxl)
library(tidyr)
library(dplyr)
library(tabulizer)

###Load SMR0 Tables###
SMR01_accuracy <- read_excel("~/dqa dashboard/SMR01 accuracy by data item and hospital.xls", sheet = 2)
SMR02_accuracy <- read_excel("smr02_accuracy_summary_by_year.xlsx")


###House Keeping###

#rounding percentages to the same decimal point
SMR01_accuracy$`2019/20` <- round(SMR01_accuracy$`2019/20`, 1)
SMR02_accuracy$`2017/18`<- round(SMR02_accuracy$`2017/18`, 1)

#replacing na strings with NA value
na_index <- SMR02_accuracy$`2008/09`=="Not assessed"
SMR02_accuracy$`2008/09`[na_index] <- NA

SMR02_accuracy$`2008/09`<-as.numeric(SMR02_accuracy$`2008/09`)


###Convert Data to Tidy Format###

SMR01_tidy <- SMR01_accuracy %>%
  pivot_longer(cols=2:5, names_to="Year", values_to = "Accuracy")%>%
  rename("DataItemName"= "...1") %>%
  mutate(Audit="SMR01")


SMR02_tidy <- SMR02_accuracy %>%
  pivot_longer(cols=2:3, names_to="Year", values_to = "Accuracy")%>%
  rename("DataItemName"= "Data item") %>%
  mutate(Audit="SMR02")


pdf_path <- "https://www.isdscotland.org/Products-and-Services/Data-Quality/docs/Assessment-of-SMR04-Data-Scotland-2015-2016.pdf"
table1 <- extract_tables(pdf_path, output="data.frame", pages=c(29))
SMR04_table1 <- table1[[1]]
SMR04_table1<- SMR04_table1[c(2:9), c(1,4)] %>%
  rename(c("DataItemName"="Non.clinical.data.item", "Accuracy"="Percentage"))%>%
  mutate(Year="2015/16")%>%
  mutate(Audit="SMR04")

SMR_tidy <- rbind(SMR01_tidy,SMR02_tidy,SMR04_table1)
