library(readxl)
library(dplyr)
library(tidyr)
library(pdftools)


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

SMR01_tidy <- cbind(SMR01_tidy[4],SMR01_tidy[1:3])

SMR02_tidy <- SMR02_accuracy %>%
  pivot_longer(cols=2:3, names_to="Year", values_to = "Accuracy")%>%
  rename("DataItemName"= "Data item") %>%
  mutate(Audit="SMR02")
  
