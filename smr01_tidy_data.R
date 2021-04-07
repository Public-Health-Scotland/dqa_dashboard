library(readxl)
library(tidyr)
library(dplyr)

###Load SMR0 Tables###
SMR01_accuracy <- read_excel("~/dqa dashboard/SMR01 accuracy by data item and hospital.xls", sheet = 2)
View(SMR01_accuracy)



###Convert Data to Tidy Format###
SMR01_accuracy$`2019/20` <- round(SMR01_accuracy$`2019/20`, 1)

SMR01_tidy <- SMR01_accuracy %>%
  pivot_longer(cols=2:5, names_to="Year", values_to = "Accuracy")%>%
  rename("DataItemName"= "...1") %>%
  mutate(Audit="SMR01")

SMR01_tidy <- cbind(SMR01_tidy[4],SMR01_tidy[1:3])
  
