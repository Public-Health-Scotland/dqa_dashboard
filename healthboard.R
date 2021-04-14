library(tidyr)
library(dplyr)
library(readxl)
library(stringr)
library(ggplot2)

#SMR01 2018/19, accuracy data split by hospital
SMR01_2018<- read_excel("SMR01 accuracy by data item and hospital.xls", sheet = "all data items by site")
SMR01_2018 <- SMR01_2018[-c(2,10,29,34,40),] #removing label rows

tidy_df <- SMR01_2018 %>%
  pivot_longer(cols=4:14, names_to = "DataItemName", values_to = "Accuracy")%>%
  mutate(Healthboard = case_when(Hospital == "Scotland" ~ "NHS SCOTLAND",
                                 
                                 str_detect(Hospital, "Arran War Memorial|Crosshouse|The Ayr Hospital") ~ "NHS AYRSHIRE & ARRAN",
                                 
                                 Hospital == "Borders General Hospital" ~ "NHS BORDERS",
                                 
                                 str_detect(Hospital,"Dumfries & Galloway Royal Infirmary|Galloway Community Hospital") ~ "NHS DUMFRIES & GALLOWAY",
                                 
                                 str_detect(Hospital,"Queen Margaret Hospital|Victoria Hospital") ~ "NHS FIFE",
                                 
                                 Hospital == "Forth Valley Royal Hospital" ~ "NHS FORTH VALLEY",
                                 
                                 str_detect(Hospital,"Royal Aberdeen Children's Hospital|Aberdeen Royal Infirmary|Dr Gray's Hospital") ~ "NHS GRAMPIAN",
                                 
                                 str_detect(Hospital,"Glasgow Royal Infirmary|Inverclyde Royal Hospital|Royal Alexandra Hospital|Royal Hospital for Sick Children, Yorkhill|Southern General Hospital|Vale of Leven District General Hospital|Victoria Infirmary, Glasgow|Western Infirmary/Gartnavel/Beatson Hospitals")
                                 ~ "NHS GREATER GLASGOW & CLYDE",
                                 
                                 str_detect(Hospital,"Belford Hospital|Caithness General Hospital|Lorn and Island District General Hospital|Raigmore Hospital") ~ "NHS HIGHLAND",
                                 
                                 str_detect(Hospital,"Wishaw General Hospital|Monklands Hospital|Hairmyres Hospital") ~ "NHS LANARKSHIRE",
                                 
                                 str_detect(Hospital,"Western General Hospital, Edinburgh|Royal Hospital for Sick Children, Edinburgh|Royal Infirmary of Edinburgh|St John's Hospital at Howden") 
                                 ~ "NHS LOTHIAN",
                                 
                                 Hospital == "Balfour Hospital" ~ "NHS ORKNEY",
                                 
                                 str_detect(Hospital,"Ninewells Hospital|Perth Royal Infirmary|Stracathro Hospital") ~ "NHS TAYSIDE",
                                 
                                 Hospital == "Gilbert Bain Hospital (Shetland)" ~ "NHS SHETLAND",
                                 
                                 Hospital == "Western Isles Hospital" ~ "NHS WESTERN ISLES",
                                 
                                 Hospital == "Golden Jubilee National Hospital" ~ "NHS GOLDEN JUBILEE"))

#convert hospital type to factor variable
tidy_df$`Hospital Type` <- factor(tidy_df$`Hospital Type`, levels=c("A1","A2","A31","A32","A4"), 
                                     labels = c("Teaching Hospitals", "General Hospitals(Large)",
                                                "General Hospitals(Medium)","General Hospitals(Small)", "Children's Hospitals" ))

#create tidy_df_output formatted for the excel spreadsheet on the network

###plots
#healthboard means data set
hm <- tidy_df %>%
  group_by(Healthboard)%>%
  summarise(accuracy = mean(Accuracy))

ggplot(hm, aes(x=Healthboard, y=accuracy))+
  geom_col()
