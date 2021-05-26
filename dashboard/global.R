# In the line below, import the shiny library so that it's available
# in both ui.R and server.R
library(shiny)
library(tidyverse)
library(DT)
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


