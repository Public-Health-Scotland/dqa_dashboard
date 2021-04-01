library(readxl)
library(tidyr)

SMR01_accuracy <- read_excel("~/dqa dashboard/SMR01 accuracy by data item and hospital.xls", sheet = 2)
View(SMR01_accuracy)

#convert data to tidy format