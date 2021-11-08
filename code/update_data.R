
#libraries
library(zip)

#database connection

#source scripts from code folder

#write outputs to data folder

#save a zipped copy in archive
data_list <- paste0("/home/maians01/dqa_dashboard/data/",list.files(here::here("data")))
zip::zip(paste0("/conf/Data_Quality_Dashboard/data_archive/dq_dashboard_data_", 
           Sys.Date(), ".zip"), data_list, mode="cherry-pick")
