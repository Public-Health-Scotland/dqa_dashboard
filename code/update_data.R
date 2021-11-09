
#libraries
library(zip)
library(shiny)#needed to load icon function for completeness table

#database connection
con <- dbConnect(odbc(), dsn = "SMRA", uid = .rs.askForPassword("SMRA Username:"), 
                 pwd = .rs.askForPassword("SMRA Password:"))

#source scripts from code folder
source(here::here("code/data_quality_tab","data_monitoring_completeness.R"))
#close database connection

#write outputs to data folder

#save a zipped copy in archive
data_list <- paste0("/home/maians01/dqa_dashboard/data/",list.files(here::here("data")))
zip::zip(paste0("/conf/Data_Quality_Dashboard/data_archive/dq_dashboard_data_", 
           Sys.Date(), ".zip"), data_list, mode="cherry-pick")
