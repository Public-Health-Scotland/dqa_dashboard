#libraries
library(odbc)
library(purrr)
library(zip)

#load functions
walk(list.files(here::here("functions"), full.names = TRUE), source)

#database connection
con <- dbConnect(odbc(), dsn = "SMRA", uid = .rs.askForPassword("SMRA Username:"), 
                 pwd = .rs.askForPassword("SMRA Password:"))


# Source individual scripts from code folder -----------------------------------------

##Data Quality Tab ---
#completeness
source(here::here("code/data_quality_tab","data_monitoring_completeness.R"))

#timeliness


#SMR audits accuracy results
source(here::here("code/data_quality_tab", "smr_audit.R"))

##Clinical Coding Discrepancies and Issues Tab ---
#smr01 R codes 
source(here::here("code/clinical_coding_tab", "terminology_tables_smr01.R"))

#smr02 clinical coding of diabetes
source(here::here("code/clinical_coding_tab", "terminology_tables_smr02.R"))

# Close database connection -----------------------------------------------

#save a zipped copy in archive
# data_list <- paste0("/home/maians01/dqa_dashboard/data/",list.files(here::here("data")))
# zip::zip(paste0("/conf/Data_Quality_Dashboard/data_archive/dq_dashboard_data_", 
#            Sys.Date(), ".zip"), data_list, mode="cherry-pick")
