# library(RODBC)

library(DBI)
library(odbc)

library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(purrr)
library(lubridate)


# Load Function(s) --------------------------------------------------------
walk(list.files(here::here("functions"), full.names = TRUE), source)

spk_tool <- function(labels) {
  htmlwidgets::JS(
    sprintf(
      "function(sparkline, options, field){
  return %s[field[0].offset];
}",
      jsonlite::toJSON(labels)
    )
  )
}

# Import lookup ----------------------------------------------------------


hb2019 <- read_csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/652ff726-e676-4a20-abda-435b98dd7bdc/download/hb14_hb19.csv")%>%
  select(HB, HBName)%>%
  clean_names()

hb_other <- as.data.frame(rbind(c("S08200001", "England/Wales/Northern Ireland"),
                  c("S08200002", "No Fixed Abode"),
                  c("S08200003", "Not Known"),
                  c("S08200004", "Outside U.K."))) %>%
    rename("hb"="V1", "hb_name"="V2")

hb_lookup <- rbind(hb2019, hb_other)



# Pull and Wrangle data ---------------------------------------------------

         
con <- dbConnect(odbc(), dsn = "SMRA", uid = .rs.askForPassword("SMRA Username:"), 
                 pwd = .rs.askForPassword("SMRA Password:"))

#select records  over a specific time period from smr00, smr01, smr02 and smr04

#smr00 contains outpatient data so the records are filtered by clinic date
raw_smr00 <- dbGetQuery(con, statement = "SELECT *
                 FROM ANALYSIS.SMR00_PI
                 WHERE clinic_date BETWEEN {d TO_DATE('2021-01-01', 'YYYY-MM-DD')} 
                 AND {d TO_DATE('2021-06-30', 'YYYY-MM-DD')};" )%>%
  clean_names()%>%
  rename("event_date"="clinic_date")

#for the remaining datasets smr01, 02 and 04, the data is filtered by discharge date
raw_smr01 <- dbGetQuery(con, statement = "SELECT *
                 FROM ANALYSIS.SMR01_PI
                 WHERE discharge_date BETWEEN {d TO_DATE('2021-01-01', 'YYYY-MM-DD')} 
                 AND {d TO_DATE('2021-06-30', 'YYYY-MM-DD')};" )%>%
  clean_names() %>%
  rename("event_date"="discharge_date")

raw_smr02 <- dbGetQuery(con, statement = "SELECT *
                 FROM ANALYSIS.SMR02_PI
           WHERE discharge_date BETWEEN {d TO_DATE('2021-01-01', 'YYYY-MM-DD')} 
           AND {d TO_DATE('2021-06-30', 'YYYY-MM-DD')};" )%>%
  clean_names() %>% 
  rename("event_date"="discharge_date")


raw_smr04 <- dbGetQuery(con, statement = "SELECT *
                 FROM ANALYSIS.SMR04_PI
                 WHERE discharge_date BETWEEN {d TO_DATE('2021-01-01', 'YYYY-MM-DD')} 
                 AND {d TO_DATE('2021-06-30', 'YYYY-MM-DD')};" )%>%
  clean_names() %>% 
  rename("event_date"="discharge_date")




#vectors of names of the SMR data items we want to monitor completeness for

#list of data items per smr

smr00_cols <- c("dob", "sex", "postcode", "ethnic_group", "main_operation",
                     "mode_of_clinical_interaction", "referral_type", "specialty")


smr01_cols <- c("dob", "sex", "postcode", "ethnic_group", "main_operation", "admission_type",
                     "significant_facility","admission_transfer_from", 
                     "discharge_transfer_to", "management_of_patient", "specialty")

smr02_cols <- c("dob", "sex", "postcode", "main_operation","ethnic_group", "admission_type",
                "admission_transfer_from", "discharge_transfer_to", 
                "management_of_patient", "specialty",
                "condition_on_discharge")

smr04_cols <- c("dob", "sex", "postcode","main_operation", "ethnic_group", "admission_type",
                "admission_transfer_from", 
                "discharge_transfer_to", "management_of_patient", "specialty")

#Output completeness for each smr

smr00_completeness <- completeness(raw_smr00, smr00_cols)
smr01_completeness <- completeness(raw_smr01, smr01_cols)
smr02_completeness <- completeness(raw_smr02, smr02_cols)
smr04_completeness <- completeness(raw_smr04, smr04_cols)

df_names <- c("smr00_completeness", "smr01_completeness", "smr02_completeness", "smr04_completeness")


#Bind and format completeness data frame

smr_completeness <- append_source(df_names)%>%
  left_join(hb_lookup, by = c("hbres_currentdate" = "hb")) %>%
  mutate(month_name =
           recode_factor(as.factor(event_month),
                         `1`="January", `2`="February", `3`="March", `4`="April",
                         `5`="May", `6`="June", `7`="July", `8`="August",
                         `9`="September", `10`="October", `11`="November", `12`="December"),
         source = case_when(source=="smr00_completeness" ~ "SMR00", 
                            source=="smr01_completeness" ~ "SMR01", 
                            source == "smr02_completeness" ~ "SMR02", 
                            source == "smr04_completeness" ~ "SMR04"))%>%
  rename("smr"="source") %>% 
  ungroup()


# Generate sparkline plots for % completeness over previous months
# plots are stored in html and will be executed by the shiny app

completeness_plots <- smr_completeness %>%
  group_by(smr, hb_name, data_item, event_year)%>%
  arrange(event_month)%>%
  summarise(mini_plot = spk_chr(percent_complete_month,
                                height = "40px",
                                width = 100,
                                chartRangeMin = 0,
                                chartRangeMax = 100,
                                type="bar",
                                numberDigitGroupSep = "",
                                barWidth = 10,
                                barSpacing = 2,
                                tooltipFormat = 
                                  paste0(
                                  '{{value}}', '%')
                                )
            )

change_df <- create_change_symbol(smr_completeness)
flag_df <- create_flag_symbol(smr_completeness)

#Join the plots to the main table and filter through last month's figures only
smr_completeness_2 <- smr_completeness %>%
  filter(event_month == max(event_month)) %>%
  left_join(completeness_plots) %>%
  left_join(change_df)%>%
  left_join(flag_df)

#finally, we store a record of the earliest month and latest month the bar charts were produced from
#this will be displayed in the text above the table when the app is run
date1 <- smr_completeness %>% 
  filter(event_year == min(event_year) & event_month == min(event_month))%>%
  distinct(event_year, event_month, month_name) %>% 
  rename(year_1=event_year, month_1=event_month, month_name_1=month_name)

date2 <- smr_completeness %>% 
  filter(event_year == max(event_year) & event_month == max(event_month))%>%
  distinct(event_year, event_month, month_name) %>% 
  rename(year_2=event_year, month_2=event_month, month_name_2=month_name)

comp_barchart_dates <- cbind(date1, date2)

# Write the Outputs --------------------------------------------------------

#write out the table output so that it can be imported in global.R
write_csv(smr_completeness_2, here::here("data", "smr_completeness.csv"))

#write the barchart dates so taht they can be referenced in the ui
write_csv(comp_barchart_dates, here::here("data", "comp_barchart_dates.csv"))
