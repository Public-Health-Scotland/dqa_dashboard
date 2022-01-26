# libraries are loaded by setup_environment file

# library(odbc)
# 
# library(readr)
# library(dplyr)
# library(tidyr)
# library(janitor)
# library(lubridate)
# library(stringr)   #loads str_c function used for icon creation
# library(sparkline) #create small barcharts for tables
# library(shiny)     #loads icon function for completeness table

# Function(s) --------------------------------------------------------

# walk(list.files(here::here("functions"), full.names = TRUE), source)

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
                  c("S08200004", "Outside U.K."),
                  c("S08100001", "Golden Jubilee"),
                  c("S08100008", "State Hospital"),
                  c("S27000001", "Non-NHS Provider Location"))) %>%
    rename("hb"="V1", "hb_name"="V2")

hb_lookup <- rbind(hb2019, hb_other)

#Extract data ---------------------------------------------------
#smr00 contains outpatient data so the records are filtered by clinic date
raw_smr00 <- dbGetQuery(con, statement = "SELECT clinic_date, hbtreat_currentdate, dob, sex, postcode, 
ethnic_group, main_operation, mode_of_clinical_interaction, referral_type, specialty
                 FROM ANALYSIS.SMR00_PI
                 WHERE clinic_date >= trunc((ADD_MONTHS(SYSDATE, -6)), 'MONTH')" )%>%
  clean_names()%>%
  rename("event_date"="clinic_date")

#for the remaining datasets smr01, 02 and 04, the data is filtered by discharge date
raw_smr01 <- dbGetQuery(con, statement = "SELECT discharge_date, hbtreat_currentdate,dob, sex, postcode, ethnic_group,
main_condition, main_operation, admission_type, significant_facility, admission_transfer_from, discharge_transfer_to,
management_of_patient, specialty
                 FROM ANALYSIS.SMR01_PI
                 WHERE discharge_date >= trunc((ADD_MONTHS(SYSDATE, -6)), 'MONTH')" )%>%
  clean_names() %>%
  rename("event_date"="discharge_date")

# unique(raw_smr01[month(raw_smr01$event_date)==12 & year(raw_smr01$event_date)==2021,]$hbtreat_currentdate)

raw_smr02 <- dbGetQuery(con, statement = "SELECT discharge_date, hbtreat_currentdate, dob, sex, postcode, ethnic_group,
main_condition, main_operation, admission_type, significant_facility, admission_transfer_from, discharge_transfer_to,
management_of_patient, specialty, condition_on_discharge
                 FROM ANALYSIS.SMR02_PI
           WHERE discharge_date >= trunc((ADD_MONTHS(SYSDATE, -6)), 'MONTH')" )%>%
  clean_names() %>% 
  rename("event_date"="discharge_date")


raw_smr04 <- dbGetQuery(con, statement = "SELECT discharge_date, hbtreat_currentdate, dob, sex, postcode, 
ethnic_group, main_condition, main_operation, admission_type, significant_facility,
admission_transfer_from, discharge_transfer_to, management_of_patient, specialty
                 FROM ANALYSIS.SMR04_PI
                 WHERE discharge_date >= trunc((ADD_MONTHS(SYSDATE, -6)), 'MONTH')" )%>%
  clean_names() %>% 
  rename("event_date"="discharge_date")


# Wrangle data ------------------------------------------------------------

#vectors of names of the SMR data items we want to monitor completeness for
#list of data items per smr

smr00_cols <- c("dob", "sex", "postcode", "ethnic_group", "main_operation",
                     "mode_of_clinical_interaction", "referral_type", "specialty")


smr01_cols <- c("dob", "sex", "postcode", "ethnic_group", "main_condition", "main_operation", "admission_type",
                     "significant_facility","admission_transfer_from", 
                     "discharge_transfer_to", "management_of_patient", "specialty")

smr02_cols <- c("dob", "sex", "postcode", "main_condition", "main_operation","ethnic_group", "admission_type",
                "admission_transfer_from", "discharge_transfer_to", 
                "management_of_patient", "specialty",
                "condition_on_discharge")

smr04_cols <- c("dob", "sex", "postcode", "main_condition", "main_operation", "ethnic_group", "admission_type",
                "admission_transfer_from", 
                "discharge_transfer_to", "management_of_patient", "specialty")

#Output completeness for each smr

smr00_completeness <- completeness(raw_smr00, smr00_cols)
smr01_completeness <- completeness(raw_smr01, smr01_cols)
smr02_completeness <- completeness(raw_smr02, smr02_cols)
smr04_completeness <- completeness(raw_smr04, smr04_cols)

df_names <- c("smr00_completeness", "smr01_completeness", "smr02_completeness",
              "smr04_completeness")


#Bind and format completeness data frame

smr_completeness <- append_source(df_names)%>%
  left_join(hb_lookup, by = c("hbtreat_currentdate" = "hb")) %>%
  mutate(month_name = month(event_month, label = TRUE, abbr = FALSE),
         source = case_when(source=="smr00_completeness" ~ "SMR00", 
                            source=="smr01_completeness" ~ "SMR01", 
                            source == "smr02_completeness" ~ "SMR02", 
                            source == "smr04_completeness" ~ "SMR04"))%>%
  rename("smr"="source") %>% 
  ungroup()


# Generate sparkline plots for % completeness over previous months
# plots are stored in html and will be executed by the shiny app

completeness_plots <- smr_completeness %>%
  group_by(smr, hb_name, data_item)%>%
  arrange(event_month)%>%
  summarise(mini_plot = spk_chr(values = percent_complete_month,
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

#store record of the earliest month and latest month the bar charts were produced from
#this will be displayed in the text above the table when the app is run

min_date <- min(smr_completeness$event_month)
max_date <- max(smr_completeness$event_month)

comp_barchart_dates <- data.frame(min_year = year(min_date), 
        min_month= month(min_date, label = TRUE, abbr = FALSE),
        max_year=year(max_date),
        max_month=month(max_date, label = TRUE, abbr = FALSE))

# Write the Outputs --------------------------------------------------------

#write out the table output so that it can be imported in global.R
write_csv(smr_completeness_2, here::here("data", "smr_completeness.csv"))

#write the barchart dates so that they can be referenced in the ui an server
write_csv(comp_barchart_dates, here::here("data", "comp_barchart_dates.csv"))
