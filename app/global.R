#Global Setup

# libraries ---------------------------------------------------------------

library(shiny)
library(shinyWidgets)
library(openxlsx)
library(readr)

library(dplyr)
library(tidyr)
library(tibble)

library(stringr)

library(DT)
library(sparkline)

library(ggplot2)
library(plotly)

library(base64enc)  #needed to display png image of phs logo

library(leaflet)    #both libraries necessary for creating maps
library(rgdal)
library(leaflegend)

library(shinymanager)

#Read in Data -----------------------------------------------------------

#smr timeliness data
timeliness <- read_csv(here::here("data","timeliness.csv"))

#smr completeness table with sparkline plot html
smr_completeness <- read_csv(here::here("data", "smr_completeness.csv")) %>%
  select(-hbtreat_currentdate)

#record of date range used to produced sparkline plots in completeness table
comp_barchart_dates <- read_csv(here::here("data", "comp_barchart_dates.csv"))

#smr audit data
smr_audit <- read_csv(here::here("data", "dashboard_smr_audit_data.csv"))

#smr01 clinical coding data
RCodes_table <- read.csv(here::here("data", "r_codes.csv"))

#smr02 clinical coding data
smr02_diabetes <- read_csv(here::here("data", "smr02_diabetes_data.csv"))

