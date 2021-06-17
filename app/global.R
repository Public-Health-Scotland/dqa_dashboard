# In the line below, import the shiny library so that it's available
# in both ui.R and server.R
library(openxlsx)
library(shiny)
library(tidyverse)
library(DT)

library(odbc)       #R library for Open Database Connectivity, used to connect to databases
library(RODBC)      #Manage DB connections




#Read in Data -----------------------------------------------------------

#smr audit data
smr_audit <- read_csv(here::here("data", "dashboard_smr_audit_data.csv"))








