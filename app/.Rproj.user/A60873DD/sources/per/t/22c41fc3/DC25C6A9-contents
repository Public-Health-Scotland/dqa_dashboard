# Environment Setup
# loads libraries and functions needed for all scripts in the code folder

# manage database connections
library(odbc) 
library(DBI)

#read and export data
library(readr)
library(readxl)
library(zip)

# wrangle data
library(dplyr)
library(tidyr)
library(purrr)
library(janitor)

library(lubridate) # manage dates

# completeness code libraries
library(stringr)   #loads str_c function used for icon creation
library(sparkline) #create small barcharts for tables
library(shiny)     #loads icon function for completeness table


#load functions
walk(list.files(here::here("functions"), full.names = TRUE), source)