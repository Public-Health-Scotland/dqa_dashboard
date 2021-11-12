library(readr)
library(tidyr)
library(dplyr)

# library(readxl)
# library(shiny)
# library(tidyverse)
# library(DT)

# library(leaflet)    #both libraries necessary for creating maps
# library(rgdal)



# Extract SMR02 data -----------------------------------------------------

# con <- dbConnect(odbc(), dsn = "SMRA", uid = .rs.askForPassword("SMRA Username:"), 
#                  pwd = .rs.askForPassword("SMRA Password:"))


diagnosis2 <- dbGetQuery(con, "SELECT MAIN_CONDITION, OTHER_CONDITION_1, OTHER_CONDITION_2, OTHER_CONDITION_3, 
                         
                         OTHER_CONDITION_4, OTHER_CONDITION_5, DIABETES, DISCHARGE_DATE,
                         
                         EPISODE_RECORD_KEY, HBTREAT_CURRENTDATE, LOCATION
                         
                         FROM ANALYSIS.SMR02_PI
                         
                         WHERE CONDITION_ON_DISCHARGE = '3'
                         AND 
                         (DIABETES IN ('1', '2', '3') 
                         OR MAIN_CONDITION LIKE 'O24%'
                         OR OTHER_CONDITION_1 LIKE 'O24%'
                         OR OTHER_CONDITION_2 LIKE 'O24%'
                         OR OTHER_CONDITION_3 LIKE 'O24%'
                         OR OTHER_CONDITION_4 LIKE 'O24%'
                         OR OTHER_CONDITION_5 LIKE 'O24%')")

RODBC::odbcCloseAll() #close all open rodbc connections


#Read in hb_lookup file:
hb_lookup <- read_csv(here::here("lookups", "hb_lookup.csv"))

#append hb names to diagnosis2 dataframe
diagnosis2 <- left_join(diagnosis2, hb_lookup[c(1:2)], by = c("HBTREAT_CURRENTDATE"="HB"))

diagnosis2 <- diagnosis2 %>% 
  mutate(
    year = (substr(DISCHARGE_DATE, 1, 4))
    )

# Error Counts ------------------------------------------------------------
non_error1 <- c("O240", "O241", "O242", "O243")
exc_error1 <- c("O244", "O249") 
years <- c('2017', '2018', '2019', '2020', '2021')
error_1_table <- diagnosis2 %>%
  group_by(HBName, year) %>%
  filter(DIABETES == 1, year %in% years) %>% 
  mutate(
    error_1 = case_when(
      MAIN_CONDITION %in% non_error1 | OTHER_CONDITION_1 %in% non_error1 | OTHER_CONDITION_2 %in% non_error1 | OTHER_CONDITION_3 %in% non_error1 | 
        OTHER_CONDITION_4 %in% non_error1 | OTHER_CONDITION_5 %in% non_error1 & !(MAIN_CONDITION %in% exc_error1) & !(OTHER_CONDITION_1 %in% exc_error1) &
        !(OTHER_CONDITION_2 %in% exc_error1) & !(OTHER_CONDITION_3 %in% exc_error1) & !(OTHER_CONDITION_4 %in% exc_error1) & 
        !(OTHER_CONDITION_5 %in% exc_error1) ~ 'no error', 
      TRUE ~ 'error 1')
  ) %>%
  summarise(error1 = sum(error_1 == "error 1"), denominator = sum(DIABETES == 1))%>%
  mutate(percentage_1 = round(error1/denominator*100, digits = 2))
error_1_table <- error_1_table[, c('HBName', 'year', "error1", "percentage_1"), drop = F]


error_split_1 <- diagnosis2 %>%
  group_by(HBName) %>%
  filter(DIABETES == 1) %>% 
  mutate(
    error_s1 = case_when(
      MAIN_CONDITION %in% non_error1 | OTHER_CONDITION_1 %in% non_error1 | OTHER_CONDITION_2 %in% non_error1 | OTHER_CONDITION_3 %in% non_error1 | 
        OTHER_CONDITION_4 %in% non_error1 | OTHER_CONDITION_5 %in% non_error1 & !(MAIN_CONDITION %in% exc_error1) & !(OTHER_CONDITION_1 %in% exc_error1) &
        !(OTHER_CONDITION_2 %in% exc_error1) & !(OTHER_CONDITION_3 %in% exc_error1) & !(OTHER_CONDITION_4 %in% exc_error1) & 
        !(OTHER_CONDITION_5 %in% exc_error1) ~ 'no error', 
      TRUE ~ 'error 1')
  ) %>%
  filter(error_s1 == 'error 1') %>% 
  mutate(
    error1_split = case_when(
      str_detect(MAIN_CONDITION, "^O24") | str_detect(OTHER_CONDITION_1, "^O24") | str_detect(OTHER_CONDITION_2, "^O24") | str_detect(OTHER_CONDITION_3, "^O24") | 
        str_detect(OTHER_CONDITION_4, "^O24") | str_detect(OTHER_CONDITION_5, "^O24") ~ 'ICD present', 
      TRUE ~ 'ICD absent'
    )) %>%
  summarise(err1_wrong_ICD10 = sum(error1_split == "ICD present"), denominator = sum(error1_split == 'ICD present' | error1_split == 'ICD absent'))%>%
  mutate(err1_wrong_ICD10_percent = round(err1_wrong_ICD10/denominator*100, digits = 2)) %>% 
  mutate(err1_no_ICD10_percent = round(100 - err1_wrong_ICD10_percent, digits = 2))
error_split_1 <- error_split_1[, c('HBName', "err1_wrong_ICD10_percent", "err1_no_ICD10_percent")]



error_2_table <- diagnosis2 %>%
  group_by(HBName, year) %>%
  filter(DIABETES == 2, year %in% years) %>%
  mutate(
    error_2 = case_when(
      MAIN_CONDITION == 'O244' | OTHER_CONDITION_1 == 'O244' | OTHER_CONDITION_2 == 'O244' |
        OTHER_CONDITION_3 == 'O244' | OTHER_CONDITION_4 == 'O244' | OTHER_CONDITION_5 == 'O244' ~ 'no error',
      TRUE ~ 'error 2')
  ) %>%
  summarise(error2 = sum(error_2 == "error 2"), denominator = sum(DIABETES == 2))%>%
  mutate(percentage_2 = round(error2/denominator*100, digits = 2))
error_2_table <- error_2_table[, c('HBName', 'year', "error2", "percentage_2")]


error_split_2 <- diagnosis2 %>%
  group_by(HBName) %>%
  filter(DIABETES == 2) %>%
  mutate(
    error_s2 = case_when(
      MAIN_CONDITION == 'O244' | OTHER_CONDITION_1 == 'O244' | OTHER_CONDITION_2 == 'O244' |
        OTHER_CONDITION_3 == 'O244' | OTHER_CONDITION_4 == 'O244' | OTHER_CONDITION_5 == 'O244' ~ 'no error',
      TRUE ~ 'error 2')
  ) %>% 
  filter(error_s2 == 'error 2') %>% 
  mutate(
    error2_split = case_when(
      str_detect(MAIN_CONDITION, "^O24") | str_detect(OTHER_CONDITION_1, "^O24") | str_detect(OTHER_CONDITION_2, "^O24") | str_detect(OTHER_CONDITION_3, "^O24") | 
        str_detect(OTHER_CONDITION_4, "^O24") | str_detect(OTHER_CONDITION_5, "^O24") ~ 'ICD present', 
      TRUE ~ 'ICD absent'
    )) %>%
  summarise(err2_wrong_ICD10 = sum(error2_split == "ICD present"), denominator = sum(error2_split == 'ICD present' | error2_split == 'ICD absent'))%>%
  mutate(err2_wrong_ICD10_percent = round(err2_wrong_ICD10/denominator*100, digits = 2)) %>% 
  mutate(err2_no_ICD10_percent = round(100 - err2_wrong_ICD10_percent, digits = 2))
error_split_2 <- error_split_2[, c('HBName', "err2_wrong_ICD10_percent", "err2_no_ICD10_percent")]
error_split_2
  
error_3_table <- diagnosis2 %>%
  group_by(HBName, year) %>%
  filter(DIABETES == 3, year %in% years) %>%
  mutate(
    error_3 = case_when(
      MAIN_CONDITION == 'O249' | OTHER_CONDITION_1 == 'O249' | OTHER_CONDITION_2 == 'O249' |
        OTHER_CONDITION_3 == 'O249' | OTHER_CONDITION_4 == 'O249' | OTHER_CONDITION_5 == 'O249' ~ 'no error',
      TRUE ~ 'error 3')
  ) %>%
  summarise(error3 = sum(error_3 == "error 3"), denominator = sum(DIABETES == 3))%>%
  mutate(percentage_3 = round(error3/denominator*100, digits = 2))
error_3_table <- error_3_table[, c('HBName', 'year', "error3", "percentage_3")]


error_split_3 <- diagnosis2 %>% 
  group_by(HBName, year) %>%
  filter(DIABETES == 3) %>%
  mutate(
    error_s3 = case_when(
      MAIN_CONDITION == 'O249' | OTHER_CONDITION_1 == 'O249' | OTHER_CONDITION_2 == 'O249' |
        OTHER_CONDITION_3 == 'O249' | OTHER_CONDITION_4 == 'O249' | OTHER_CONDITION_5 == 'O249' ~ 'no error',
      TRUE ~ 'error 3')
  ) %>%
  filter(error_s3 == 'error 3') %>% 
  mutate(
    error3_split = case_when(
    str_detect(MAIN_CONDITION, "^O24") | str_detect(OTHER_CONDITION_1, "^O24") | str_detect(OTHER_CONDITION_2, "^O24") | str_detect(OTHER_CONDITION_3, "^O24") | 
      str_detect(OTHER_CONDITION_4, "^O24") | str_detect(OTHER_CONDITION_5, "^O24") ~ 'ICD present', 
    TRUE ~ 'ICD absent'
    )) %>%
  summarise(err3_wrong_ICD10 = sum(error3_split == "ICD present"), denominator = sum(error3_split == 'ICD present' | error3_split == 'ICD absent'))%>%
  mutate(err3_wrong_ICD10_percent = round(err3_wrong_ICD10/denominator*100, digits = 2)) %>% 
  mutate(err3_no_ICD10_percent = round(100 - err3_wrong_ICD10_percent, digits = 2))
error_split_3 <- error_split_3[, c('HBName', 'year', "err3_wrong_ICD10_percent", "err3_no_ICD10_percent")]


error_4_table <- diagnosis2 %>%
  group_by(HBName, year) %>%
  filter(DIABETES == 4, year %in% years) %>%
  mutate(
    error_4 = case_when(
      str_detect(MAIN_CONDITION, "^O24") | str_detect(OTHER_CONDITION_1, "^O24") | str_detect(OTHER_CONDITION_2, "^O24") | str_detect(OTHER_CONDITION_3, "^O24") | 
        str_detect(OTHER_CONDITION_4, "^O24") | str_detect(OTHER_CONDITION_5, "^O24") ~ 'error 4', T ~ 'no error')
  ) %>%
  summarise(error4 = sum(error_4 == "error 4"), denominator = sum(DIABETES == 4))%>%
  mutate(percentage_4 = round(error4/denominator*100, digits = 2))
error_4_table <- error_4_table[, c('HBName','year', "error4", "percentage_4")]

# error_split_4 <- diagnosis2 %>%
#   group_by(HBName) %>%
#   filter(DIABETES == 4) %>%
#   mutate(
#     error_s4 = case_when(
#       str_detect(MAIN_CONDITION, "^O24") | str_detect(OTHER_CONDITION_1, "^O24") | str_detect(OTHER_CONDITION_2, "^O24") | str_detect(OTHER_CONDITION_3, "^O24") | 
#         str_detect(OTHER_CONDITION_4, "^O24") | str_detect(OTHER_CONDITION_5, "^O24") ~ 'error 4',
#       T ~ 'error 4')
#   ) %>%
#   filter(error_s4 == 'error 4') %>% 
#   mutate(
#     error4_split = case_when(
#       str_detect(MAIN_CONDITION, "^O24") | str_detect(OTHER_CONDITION_1, "^O24") | str_detect(OTHER_CONDITION_2, "^O24") | str_detect(OTHER_CONDITION_3, "^O24") | 
#         str_detect(OTHER_CONDITION_4, "^O24") | str_detect(OTHER_CONDITION_5, "^O24") ~ 'ICD present', 
#       TRUE ~ 'ICD absent'
#     )) %>%
#   summarise(err4_wrong_ICD10 = sum(error4_split == "ICD present"), denominator = sum(error4_split == 'ICD present' | error4_split == 'ICD absent'))%>%
#   mutate(err4_wrong_ICD10_percent = round(err4_wrong_ICD10/denominator*100, digits = 2)) %>% 
#   mutate(err4_no_ICD10_percent = round(100 - err4_wrong_ICD10_percent, digits = 2))
# error_split_4 <- error_split_4[, c('HBName', "err4_wrong_ICD10_percent", "err4_no_ICD10_percent")]
# error_split_4

error_5_table <- diagnosis2 %>%
  group_by(HBName, year) %>%
  filter(year %in% years) %>% 
  mutate(
    error_5 = case_when(
      !is.na(DIABETES) ~ 'no error',
      TRUE ~ 'error 5')
  ) %>%
  summarise(error5 = sum(error_5 == "error 5"), denominator = n())%>%
  mutate(percentage_5 = round(error5/denominator*100, digits = 2))
error_5_table <- error_5_table[, c('HBName', 'year', "error5", "percentage_5")]
error_5_table

error_split_5 <- diagnosis2 %>%
  group_by(HBName) %>%
  mutate(
    error_s5 = case_when(
      !is.na(DIABETES) ~ 'no error',
      TRUE ~ 'error 5')
  ) %>%
  filter(error_s5 == 'error 5') %>% 
  mutate(
    error5_split = case_when(
      str_detect(MAIN_CONDITION, "^O24") | str_detect(OTHER_CONDITION_1, "^O24") | str_detect(OTHER_CONDITION_2, "^O24") | str_detect(OTHER_CONDITION_3, "^O24") | 
        str_detect(OTHER_CONDITION_4, "^O24") | str_detect(OTHER_CONDITION_5, "^O24") ~ 'ICD present', 
      TRUE ~ 'ICD absent'
    )) %>%
  summarise(err5_wrong_ICD10 = sum(error5_split == "ICD present"), denominator = sum(error5_split == 'ICD present' | error5_split == 'ICD absent'))%>%
  mutate(err5_ICD10_present_percent = round(err5_wrong_ICD10/denominator*100, digits = 2)) %>% 
  mutate(err5_no_ICD10_percent = round(100 - err5_ICD10_present_percent, digits = 2))
error_split_5 <- error_split_5[, c('HBName', "err5_ICD10_present_percent", "err5_no_ICD10_percent")]

error_6_table <- diagnosis2 %>%
  group_by(HBName, year) %>%
  filter(year %in% years) %>% 
  mutate(
    error_6 = case_when(
      str_detect(MAIN_CONDITION, "^E10") | str_detect(OTHER_CONDITION_1, "^E10") | str_detect(OTHER_CONDITION_2, "^E10") | str_detect(OTHER_CONDITION_3, "^E10") | 
        str_detect(OTHER_CONDITION_4, "^E10") | str_detect(OTHER_CONDITION_5, "^E10") | str_detect(MAIN_CONDITION, "^E11") | str_detect(OTHER_CONDITION_1, "^E11") | 
        str_detect(OTHER_CONDITION_2, "^E11") | str_detect(OTHER_CONDITION_3, "^E11") | str_detect(OTHER_CONDITION_4, "^E11") | str_detect(OTHER_CONDITION_5, "^E11") |
        str_detect(MAIN_CONDITION, "^E12") | str_detect(OTHER_CONDITION_1, "^E12") | str_detect(OTHER_CONDITION_2, "^E12") | str_detect(OTHER_CONDITION_3, "^E12") | 
        str_detect(OTHER_CONDITION_4, "^E12") | str_detect(OTHER_CONDITION_5, "^E12") | str_detect(MAIN_CONDITION, "^E13") | str_detect(OTHER_CONDITION_1, "^E13") | 
        str_detect(OTHER_CONDITION_2, "^E13") | str_detect(OTHER_CONDITION_3, "^E13") | str_detect(OTHER_CONDITION_4, "^E13") | str_detect(OTHER_CONDITION_5, "^E13") |
        str_detect(MAIN_CONDITION, "^E14") | str_detect(OTHER_CONDITION_1, "^E14") | str_detect(OTHER_CONDITION_2, "^E14") | str_detect(OTHER_CONDITION_3, "^E14") | 
        str_detect(OTHER_CONDITION_4, "^E14") | str_detect(OTHER_CONDITION_5, "^E14") ~ 'error 6',
      T ~ 'no error')
  ) %>%
  summarise(error6 = sum(error_6 == "error 6"), denominator = n())%>%
  mutate(percentage_6 = round(error6/denominator*100, digits = 2))
error_6_table <- error_6_table[, c("HBName", 'year', "error6", "percentage_6")]

query_1_table <- diagnosis2 %>% 
  group_by(HBName, year) %>% 
  filter(DIABETES == 9, year %in% years) %>% 
  mutate(
    query = case_when(
      str_detect(MAIN_CONDITION, "^O24") | str_detect(OTHER_CONDITION_1, "^O24") | str_detect(OTHER_CONDITION_2, "^O24") | str_detect(OTHER_CONDITION_3, "^O24") | 
        str_detect(OTHER_CONDITION_4, "^O24") | str_detect(OTHER_CONDITION_5, "^O24") ~ 'ICD present', 
      TRUE ~ 'ICD absent'
    )) %>%
  summarise(query_count = sum(query == 'ICD present'), denominator = sum(DIABETES == 9)) %>% 
  mutate(query_percentage = round(query_count/denominator*100, digits = 2))
query_1_table <- query_1_table[, c("HBName", 'year', "query_count", "query_percentage")]


# Write out all the error tables ------------------------------------------
ls_error_tables <- list(error_1_table, error_2_table, error_3_table, error_4_table,
                        error_5_table, error_6_table, error_split_1, error_split_2,
                        error_split_3, error_split_5, query_1_table)
write_csv(error_1_table, here::here("data", "error1.csv"))
write_csv(error_2_table, here::here("data", "error2.csv"))
write_csv(error_3_table, here::here("data", "error3.csv"))
write_csv(error_4_table, here::here("data", "error4.csv"))
write_csv(error_5_table, here::here("data", "error5.csv"))
write_csv(error_6_table, here::here("data", "error6.csv"))
write_csv(error_split_1, here::here("data", "split1.csv"))
write_csv(error_split_2, here::here("data", "split2.csv"))
write_csv(error_split_3, here::here("data", "split3.csv"))
# write_csv(error_split_4, here::here("data", "split4.csv"))
write_csv(error_split_5, here::here("data", "split5.csv"))
write_csv(query_1_table, here::here("data", "query.csv"))


# Error maps ------------------------------------------------------

#read in the shapefile downloaded from the web and add the healthboard borders (polygons)
# ShapeFile = readOGR(dsn=here::here("data", "maps", "HBShapefile.shp"), layer="HBShapefile")
# ShapeFile <- spTransform(ShapeFile, CRS("+init=epsg:4326"))
# leaflet() %>%
#   addPolygons(data = ShapeFile)
# 
# ShapeFile@data <- ShapeFile@data %>%
#   rownames_to_column(var = "ID") %>% # Change row names to be an ID column
#   mutate(HBName = paste0("NHS ", HBName)) # add the NHS prefix to the names of the healthboards so they are uniform with the data files
# ShapeFile@data <- ShapeFile@data %>%
#   left_join(error_1_table) %>% #add each table one by one
#   left_join(error_2_table) %>% 
#   left_join(error_3_table) %>% 
#   left_join(error_4_table) %>% 
#   left_join(error_5_table) %>% 
#   left_join(error_6_table)

# colourpal1 <- colorNumeric("RdPu", domain = ShapeFile@data$percentage_1) #create a colour palette for each map, can be simplified
# colourpal2 <- colorNumeric("RdPu", domain = ShapeFile@data$percentage_2)
# colourpal3 <- colorNumeric("RdPu", domain = ShapeFile@data$percentage_3)
# colourpal4 <- colorNumeric("RdPu", domain = ShapeFile@data$percentage_4)
# colourpal5 <- colorNumeric("RdPu", domain = ShapeFile@data$percentage_5)
# colourpal6 <- colorNumeric("RdPu", domain = ShapeFile@data$percentage_6)
# 
# 
# error1map <- leaflet(ShapeFile) %>%
#   addPolygons(fillColor = ~colourpal1(percentage_1), # our colour palette function
#               fillOpacity = 0.7, # the opacity of the fill colour
#               color = "#2e2e30", # colour of shape outlines
#               weight = 2) %>% # thickness of the shape outlines
#   addLegend("bottomright", pal = colourpal1, values = ~percentage_1,
#             title = "Coding discrepancy 1",
#             labFormat = labelFormat(suffix = " %"), #add the percentage suffix
#             opacity = 1)
# 
# error2map <- leaflet(ShapeFile) %>%
#   addPolygons(fillColor = ~colourpal2(percentage_2), # our colour palette function
#               fillOpacity = 0.7, # the opacity of the fill colour
#               color = "#2e2e30", # colour of shape outlines
#               weight = 2) %>% # thickness of the shape outlines
#   addLegend("bottomright", pal = colourpal2, values = ~percentage_2,
#             title = "Coding discrepancy 2",
#             labFormat = labelFormat(suffix = " %"),
#             opacity = 1)
# 
# error3map <- leaflet(ShapeFile) %>%
#   addPolygons(fillColor = ~colourpal3(percentage_3), # our colour palette function
#               fillOpacity = 0.7, # the opacity of the fill colour
#               color = "#2e2e30", # colour of shape outlines
#               weight = 2) %>% # thickness of the shape outlines
#   addLegend("bottomright", pal = colourpal3, values = ~percentage_3,
#             title = "Coding discrepancy 3",
#             labFormat = labelFormat(suffix = " %"),
#             opacity = 1)
# 
# error4map <- leaflet(ShapeFile) %>%
#   addPolygons(fillColor = ~colourpal4(percentage_4), # our colour palette function
#               fillOpacity = 0.7, # the opacity of the fill colour
#               color = "#2e2e30", # colour of shape outlines
#               weight = 2) %>% # thickness of the shape outlines
#   addLegend("bottomright", pal = colourpal4, values = ~percentage_4,
#             title = "Coding discrepancy 4",
#             labFormat = labelFormat(suffix = " %"),
#             opacity = 1)
# 
# 
# error5map <- leaflet(ShapeFile) %>%
#   addPolygons(fillColor = ~colourpal5(percentage_5), # our colour palette function
#               fillOpacity = 0.7, # the opacity of the fill colour
#               color = "#2e2e30", # colour of shape outlines
#               weight = 2) %>% # thickness of the shape outlines
#   addLegend("bottomright", pal = colourpal5, values = ~percentage_5,
#             title = "Coding discrepancy 5",
#             labFormat = labelFormat(suffix = " %"),
#             opacity = 1)
# 
# error6map <- leaflet(ShapeFile) %>%
#   addPolygons(fillColor = ~colourpal6(percentage_6), # our colour palette function
#               fillOpacity = 0.7, # the opacity of the fill colour
#               color = "#2e2e30", # colour of shape outlines
#               weight = 2) %>% # thickness of the shape outlines
#   addLegend("bottomright", pal = colourpal6, values = ~percentage_6,
#             title = "Coding discrepancy 6",
#             labFormat = labelFormat(suffix = " %"),
#             opacity = 1)


###test
# error_4_table_test <- diagnosis2 %>%
#   group_by(HBName) %>%
#   filter(DIABETES == 4) %>%
#   mutate(
#     error_4 = case_when(
#       str_detect(MAIN_CONDITION, "^O24") | str_detect(OTHER_CONDITION_1, "^O24") | str_detect(OTHER_CONDITION_2, "^O24") | str_detect(OTHER_CONDITION_3, "^O24") |
#         str_detect(OTHER_CONDITION_4, "^O24") | str_detect(OTHER_CONDITION_5, "^O24") ~ 'error 4',
#       T ~ 'no error')
#   )
# %>%
#   summarise(error4 = sum(error_4 == "error 4"), denominator = sum(DIABETES == 4))%>%
#   mutate(percentage_4 = round(error4/denominator*100, digits = 2))
# error_4_table <- error_4_table[, c('HBName',"error4", "percentage_4")]
# 
# diagnosis2%>%
#   filter(DIABETES == 4 & MAIN_CONDITION =="O24")
  

# exc_error4 <- c("O240", "O241", "O242", "O243", "O244", "O249")
# error_4_table_test <- diagnosis2 %>%
#   group_by(HBName) %>%
#   filter(DIABETES == 4) %>%
#   mutate(error_4 = case_when( MAIN_CONDITION %in% exc_error4 | OTHER_CONDITION_1 %in% exc_error4|OTHER_CONDITION_2 %in% exc_error4 | 
#                                 OTHER_CONDITION_3 %in% exc_error4 | OTHER_CONDITION_4 %in% exc_error4 | OTHER_CONDITION_5 %in% exc_error4 ~ "error 4",
#                               TRUE ~ "no error"))

