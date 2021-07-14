# In the line below, import the shiny library so that it's available
# in both ui.R and server.R
library(shiny)
library(shinyWidgets)
library(openxlsx)
library(tidyverse)
library(DT)
library(sparkline)
library(odbc)       #R library for Open Database Connectivity, used to connect to databases
library(RODBC)      #Manage DB connections

library(leaflet)    #both libraries necessary for creating maps
library(rgdal)


#Read in Data -----------------------------------------------------------

#smr audit data
smr_audit <- read_csv(here::here("data", "dashboard_smr_audit_data.csv"))
  


#smr completeness data with sparkline plot html
smr_completeness <- read_csv(here::here("data", "smr_completeness.csv")) %>%
  select(-hbres_currentdate)


#smr02 clinical coding data
error_1_table <- read_csv(here::here("data", "error1.csv"))
error_2_table <- read_csv(here::here("data", "error2.csv"))
error_3_table <- read_csv(here::here("data", "error3.csv"))
error_4_table <- read_csv(here::here("data", "error4.csv"))
error_5_table <- read_csv(here::here("data", "error5.csv"))
error_6_table <- read_csv(here::here("data", "error6.csv"))
split_1_table <- read.csv(here::here("data", "split1.csv"))
split_2_table <- read.csv(here::here("data", "split2.csv"))
split_3_table <- read.csv(here::here("data", "split3.csv"))
split_4_table <- read.csv(here::here("data", "split4.csv"))
split_5_table <- read.csv(here::here("data", "split5.csv"))
query_1_table <- read.csv(here::here("data", "query.csv"))

# Error maps ------------------------------------------------------

#read in the shapefile downloaded from the web and add the healthboard borders (polygons)
ShapeFile = readOGR(dsn=here::here("data", "maps", "HBShapefile.shp"), layer="HBShapefile")
ShapeFile <- spTransform(ShapeFile, CRS("+init=epsg:4326"))
leaflet() %>%
  addPolygons(data = ShapeFile)

ShapeFile@data <- ShapeFile@data %>%
  rownames_to_column(var = "ID") %>% # Change row names to be an ID column
  mutate(HBName = paste0("NHS ", HBName)) # add the NHS prefix to the names of the healthboards so they are uniform with the data files
ShapeFile@data <- ShapeFile@data %>%
  left_join(error_1_table) %>% #add each table one by one
  left_join(error_2_table) %>% 
  left_join(error_3_table) %>% 
  left_join(error_4_table) %>% 
  left_join(error_5_table) %>% 
  left_join(error_6_table) %>% 
  left_join(query_1_table)

colourpal1 <- colorNumeric("RdPu", domain = ShapeFile@data$percentage_1) #create a colour palette for each map, can be simplified
colourpal2 <- colorNumeric("RdPu", domain = ShapeFile@data$percentage_2)
colourpal3 <- colorNumeric("RdPu", domain = ShapeFile@data$percentage_3)
colourpal4 <- colorNumeric("RdPu", domain = ShapeFile@data$percentage_4)
colourpal5 <- colorNumeric("RdPu", domain = ShapeFile@data$percentage_5)
colourpal6 <- colorNumeric("RdPu", domain = ShapeFile@data$percentage_6)
colourpal7 <- colorNumeric("RdPu", domain = ShapeFile@data$query_count)



error1map <- leaflet(ShapeFile, options = leafletOptions(zoomControl = FALSE,
                                                         dragging = FALSE)) %>%
  leaflet::addPolygons(fillColor = ~colourpal1(percentage_1), # our colour palette function
              fillOpacity = 0.7, # the opacity of the fill colour
              color = "#2e2e30", # colour of shape outlines
              weight = 2,
              popup = paste0("percentage error: ", ShapeFile@data$percentage_1)) %>% # thickness of the shape outlines
  leaflet::addLegend("bottomright", pal = colourpal1, values = ~percentage_1,
            title = "Coding discrepancy 1",
            labFormat = labelFormat(suffix = " %"), #add the percentage suffix
            opacity = 1)

error2map <- leaflet(ShapeFile, options = leafletOptions(zoomControl = FALSE,
                                                         dragging = FALSE)) %>%
  leaflet::addPolygons(fillColor = ~colourpal2(percentage_2), # our colour palette function
              fillOpacity = 0.7, # the opacity of the fill colour
              color = "#2e2e30", # colour of shape outlines
              weight = 2,
              popup = paste0("percentage error: ", ShapeFile@data$percentage_2)) %>% # thickness of the shape outlines
  leaflet::addLegend("bottomright", pal = colourpal2, values = ~percentage_2,
            title = "Coding discrepancy 2",
            labFormat = labelFormat(suffix = " %"),
            opacity = 1)

error3map <- leaflet(ShapeFile, options = leafletOptions(zoomControl = FALSE,
                                                         dragging = FALSE)) %>%
  leaflet::addPolygons(fillColor = ~colourpal3(percentage_3), # our colour palette function
              fillOpacity = 0.7, # the opacity of the fill colour
              color = "#2e2e30", # colour of shape outlines
              weight = 2,
              popup = paste0("percentage error: ", ShapeFile@data$percentage_3)) %>% # thickness of the shape outlines
  leaflet::addLegend("bottomright", pal = colourpal3, values = ~percentage_3,
            title = "Coding discrepancy 3",
            labFormat = labelFormat(suffix = " %"),
            opacity = 1)

error4map <- leaflet(ShapeFile, options = leafletOptions(zoomControl = FALSE,
                                                         dragging = FALSE)) %>%
  leaflet::addPolygons(fillColor = ~colourpal4(percentage_4), # our colour palette function
              fillOpacity = 0.7, # the opacity of the fill colour
              color = "#2e2e30", # colour of shape outlines
              weight = 2,
              popup = paste0("percentage error: ", ShapeFile@data$percentage_4)) %>% # thickness of the shape outlines
  leaflet::addLegend("bottomright", pal = colourpal4, values = ~percentage_4,
            title = "Coding discrepancy 4",
            labFormat = labelFormat(suffix = " %"),
            opacity = 1)


error5map <- leaflet(ShapeFile, options = leafletOptions(zoomControl = FALSE,
                                                         dragging = FALSE)) %>%
  leaflet::addPolygons(fillColor = ~colourpal5(percentage_5), # our colour palette function
              fillOpacity = 0.7, # the opacity of the fill colour
              color = "#2e2e30", # colour of shape outlines
              weight = 2,
              popup = paste0("percentage error: ", ShapeFile@data$percentage_5)) %>% # thickness of the shape outlines
  leaflet::addLegend("bottomright", pal = colourpal5, values = ~percentage_5,
            title = "Coding discrepancy 5",
            labFormat = labelFormat(suffix = " %"),
            opacity = 1)

error6map <- leaflet(ShapeFile, options = leafletOptions(zoomControl = FALSE,
                                                         dragging = FALSE)) %>%
  leaflet::addPolygons(fillColor = ~colourpal6(percentage_6), # our colour palette function
              fillOpacity = 0.7, # the opacity of the fill colour
              color = "#2e2e30", # colour of shape outlines
              weight = 2,
              popup = paste0("percentage error: ", ShapeFile@data$percentage_6)) %>% # thickness of the shape outlines
  leaflet::addLegend("bottomright", pal = colourpal6, values = ~percentage_6,
            title = "Coding discrepancy 6",
            labFormat = labelFormat(suffix = " %"),
            opacity = 1)

query_map <- leaflet(ShapeFile, options = leafletOptions(zoomControl = FALSE,
                                                         dragging = FALSE)) %>%
  leaflet::addPolygons(fillColor = ~colourpal7(query_count), # our colour palette function
              fillOpacity = 0.7, # the opacity of the fill colour
              color = "#2e2e30", # colour of shape outlines
              weight = 2,
              popup = paste0("percentage error: ", ShapeFile@data$query_count)) %>% # thickness of the shape outlines
  leaflet::addLegend("bottomright", pal = colourpal7, values = ~query_count,
            title = "Coding discrepancy 6",
            labFormat = labelFormat(suffix = " %"),
            opacity = 1)
