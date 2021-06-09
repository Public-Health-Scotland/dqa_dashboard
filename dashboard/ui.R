shinyUI(navbarPage(
  title = "Data Quality Dashboard",
  theme = "styles.css",
  tabPanel( #at the top of every page to navigate through the entire dashboard, contains tabs for home
    title = "Home",
    navlistPanel( 
       id = "tabset",
       tabPanel("Summary Data Profile", "Panel one contents"),
       tabPanel("Info Panel", "Panel two contents")
           )),
  tabPanel( #at the top of every page to navigate through the entire dashboard, contains tabs for data quality
    title = "Data Quality",
    navlistPanel(
      id = "tabset",
      tabPanel("Home", "Panel one contents"),
      tabPanel("Completeness", "Panel two contents"),
      tabPanel("Timeliness", "Panel three contents"),
      tabPanel("Accuracy Scores from SMR Audits", dataTableOutput("gapminder_table"))
  )),
  tabPanel( #at the top of every page to navigate through the entire dashboard, contains tabs for terminology services
    title = "Coding Discrepancies and Issues",
    navlistPanel(
      id = "tabset",
      tabPanel("SMR02 Recording of Diabetes",
               tabsetPanel(
                 tabPanel("Error 1", dataTableOutput("error_1")), 
                 tabPanel("Error 2", dataTableOutput("error_2")),
                 tabPanel("Error 3", dataTableOutput("error_3")),
                 tabPanel("Error 4", dataTableOutput("error_4")),
                 tabPanel("Error 5", dataTableOutput("error_5")),
                 tabPanel("Error 6", dataTableOutput("error_6")),
                 tabPanel("Map error 1", leafletOutput("error1map", width = "40%", height = 600))
                 )),
      tabPanel("SMR01 ICD-10 Symptom R Codes", "Panel two contents")
  ))))