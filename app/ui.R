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
      tabPanel("Accuracy Scores from SMR Audits",
               fluidRow(
                 column(3,
                        selectInput("SMRaudit", "SMR", choices = c("(All)", unique(smr_audit$audit)))
                 ),
                 column(3, selectInput("Year", "Year", choices = NULL)
                 ),
                 
                 column(3, selectInput("Healthboard", "Health Board", choices = NULL)
                 ),
                 
                 column(3, selectInput("DataItemName", "Data Item", choices = NULL)
                 )
               ),
               
               fluidRow(
                 tableOutput("data")
               )
      )
    )),
  tabPanel( #at the top of every page to navigate through the entire dashboard, contains tabs for terminology services
    title = "Coding Discrepancies and Issues",
    navlistPanel(
      id = "tabset",
      tabPanel("SMR02 Recording of Diabetes",
               navbarPage('Errors', #has to have a title otherwise will crash
                          navbarMenu("Error 1", # one bar in the menu for each error
                                     tabPanel('Table', dataTableOutput("error_1")), #dropdown for table
                                     tabPanel('Map', leafletOutput("error1map", width = "50%", height = 700))), # dropdown for map including dimensions
                          navbarMenu("Error 2", 
                                     tabPanel('Table', dataTableOutput("error_2")), 
                                     tabPanel('Map', leafletOutput("error2map", width = "50%", height = 700))),
                          navbarMenu("Error 3", 
                                     tabPanel('Table', dataTableOutput("error_3")), 
                                     tabPanel('Map', leafletOutput("error3map", width = "50%", height = 700))),
                          navbarMenu("Error 4", 
                                     tabPanel('Table', dataTableOutput("error_4")), 
                                     tabPanel('Map', leafletOutput("error4map", width = "50%", height = 700))),  
                          navbarMenu("Error 5", 
                                     tabPanel('Table', dataTableOutput("error_5")), 
                                     tabPanel('Map', leafletOutput("error5map", width = "50%", height = 700))),
                          navbarMenu("Error 6", 
                                     tabPanel('Table', dataTableOutput("error_6")), 
                                     tabPanel('Map', leafletOutput("error6map", width = "50%", height = 700)))
               )),
      tabPanel("SMR01 ICD-10 Symptom R Codes", "Panel two contents")
    ))))
