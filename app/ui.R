shinyUI(navbarPage(
  
  title = "Data Quality Dashboard",
  
  theme = "styles.css",
  
  tabPanel( #at the top of every page to navigate through the entire dashboard, contains tabs for home
  
    title = "Home",
    navlistPanel( 
       id = "tabset",
       tabPanel("Summary Data Profile", "Panel one contents"),
       tabPanel("Info Panel", "Panel two contents")
           )
    ),
  
  tabPanel( #at the top of every page to navigate through the entire dashboard, contains tabs for data quality
    
    title = "Data Quality",
    navlistPanel(
      id = "tabset",
      tabPanel("Home", "Panel one contents"),
      
      tabPanel("Completeness", "Panel two contents",
               
              fluidRow(
                column(4, 
                       selectInput("smr_in", "SMR", choices = c("(All)", unique(smr_completeness$smr)))
                          ),
                column(4,
                       selectInput("hb_in", "Health Board", choices = c("(All)", unique(smr_completeness$hb_name)))
                          ),
                column(4,
                       selectInput("data_item_in", "Data Item", choices = NULL)
                          )
                     ),
              
              fluidRow(
                checkboxGroupInput("percentage_in", 
                                   "Completeness percentage range",
                                   choices = list("Above 60% complete" = 1,
                                                  "Between 40% and 60% complete" = 2,
                                                  "Below 40% complete" = 3),
                                   selected = c(1,2,3),
                                   inline = TRUE
                                   )
                    ),
                      
              
              fluidRow(
                #add icon colors
                tags$style(".glyphicon-ok {color:#2BE532}
                          .glyphicon-warning-sign {color:#FFC300}
                          .glyphicon-flag {color:#C70039}"),
                DT::dataTableOutput("completeness_table")
                    )
               ),
      
      tabPanel("Timeliness", "Panel three contents"),
      
      tabPanel("Accuracy Scores from SMR Audits",
               
               fluidRow(
                 column(3,
                        selectInput("SMRaudit", "SMR", choices = c("(All)", unique(smr_audit$audit)))
                 ),
                 column(3, selectInput("Year", "Year", choices = NULL)
                 ),
                 
                 column(3, selectInput("Healthboard", "Health Board", choices = c("(All)",unique(smr_audit$healthboard)))
                 ),
                 
                 column(3, selectInput("DataItemName", "Data Item", choices = NULL)
                 )
               ),
               
               fluidRow(
                 
                 DT::dataTableOutput("audit_data")
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
                                     tabPanel('Table', p(
                                       'Pre-existing diabetes is recorded in SMR02 diabetes value, 
                                       but the recorded ICD10 diabetes code is not ‘pre-existing diabetes.’'
                                     ),
                                              dataTableOutput("error_1")), #dropdown for table
                                     tabPanel('Map', p(
                                       'The table displays error percentages per healthboard. 
                                       Click on a healthboard to see the exact values.'
                                     ),
                                              leafletOutput("error1map", width = "80%", height = 700)),
                                     tabPanel('Error Split', p(
                                       'This table shows what percentage of each coding discrepancy is attributable 
                                       to the actual condition (wrong ICD-10 "O24x" code) and what percentage is due to
                                       the "O24x" code being omitted whatsoever.'
                                     ),
                                              dataTableOutput('split_1'))), # dropdown for map including dimensions
                          navbarMenu("Error 2", 
                                     tabPanel('Table', p(
                                       'Gestational diabetes is recorded in SMR02 diabetes value, 
                                       but the recorded ICD10 diabetes code is not ‘gestational diabetes’.'
                                     ), dataTableOutput("error_2")), 
                                     tabPanel('Map', p(
                                       'The table displays error percentages per healthboard. 
                                       Click on a healthboard to see the exact values.'
                                     ),
                                              leafletOutput("error2map", width = "80%", height = 700)),
                                     tabPanel('Error Split', p(
                                       'This table shows what percentage of each coding discrepancy is attributable 
                                       to the actual condition (wrong ICD-10 "O24x" code) and what percentage is due to
                                       the "O24x" code being omitted whatsoever.'
                                     ),
                                              dataTableOutput('split_2'))),
                          navbarMenu("Error 3", 
                                     tabPanel('Table', p(
                                       'Diabetes of unspecified onset is recorded in SMR02 diabetes value, 
                                       but the recorded ICD10 diabetes code is not ‘unspecified diabetes in pregnancy.’'
                                     ),
                                              dataTableOutput("error_3")), 
                                     tabPanel('Map', p(
                                       'The table displays error percentages per healthboard. 
                                       Click on a healthboard to see the exact values.'
                                     ),
                                              leafletOutput("error3map", width = "80%", height = 700)),
                                     tabPanel('Error Split', p(
                                       'This table shows what percentage of each coding discrepancy is attributable 
                                       to the actual condition (wrong ICD-10 "O24x" code) and what percentage is due to
                                       the "O24x" code being omitted whatsoever.'
                                     ),
                                              dataTableOutput('split_3'))),
                          navbarMenu("Error 4", 
                                     tabPanel('Table', p(
                                       'Patient recorded as NOT having diabetes in SMR02 diabetes value, 
                                       but ICD10 diabetes O24 code is recorded.'
                                     ),
                                              dataTableOutput("error_4")), 
                                     tabPanel('Map', p(
                                       'The table displays error percentages per healthboard. 
                                       Click on a healthboard to see the exact values.'
                                     ),
                                              leafletOutput("error4map", width = "80%", height = 700)),
                                     tabPanel('Error Split', p(
                                       'This table shows what percentage of each coding discrepancy is attributable 
                                       to the actual condition (wrong ICD-10 "O24x" code) and what percentage is due to
                                       the "O24x" code being omitted whatsoever.'
                                     ),
                                              dataTableOutput('split_4'))),  
                          navbarMenu("Error 5", 
                                     tabPanel('Table', p(
                                       'Mandatory SMR02 diabetes value is not recorded.'
                                     ), dataTableOutput("error_5")), 
                                     tabPanel('Map', p(
                                       'The table displays error percentages per healthboard. 
                                       Click on a healthboard to see the exact values.'
                                     ),
                                              leafletOutput("error5map", width = "80%", height = 700)),
                                     tabPanel('Error Split', p(
                                       'This table shows what percentage of records without a hard 
                                       code have and have not the ICD-10 "O24x" code recorded.'
                                     ),
                                              dataTableOutput('split_5'))),
                          navbarMenu("Error 6", 
                                     tabPanel('Table', p(
                                       'ICD10 E10-E14 is recorded instead of ICD10 O24.'
                                     ), dataTableOutput("error_6")), 
                                     tabPanel('Map', p(
                                       'The table displays error percentages per healthboard. 
                                       Click on a healthboard to see the exact values.'
                                     ),
                                              leafletOutput("error6map", width = "80%", height = 700))),
                          navbarMenu("Query 1", 
                                     tabPanel('Table', p(
                                       'SMR02 diabetes value recorded as ‘not known’, 
                                       ICD10 diabetes code O24 is recorded.'
                                     ), dataTableOutput("query")),
                                     tabPanel('Map', p(
                                       'The table displays counts qualifying for query 1 per healthboard. 
                                       Click on a healthboard to see the exact values.'
                                     ),
                                     leafletOutput("querymap", width = "80%", height = 700)))
               )),
      tabPanel("SMR01 ICD-10 Symptom R Codes", "Panel two contents")
    ))))
