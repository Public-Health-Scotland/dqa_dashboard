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
      tabPanel("Completeness", "Panel two contents",
               fluidRow(
                   column(3,
                          selectInput("hb_in", "Health Board", choices = c("(All)", unique(smr_completeness$hb_name)))
                          ),
                   column(3,
                          selectInput("month_in", "Month", choices = c("(All)", unique(smr_completeness$month_record_inserted)))
                          ),
                   column(3,
                          selectInput("data_item_in", "Data Item", choices = c("(All)", unique(smr_completeness$data_item)))
                          ),
                   column(3,
                          selectInput("percentage_in", "Percentage Complete", choices = c("(All)","0% - 20%", "20% <", "50% <", "80% <", "100%" ))
                   )
                        ),
               fluidRow(
                 tableOutput("completeness_table")
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
                 
                 column(3, selectInput("Healthboard", "Health Board", choices = NULL)
                 ),
                 
                 column(3, selectInput("DataItemName", "Data Item", choices = NULL)
                 )
               ),
               
               fluidRow(
                 tableOutput("audit_data")
               )
      )
    )),
  tabPanel( #at the top of every page to navigate through the entire dashboard, contains tabs for terminology services
    title = "Coding Discrepancies and Issues",
    navlistPanel(
      id = "tabset",
      tabPanel("SMR02 Recording of Diabetes",
               navbarPage('Errors', #has to have a title otherwise will crash
                          tabPanel("Error 1", # one bar in the menu for each error
                                     p(
                                       'Pre-existing diabetes is recorded in SMR02 diabetes value, 
                                       but the recorded ICD10 diabetes code is not ‘pre-existing diabetes.’'
                                     ), selectInput('year1', 'Choose year:', 
                                                    choices = unique(error_1_table$year)),
                                   tableOutput("error_1")),
                          tabPanel("Error 2", 
                                     p(
                                       'Gestational diabetes is recorded in SMR02 diabetes value, 
                                       but the recorded ICD10 diabetes code is not ‘gestational diabetes’.'
                                     ), selectInput('year2', 'Choose year:', 
                                                    choices = unique(error_2_table$year)),
                                   tableOutput("error_2")),
                          tabPanel("Error 3", 
                                     p(
                                       'Diabetes of unspecified onset is recorded in SMR02 diabetes value, 
                                       but the recorded ICD10 diabetes code is not ‘unspecified diabetes in pregnancy.’'
                                     ),
                                   selectInput('year3', 'Choose year:', 
                                               choices = unique(error_3_table$year)),
                                   tableOutput("error_3")),
                          tabPanel("Error 4",
                                   p(
                                       'Patient recorded as NOT having diabetes in SMR02 diabetes value, 
                                       but ICD10 diabetes O24 code is recorded.'
                                     ),
                                   selectInput('year4', 'Choose year:', 
                                               choices = unique(error_4_table$year)),
                                   tableOutput("error_4")),
                          tabPanel("Error 5",
                                   p(
                                       'Mandatory SMR02 diabetes value is not recorded.'
                                     ),                                    
                                   selectInput('year5', 'Choose year:', 
                                               choices = unique(error_5_table$year)),
                                   tableOutput("error_5")),
                          tabPanel("Error 6",
                                     p(
                                       'ICD10 E10-E14 is recorded instead of ICD10 O24.'
                                     ), selectInput('year6', 'Choose year:', 
                                                    choices = unique(error_6_table$year)),
                                     tableOutput("error_6")),
                          tabPanel("Query 1", 
                                   p(
                                       'SMR02 diabetes value recorded as ‘not known’, 
                                       ICD10 diabetes code O24 is recorded.'
                                     ), selectInput('yearQ', 'Choose year:', 
                                                    choices = sort(unique(query_1_table$year))),
                                   tableOutput("query")))),
      tabPanel("SMR01 ICD-10 Symptom R Codes", selectInput('yearR', 'Choose year:', 
                                                           choices = sort(unique(RCodes_table$year))),
                                                             tableOutput("RCodes"))
    ))))
