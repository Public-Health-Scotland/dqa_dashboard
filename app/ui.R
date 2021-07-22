#set sidebar width
 
sb_width <- c(3,9)

shinyUI(navbarPage(
  
  title = "Data Quality Dashboard",
  
  theme = "styles.css",
  
  tabPanel( #at the top of every page to navigate through the entire dashboard, contains tabs for home
  
    title = "Home",
    navlistPanel( 
       id = "tabset",
       widths = sb_width,
       tabPanel("Summary Data Profile", "Panel one contents"),
       tabPanel("Info Panel", "Panel two contents")
           )
    ),
  
  tabPanel( #at the top of every page to navigate through the entire dashboard, contains tabs for data quality
    
    title = "Data Quality",
    navlistPanel(
      id = "tabset",
      widths = sb_width,
      tabPanel("Home", "Panel one contents"),
      
      tabPanel("Completeness", 
               "Completeness percentage per data item, calculated for events taking place in the latest complete month.",
               
              fluidRow(
                column(4, 
                       selectInput("smr_in", "SMR", choices = c("(All)", unique(smr_completeness$smr)))
                ),
                column(4,
                       selectInput("hb_in", "Health Board", choices = c("(All)", unique(smr_completeness$hb_name)))
                ),
                column(4,
                       selectInput("data_item_in", "Data Item", choices = c("(All)",
                                                                            unique(smr_completeness$data_item)))
                )
              ),
              
              fluidRow(
                column(12,
                       checkboxGroupInput("percentage_in", 
                                          "Completeness percentage range",
                                          choices = list("Above 60% complete" = 1,
                                                         "Between 40% and 60% complete" = 2,
                                                         "Below 40% complete" = 3),
                                          selected = c(1,2,3),
                                          inline = TRUE
                       )
                )
              ),
                      
              
              fluidRow(
                tags$style(".glyphicon-ok {color:#2BE532}
                          .glyphicon-warning-sign {color:#FFC300}
                          .glyphicon-flag {color:#C70039}"),
                column(12, DT::dataTableOutput("completeness_table")
                )
              )
                #add icon colors
                
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
                 column(12,
                        DT::dataTableOutput("audit_data")
                  )
               )
      )
    )),
  tabPanel( #at the top of every page to navigate through the entire dashboard, contains tabs for terminology services
    title = "Coding Discrepancies and Issues",
    navlistPanel(
      id = "tabset",
      widths = sb_width,
      tabPanel("SMR02 Recording of Diabetes",
               navbarPage('Errors', #has to have a title otherwise will crash
                          tabPanel("Error 1", # one bar in the menu for each error
                                     p(
                                       'Pre-existing diabetes is recorded in SMR02 diabetes value, 
                                       but the recorded ICD10 diabetes code is not ‘pre-existing diabetes.’'
                                     ), selectInput('year1', 'Choose year:', 
                                                    choices = unique(error_1_table$year)),
                                   DT::dataTableOutput("error_1")),
                          tabPanel("Error 2", 
                                     p(
                                       'Gestational diabetes is recorded in SMR02 diabetes value, 
                                       but the recorded ICD10 diabetes code is not ‘gestational diabetes’.'
                                     ), selectInput('year2', 'Choose year:', 
                                                    choices = unique(error_2_table$year)),
                                   DT::dataTableOutput("error_2")),
                          tabPanel("Error 3", 
                                     p(
                                       'Diabetes of unspecified onset is recorded in SMR02 diabetes value, 
                                       but the recorded ICD10 diabetes code is not ‘unspecified diabetes in pregnancy.’'
                                     ),
                                   selectInput('year3', 'Choose year:', 
                                               choices = unique(error_3_table$year)),
                                   DT::dataTableOutput("error_3")),
                          tabPanel("Error 4",
                                   p(
                                       'Patient recorded as NOT having diabetes in SMR02 diabetes value, 
                                       but ICD10 diabetes O24 code is recorded.'
                                     ),
                                   selectInput('year4', 'Choose year:', 
                                               choices = unique(error_4_table$year)),
                                   DT::dataTableOutput("error_4")),
                          tabPanel("Error 5",
                                   p(
                                       'Mandatory SMR02 diabetes value is not recorded.'
                                     ),                                    
                                   selectInput('year5', 'Choose year:', 
                                               choices = unique(error_5_table$year)),
                                   DT::dataTableOutput("error_5")),
                          tabPanel("Error 6",
                                     p(
                                       'ICD10 E10-E14 is recorded instead of ICD10 O24.'
                                     ), selectInput('year6', 'Choose year:', 
                                                    choices = unique(error_6_table$year)),
                                   DT::dataTableOutput("error_6")),
                          tabPanel("Query 1", 
                                   p(
                                       'SMR02 diabetes value recorded as ‘not known’, 
                                       ICD10 diabetes code O24 is recorded.'
                                     ), selectInput('yearQ', 'Choose year:', 
                                                    choices = sort(unique(query_1_table$year))),
                                   DT::dataTableOutput("query")))),
      tabPanel("SMR01 ICD-10 Symptom R Codes", 
               selectInput('yearR', 'Choose year:', 
               choices = c('All', sort(unique(RCodes_table$year))),
               DT::dataTableOutput("RCodes"))
    )))))
