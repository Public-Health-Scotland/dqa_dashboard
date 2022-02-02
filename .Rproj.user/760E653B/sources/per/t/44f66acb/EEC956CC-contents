#UI

sb_width <- c(2,10) #sidebar width
b64 <- base64enc::dataURI(file=here::here("www", "phs_logo.png"), 
                          mime = 'image/png') #encoding for phs logo

secure_app(
  shinyUI(  
    navbarPage(
      
      title = div(tags$a(img(src=b64, width=120, alt = "Public Health Scotland logo"), 
                         href= "https://www.publichealthscotland.scot/",
                         target = "_blank"),
                  style = "position: relative; top: -10px;"), 
      
      windowTitle = "Data Quality Dashboard",
      
      header = tags$head(includeCSS(here::here("www", "styles.css"))),
      
      tabPanel( #at the top of every page to navigate through the entire dashboard, contains tabs for home
        
        tags$head(tags$style(HTML(".selectize-input {border: 1px solid #3F3685;}"))), #controls SelectInput boxes border color
        
        title = "Home",
        navlistPanel( 
          id = "tabset",
          widths = sb_width,
          tabPanel("Summary Data Profile", verbatimTextOutput("text1"),
                   leafletOutput('countmap', width = "68%", height = 600)),
          tabPanel("Info", 
                   fluidRow(
                     column(9,
                            tags$b("About the data"),
                            br(),
                            verbatimTextOutput('SMRtext')
                     ),
                     column(3,
                            tags$b("More information about SMR Datasets"),
                            br(),
                            tags$a(href = "https://www.ndc.scot.nhs.uk/Data-Dictionary/SMR-Datasets//SMR00-Outpatient-Attendance/",
                                   "SMR00 Homepage", target = "_blank"
                            ),
                            br(),
                            tags$a(href = "https://www.ndc.scot.nhs.uk/Data-Dictionary/SMR-Datasets//SMR01-General-Acute-Inpatient-and-Day-Case/",
                                   "SMR01 Homepage", target = "_blank"
                            ),
                            br(),
                            tags$a(href = "https://www.ndc.scot.nhs.uk/Data-Dictionary/SMR-Datasets//SMR02-Maternity-Inpatient-and-Day-Case/",
                                   "SMR02 Homepage", target = "_blank"
                            ),
                            br(),
                            tags$a(href = "https://www.ndc.scot.nhs.uk/Data-Dictionary/SMR-Datasets//SMR04-Mental-Health-Inpatient-and-Day-Case/",
                                   "SMR04 Homepage", target = "_blank"
                            ),
                            br(),
                            tags$a(href = "https://www.ndc.scot.nhs.uk/Dictionary-A-Z/",
                                   "Data Dictionary for definitions of SMR data items", target = "_blank"
                            ),
                            br(),
                            br(),
                            tags$b("Data support and monitoring of national datasets"),
                            br(),
                            tags$a(href = "https://www.isdscotland.org/Products-and-Services/Data-Support-and-Monitoring/",
                                   "Data Support and Monitoring Homepage", target = "_blank"
                            ),
                            br(),
                            tags$a(href = "https://www.isdscotland.org/products-and-Services/Data-Support-and-Monitoring/SMR-Completeness/",
                                   "SMR Completeness", target = "_blank"
                            ),
                            br(),
                            tags$a(href = "https://www.isdscotland.org/products-and-Services/Data-Support-and-Monitoring/SMR-Timeliness/",
                                   "SMR Timeliness", target = "_blank"
                            ),
                            br(),
                            br(),
                            tags$b("Information and support on Scottish clinical coding standards"),
                            br(),
                            tags$a(href = "https://www.isdscotland.org/Products-and-Services/Terminology-Services/Clinical-Coding-Guidelines/",
                                   "Scottish clinical coding standards", target = "_blank"
                            ),
                            br(),
                            tags$a(href = "https://www.isdscotland.org/Products-and-Services/Terminology-Services/Coding-Information-for-Analysts/",
                                   "Clinical coding information for data users", target = "_blank"
                            ),
                            br(),
                            br(),
                            tags$b("Information on data quality assessments"),
                            br(),
                            tags$a(href = "https://beta.isdscotland.org/products-and-services/data-quality-assurance/dqa-assessments/",
                                   "Data quality audit and assessment reports", target = "_blank"
                            ),
                            br(),
                            tags$a(href = "https://beta.isdscotland.org/products-and-services/data-quality-assurance/dqa-assessing-accuracy/",
                                   "How data accuracy is assessed on audits", target = "_blank"
                            )
                     )
                   )
          )
        )
      ),
      
      
      tabPanel( #at the top of every page to navigate through the entire dashboard, contains tabs for data quality
        
        title = "Data Quality",
        navlistPanel(
          id = "tabset",
          widths = sb_width,
          
          tabPanel("Completeness", 
                   "This completeness metric portrays the degree to which required data is enterred in SMR datasets 
                   (ie. percentage of completed entries per data item out of the total number of records submitted).",
                   br(),
                   "All of the data items available in this table are recorded on a mandatory basis,
                   except for main_operation which is conditionally mandatory and should only be recorded if an operation has taken place.",
                   br(),
                   br(),
                   fluidRow(column(12,uiOutput("completeness_key"))),
                   br(),
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
                            prettyCheckboxGroup("percentage_in", 
                                                "Completeness percentage range",
                                                choices = list("Above 60% complete" = 1,
                                                               "Between 40% and 60% complete" = 2,
                                                               "Below 40% complete" = 3),
                                                status = "primary",
                                                shape = "curve",
                                                bigger = TRUE,
                                                selected = c(1,2,3),
                                                inline = TRUE
                            ),
                            tags$head(tags$style(HTML("#percentage_in :after,
                                                      #percentage_in :before
                                                      {border: 1.5px solid #3F3685}"
                            ) #adds border color to checkboxes
                            )
                            )
                     )
                   ),
                   fluidRow(
                     column(12, align="left", downloadButton("download_completeness", "Download CSV"))
                   ),
                   fluidRow(
                     tags$style(".glyphicon-ok {color:#128716}
                                .glyphicon-warning-sign {color:#C200A1}
                                .glyphicon-flag {color:#C70039}"),
                     column(12, DT::dataTableOutput("completeness_table")
                     )
                     )
                   #add icon colors
                   
                   ),
          
          tabPanel("Timeliness", 
                   "The Scottish Government target for SMR submission to ISD is 6 weeks (42 days) following discharge/transfer/death or clinic attendance.",
                   br(),
                   br(),
                   "The following bullet chart and data give an overview of the number of records submitted by the target date, 
                   as well as the expected number of records from each healthboard.",
                   br(),
                   br(),
                   tabsetPanel(
                     tabPanel("Bullet Chart",
                              fluidRow(
                                column(6,
                                       selectInput("timeliness_smr_in", "SMR", choices = c(unique(timeliness$smr)))
                                ),
                                column(6,
                                       selectInput("timeliness_month_in", "Month", choices = c(unique(timeliness$event_month_name)))
                                )
                              ),
                              
                              fluidRow(
                                column(6,
                                       textOutput("timeliness_mean_on_time")
                                ),
                                column(6,
                                       textOutput("timeliness_mean_late")
                                )
                                
                              ),
                              
                              fluidRow(
                                column(12,
                                       plotlyOutput("timeliness_plot")
                                )
                              )
                     ),
                     tabPanel("Data",
                              fluidRow(
                                column(6,
                                       selectInput("timeliness_smr_in_2", "SMR", choices = c("(All)",unique(timeliness$smr)))
                                ),
                                column(6,
                                       selectInput("timeliness_month_in_2", "Month", choices = c("(All)",unique(timeliness$event_month_name)))
                                )
                              ),
                              fluidRow(
                                column(12, align="left", downloadButton("download_timeliness", "Download CSV"))
                              ),
                              fluidRow(
                                column(12,
                                       DT::dataTableOutput("timeliness_rows")
                                )
                              )
                     )
                   )
          ),
          
          tabPanel("Accuracy Scores from SMR Audits",
                   "Scottish Morbidity Records (SMR) are routinely assessed on audits conducted by Public Health Scotland (PHS). The auditors are asked to assess a sample of records and mark down any errors. An accuracy score is then derived for each data item assessed. The table down below contains accuracy scores from SMR Audit reports published to date.",
                   br(),
                   br(),
                   "More information about SMR audits and the derivation of the accuracy scores can be found on the", 
                   tags$a(href = "https://beta.isdscotland.org/products-and-services/data-quality-assurance/",
                          "PHS Data Quality Assurance homepage.", target= "_blank"),
                   br(),
                   br(),
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
                     column(12, align="left", downloadButton("download_audit", "Download CSV"))
                   ),
                   fluidRow(
                     column(12,
                            DT::dataTableOutput("audit_table")
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
                   p("Diabetes in pregnancy is recorded by two different variables in the SMR02 dataset. 
                     It's coded with an International Classification of Diseases (ICD10) code, and a", 
                     tags$a(href="https://www.ndc.scot.nhs.uk/Dictionary-A-Z/Definitions/index.asp?Search=D&ID=214&Title=Diabetes",
                            "diabetes hard-code", target = "_blank"), 
                     "(ie. the recorder selects a value from a drop-down menu). The following tables provides counts of 
                     SMR02 records where the hard-code value and the ICD10 code present conflicting information."),
                   tabsetPanel(
                     tabPanel("Error 1", # one bar in the menu for each error
                              p(
                                tags$b('Error 1 is defined as: '),'Pre-existing diabetes is hard-coded, 
                                but the recorded ICD10 code is not ‘pre-existing diabetes.’'
                              ), 
                              p(
                                'The denominator in the percentage column is the total number
                                of records with the hard-code for pre-existing diabetes'
                              ),
                              selectInput('year1', 'Choose year:', 
                                          choices = c("(All)",sort(unique(error_1_table$year))),
                                          selected = max(unique(error_1_table$year))
                              ),
                              downloadButton("download_smr02_error1", "Download CSV"),
                              DT::dataTableOutput("error_1")),
                     tabPanel("Error 2", 
                              p(
                                tags$b('Error 2 is defined as: '),'Gestational diabetes is hard-coded, 
                                but the recorded ICD10 code is not ‘gestational diabetes’.'
                              ), 
                              p(
                                'The denominator in the percentage column is the total number
                                of records with the hard-code for gestational diabetes'
                              ),
                              selectInput('year2', 'Choose year:', 
                                          choices = c("(All)",sort(unique(error_2_table$year))),
                                          selected = max(unique(error_2_table$year))
                              ),
                              downloadButton("download_smr02_error2", "Download CSV"),
                              DT::dataTableOutput("error_2")),
                     tabPanel("Error 3", 
                              p(
                                tags$b('Error 3 is defined as: '),'Diabetes of unspecified onset is hard-coded, 
                                but the recorded ICD10 code is not ‘unspecified diabetes in pregnancy.’'
                              ),
                              p(
                                'The denominator in the percentage column is the total number of records with 
                                the hard-code for diabetes of unspecified onset'
                              ),
                              selectInput('year3', 'Choose year:', 
                                          choices = c("(All)",sort(unique(error_3_table$year))),
                                          selected = max(unique(error_3_table$year))
                              ),
                              downloadButton("download_smr02_error3", "Download CSV"),
                              DT::dataTableOutput("error_3")),
                     tabPanel("Error 4",
                              p(
                                tags$b('Error 4 is defined as: '),"The hard-code for 'No diabetes during pregnancy' is present, 
                                but an ICD10 code for diabetes in pregnancy (O24*) is recorded."
                              ),
                              p(
                                "The denominator in the percentage column is the total number of records with 
                                the hard-code for 'No diabetes during pregnancy'."
                              ),
                              selectInput('year4', 'Choose year:', 
                                          choices = c("(All)",sort(unique(error_4_table$year))),
                                          selected = max(unique(error_4_table$year))
                              ),
                              downloadButton("download_smr02_error4", "Download CSV"),
                              DT::dataTableOutput("error_4")),
                     tabPanel("Error 5",
                              p(
                                tags$b('Error 5 is defined as: '),'The mandatory diabetes hard-code is not recorded.'
                              ),
                              selectInput('year5', 'Choose year:', 
                                          choices = c("(All)",sort(unique(error_5_table$year))),
                                          selected = max(unique(error_5_table$year))
                              ),
                              downloadButton("download_smr02_error5", "Download CSV"),
                              DT::dataTableOutput("error_5")),
                     tabPanel("Error 6",
                              p(
                                tags$b('Error 6 is defined as: '),'An ICD10 diabetes code (E10-E14) is recorded instead
                              of an ICD10 diabetes in pregnancy code (O24*).'
                              ), 
                              p(
                                'The denominator in the percentage column is the total number of records 
                                       for the given healthboard and selected year.'
                              ),
                              selectInput('year6', 'Choose year:', 
                                          choices = c("(All)",sort(unique(error_6_table$year))),
                                          selected = max(unique(error_6_table$year))
                              ),
                              downloadButton("download_smr02_error6", "Download CSV"),
                              DT::dataTableOutput("error_6")),
                     tabPanel("Query 1", 
                              p(
                                tags$b('Query 1 is defined as: '),'Diabetes is hard-coded as ‘Not Known’, 
                                        but an ICD10 diabetes in pregnancy code (O24*) is recorded.'
                              ), 
                              p(
                                "The denominator in the percentage column is the total number of records
                                       where diabetes is hard-coded as 'Not Known'."
                              ),
                              selectInput('yearQ', 'Choose year:', 
                                          choices = c("(All)",sort(unique(query_1_table$year))),
                                          selected = max(unique(query_1_table$year))
                              ),
                              downloadButton("download_smr02_query1", "Download CSV"),
                              DT::dataTableOutput("query")))
                     ),
          tabPanel("SMR01 ICD-10 Symptom R Codes",
                   p('This table reports the number of R codes entered as a Main Condition in the last episode of multi-episode stays in SMR01 data. A multi-episode stay is a continuous inpatient stay (CIS) with more than one episode.'),
                   p('R codes are symptomatic codes defined in the International Classification of Diseases (ICD10), this table also reports the counts for the following groupings of R codes'),
                   p('R05*, R06*, R07* - Respiratory and Chest'),
                   p('R10*, R11* - Abdominal Pain and Vomiting'),
                   p('R55*, R56* - Collapse and Convulsions'),
                   selectInput('yearR', 'Choose year:', 
                               choices = c('(All)', sort(unique(RCodes_table$year)))
                   ),
                   downloadButton("download_smr01_rcodes", "Download CSV"),
                   DT::dataTableOutput("RCodes")
          )
        )
      )
    )
  )
) #secure app



