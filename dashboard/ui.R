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
                        selectInput("SMRaudit", "SMR", choices = unique(hb_mean$Audit))
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
    title = "Coding Errors and Issues",
    navlistPanel(
      id = "tabset",
      tabPanel("SMR02 Recording of Diabetes", "Panel one contents"),
      tabPanel("SMR01 ICD-10 Symptom R Codes", "Panel two contents")
  ))))

#dataTableOutput("gapminder_table")






### Nested filters test in separate UI --------------------------------------

# ui <- fluidPage(
#   
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("SMRaudit", "SMR", choices = unique(hb_mean$Audit)),
#       selectInput("Year", "Year", choices = NULL),
#       selectInput("Healthboard", "Health Board", choices = NULL),
#       selectInput("DataItemName", "Data Item", choices = NULL)
#     ),
#     mainPanel(
#       tableOutput("data")
#     )
#   )
# )




