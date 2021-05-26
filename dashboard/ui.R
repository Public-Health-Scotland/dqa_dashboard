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
    title = "Coding Errors and Issues",
    navlistPanel(
      id = "tabset",
      tabPanel("SMR02 Recording of Diabetes", "Panel one contents"),
      tabPanel("SMR01 ICD-10 Symptom R Codes", "Panel two contents")
  ))))