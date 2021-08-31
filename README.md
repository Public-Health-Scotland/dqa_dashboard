# DQA Dashboard

*Under development* 

The Data Quality Audits dashboard combines data quality metrics produced across data management teams in Public Health Scotland. This dashboard currently focusses on the SMR (Scottish Morbidity Records) dataset with detail on completeness, timeliness, accuracy audits, and investigative work in coding discrepencies/issues. This dashboard will be useful across internal PHS work but also across the NHS and beyond. 

## Project Structure

The project follows a simple structure for maintainability which is outlined below:

```
├── app                     // code for building R shiny app
├── code                    // data pipelining and other supporting code 
├── data                    // data quality data files and outputs
│   ├── maps                // supporting content for maps
├── docs                    // documents relating to development and maintenance
├── dqa_dashboard.Rproj     // the R project file
├── functions               // functions used across the code
├── .gitignore              // gitignore file
├── lookups                 // lookup files
├── README.md               // this
└── www                     // app accessible static content
```

## Contribution & Maintenance

This project has been developed by the Data Quality Audits team in Public Health Scotland. [Mainana Sanjuan](https://github.com/maiana-sanjuan) is the primary developer and contact. A [developer's guide](https://github.com/Public-Health-Scotland/dqa_dashboard/blob/main/docs/development_guide.Rmd) has also been written with specific details on all of the code. For any other contributions, please open an [issue here](https://github.com/Public-Health-Scotland/dqa_dashboard/issues/new). 
