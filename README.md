# Data Quality Dashboard

*Under development* 

The Data Quality dashboard combines data quality metrics produced across multi-disciplinary teams in Public Health Scotland. This dashboard currently focusses on the SMR (Scottish Morbidity Records) dataset with detail on completeness, timeliness, accuracy audits, and investigative work in coding discrepencies/issues. This dashboard will be useful across internal PHS work but also across the NHS and beyond. 

## Project Structure

The project follows a simple structure for maintainability which is outlined below:

```
├── app                     // code for building R shiny app
| ├── code                  // data pipelining and other supporting code 
| ├── functions             // functions used across the code
| ├── www                   // app accessible static content
├── docs                    // documents relating to development and maintenance
├── .gitignore              // gitignore file
└── README.md               // this
```

## Contribution & Maintenance

This project has been developed by the teams: Data Quality Assurance, Data Monitoring, and Terminology Services in Public Health Scotland. [Maiana Sanjuan](https://github.com/maiana-sanjuan) is the primary developer and contact. A [developer's guide](https://github.com/Public-Health-Scotland/dqa_dashboard/blob/main/docs/development_guide.Rmd) has also been written with specific details on all of the code. For any other contributions, please open an [issue here](https://github.com/Public-Health-Scotland/dqa_dashboard/issues/new). 
