#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(knitr)
library(gridExtra)
library(caret)
library(tree)

dashboardPage(skin="purple",
              
              #add title
              dashboardHeader(title="Investigation of MRI and Alzheimers",titleWidth=1000),
              
              #define sidebar items
              dashboardSidebar(sidebarMenu(
                menuItem("About", tabName = "about", icon = icon("brain", lib = "font-awesome")),
                menuItem("Data Exploration", tabName = "eda", icon = icon("book-medical", lib = "font-awesome")),
                menuItem("Modeling", tabName = "modeling", icon = icon("chart-line", lib = "font-awesome"),
                         menuSubItem("Modeling Information", tabName = "modinfo", 
                                     icon = icon("microscope", lib = "font-awesome")),
                         menuSubItem("Model Fitting", tabName = "modfit", 
                                     icon = icon("hospital", lib = "font-awesome")),
                         menuSubItem("Prediction", tabName = "pred", 
                                     icon = icon("briefcase-medical", lib = "font-awesome"))
                         ),
                menuItem("Data", tabName = "data", icon = icon("stethoscope", lib = "font-awesome"))
              )),
              # briefcase-medical and file-medical works
              #define the body of the app
              dashboardBody(
                tabItems(
                  # First tab content
                  tabItem(tabName = "about",
                          fluidRow(
                            #add in latex functionality if needed
                            withMathJax(),
                            
                            #two columns for each of the two items
                            column(6,
                                   #Description of App
                                   h1("What does this app do?"),
                                   #box to contain description
                                   box(background="purple",width=12,
                                       h4("This application investigates Alzheimer's Disease through MRI scans and other covariates.")),
                                   box(background="purple",width=12,
                                       h4("Alzheimer's Disease, the most common form of dementia, is a disease which causes brain atrophy. This brain atrophy leads to a decline in a person's ability to live independently, negatively affecting social, thinking and behavioral skills.")),
                                   box(background="purple",width=12,
                                       h4("This application utilizes data from the ",a(" Open Access Series of Imaging Studies (OASIS)", href = "https://www.kaggle.com/datasets/jboysen/mri-and-alzheimers?select=oasis_cross-sectional.csv"), " project - particularly the Cross-sectional MRI Data in Young, Middle Aged, Nondemented and Demented Older Adults Dataset."),
                                       h4("The goal of this app is to use the data present to predict Alzheimer's Disease.")
                                   )
                            ),
                            
                            column(6,
                                   box(width=12,
                                   tags$img(src = "Ch4_L1_Oasis_Logo.png", width = "300px", height = "150px",)
                                   ),

                                   #How to use the app
                                   h1("Breakdown of the Pages"),
                                   #box to contain description
                                   box(background="purple",width=12,
                                       h4(strong("Data Exploration: "), "Here is where you will be able to investigate any potential relationships between the variables, through both numerical and graphical summaries. This is an important first step in investigating any dataset, as understanding relationships prior to modeling allows for making stronger models."),
                                       br(),
                                       h4(strong("Modeling: "), "This tab includes explanations of the models themselves, actually fitting the described models, and finally using a model fitted to predict Alzheimer's Disease."),
                                       br(),
                                       h4(strong("Data: "), "This page is where you will find the data. You can choose to subset and/or download the data directly to your device.")
                                   )
                            )
                          )
                  ),
                  
                  #      
                  tabItem(tabName = "eda",
                          fluidRow(
                            column(width=3,
                                   box(width=12,background="purple",
                                       selectInput("plottype", "Choose a Type of Plot:", 
                                                   c("Bar Chart" = "bar", "Histogram" = "hist", "Box Plot" = "box")),
                                       conditionalPanel(condition = "input.plottype == 'bar'",
                                                        radioButtons("pickvar1", "Choose a variable:", c("M/F", "Educ"))),
                                       conditionalPanel(condition = "input.plottype == 'hist'",
                                                        radioButtons("pickvar2", "Choose a variable:", 
                                                                     c("eTIV", "nWBV", "ASF"))),
                                       conditionalPanel(condition = "input.plottype == 'box'",
                                                        radioButtons("pickvar3", "Choose a variable:", 
                                                                     c("eTIV", "nWBV", "Age")))
                                   ),
                                   box(width=12,
                                       background="purple",
                                       
                                       selectInput("summtype", "Choose a Type of Summary:", 
                                                   c("1-way Contingency Table" = "tab1", 
                                                     "2-way Contingency Table" = "tab2", "5# Summary" = "numsum")),
                                       conditionalPanel(condition = "input.summtype == 'tab1'",
                                                        radioButtons("pickvar4", "Choose a variable:", 
                                                                     c("M/F", "Educ", "SES", "CDR"))),
                                       conditionalPanel(condition = "input.summtype == 'tab2'",
                                                        radioButtons("pickvar5", "Choose a variable:", 
                                                                     c("M/F", "Educ", "SES"))),
                                       conditionalPanel(condition = "input.summtype == 'numsum'",
                                                        radioButtons("pickvar6", "Choose a variable:", 
                                                                     c("eTIV", "nWBV", "Age")))
              
                                   )
                            ),
                            column(width=9,
                                   fluidRow(
                                     box(width=12,
                                         title = "Graphical Summary of Your Choice",
                                         plotOutput("plot1")
                                     ),
                                     box(width=12,
                                         title = "Numerical Summary of Your Choice",
                                         tableOutput("table1")
                                         
                                         
                                     )
                                   )
                            )
                          )
                  ),
                  tabItem(tabName = "modinfo",
                          fluidRow(
                            #add in latex functionality if needed
                            withMathJax(),
                            
                            #two columns for each of the two items
                            column(12,
                                   #Description of App
                                   h1("About the Models"),
                                   #box to contain description
                                   box(background="purple",width=12,
                                       title = "Preparing the Data",
                                       h5("testing/training stuff")),
                                   box(background="purple",width=12,
                                       title = "Linear Regression Model",
                                       h5("Stuff")),
                                   box(background="purple",width=12,
                                       title = "Classification Tree",
                                       h5("Stuff")),
                                   box(background="purple",width=12,
                                       title = "Random Forest",
                                       h5("wow so cool")
                                       
                                   )
                            )
                          )
                  ),
                  tabItem(tabName = "modfit",
                          fluidRow(
                            #add in latex functionality if needed
                            withMathJax(),
                            
                            #two columns for each of the two items
                            column(3,
                                   box(background="purple",width=12,
                                       sliderInput("split", "Choose what porportion to use for splitting the data:", 
                                                   min = 0, max = 1, step = 0.05, value = 0.75)
                                       
                                   ),
                                   box(background="purple",width=12,
                                       title = "Linear Regression Model",
                                       
                                       checkboxGroupInput("vargroup1", "Choose which variables to include in the model:",
                                                          choices = list("gender", "Age", "Educ", "SES", "MMSE", "nWBV"),
                                                          selected = list("gender", "Age", "Educ", "SES", "MMSE", "nWBV")),
                                       numericInput("lmk", "Select a number for cross validation:", value = 10, min = 5, max = 20, step = 1),
                                       checkboxInput("preprocesslm", "Do you want to use PreProcess = c('center', 'scale')?", value = TRUE)
                                   ),
                                   box(background="purple",width=12,
                                       title = "Classification Tree",
                                       checkboxGroupInput("vargroup2", "Choose which variables to include in the model:",
                                                          choices = list("gender", "Age", "Educ", "SES", "MMSE", "nWBV"),
                                                          selected = list("gender", "Age", "Educ", "SES", "MMSE", "nWBV")),
                                       numericInput("treek", "Select a number for cross validation:", value = 10, min = 5, max = 20, step = 1),
                                       checkboxInput("preprocesstree", "Do you want to use PreProcess = c('center', 'scale')?", value = TRUE)
                                   ),
                                   box(background="purple",width=12,
                                       title = "Random Forest",
                                       checkboxGroupInput("vargroup3", "Choose which variables to include in the model:",
                                                          choices = list("gender", "Age", "Educ", "SES", "MMSE", "nWBV"),
                                                          selected = list("gender", "Age", "Educ", "SES", "MMSE", "nWBV")),
                                       numericInput("rfk", "Select a number for cross validation:", value = 10, min = 5, max = 20, step = 1),
                                       checkboxInput("preprocessrf", "Do you want to use PreProcess = c('center', 'scale')?", value = TRUE)
                                       
                                   ),
                                   box(background="purple",width=12,
                                       title = "Click the button to run the models",
                                       actionButton("B",h3("Ready!"), width = '100%')
                                       
                                   )
                            ),
                            column(9,
                                   #Description of App
                                   h1("Fitting the Models"),
                                   #box to contain description
                                   box(background="purple",width=12,
                                       title = "Preparing the Data",
                                       h5("testing/training stuff")),
                                   box(background="purple",width=12,
                                       title = "Linear Regression Model",
                                       verbatimTextOutput("glmfit"),
                                       h5("Stuff")),
                                   box(background="purple",width=12,
                                       title = "Classification Tree",
                                       plotOutput("treefit")),
                                   box(background="purple",width=12,
                                       title = "Random Forest",
                                       h5("wow so cool")
                                       
                                   )
                            )
                          )
                  ), # start next tab here
                  tabItem(tabName = "pred",
                          fluidRow(
                            #add in latex functionality if needed
                            withMathJax(),
                            
                            #two columns for each of the two items
                            column(3,
                                   #Description of App
                                   h1("Prediction"),
                                   #box to contain description
                                   box(background="purple",width=12,
                                       title = "Preparing the Data",
                                       h5("testing/training stuff")),
                                   
                                   box(background="purple",width=12,
                                       title = "Random Forest",
                                       h5("wow so cool")
                                   )
                            ),
                            column(9,
                                   #Description of App
                                   h1("Prediction"),
                                   #box to contain description
                                   box(background="purple",width=12,
                                       title = "Preparing the Data",
                                       h5("testing/training stuff")),
                                   
                                   box(background="purple",width=12,
                                       title = "Random Forest",
                                       h5("wow so cool")
                                   )
                            )
                          )
                  ),
                  tabItem(tabName = "data",
                          fluidRow(
                            #add in latex functionality if needed
                            withMathJax(),
                            
                            #two columns for each of the two items
                            column(12,
                                   #Description of App
                                   h1("About the Models"),
                                   #box to contain description
                                   box(background="purple",width=12,
                                       title = "Preparing the Data",
                                       h5("testing/training stuff")),
                                   box(background="purple",width=12,
                                       title = "Linear Regression Model",
                                       h5("Stuff")),
                                   box(background="purple",width=12,
                                       title = "Classification Tree",
                                       h5("Stuff")),
                                   box(background="purple",width=12,
                                       title = "Random Forest",
                                       h5("wow so cool")
                                       
                                   )
                            )
                          )
                  )
                  
                )
              )
)
