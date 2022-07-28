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
library(DT)

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
                                   img(src = "oasis-logo-v1.png", height = "250px")
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
                                   box(width=12, background = "purple",
                                       radioButtons("filtering", 
                                                    "Filter by Gender", 
                                                    choices = c("No Filter" = "no", 
                                                                "Females" = "F", 
                                                                "Males" = "M"), 
                                                    selected = "no")
                                   ),
                                   
                                   box(width=12,background="purple",
                                       selectInput("plottype", 
                                                   "Choose a Type of Plot:", 
                                                   c("Bar Chart" = "bar", 
                                                     "Histogram" = "hist", 
                                                     "Box Plot" = "box")),
                                       
                                       conditionalPanel(condition = "input.plottype == 'bar'",
                                                        radioButtons("pickvar1", 
                                                                     "Choose a variable:", 
                                                                     c("M/F", "Educ"))),
                                       
                                       conditionalPanel(condition = "input.plottype == 'hist'",
                                                        radioButtons("pickvar2", 
                                                                     "Choose a variable:", 
                                                                     c("eTIV", "nWBV", "ASF"))),
                                       
                                       conditionalPanel(condition = "input.plottype == 'box'",
                                                        radioButtons("pickvar3", 
                                                                     "Choose a variable:", 
                                                                     c("eTIV", "nWBV", "Age")))
                                   ),
                                   box(width=12,
                                       background="purple",
                                       
                                       selectInput("summtype", 
                                                   "Choose a Type of Summary:", 
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
                                       h5("The model selection process first involves splitting the data into two sections: a testing subset and a training subset. Usually the data are split based on a proportion (eg. 70:30), with the training set being larger. This app will allow the user to choose the porportion to use for the training set with the slider in the Model Fitting tab. The purpose of splitting the data is to model the data using the training set and then see how well that model performs when used on other (testing) dataset."),
                                       br(),
                                       h5("The original values for CDR were 0 = Normal Cognition, 0.5 = Questionable Dementia, 1 = Mild Cognitive Impairment, and 2 = Moderate Cognitive Impairment. For purposes of this project, the outcome variable, CDR, will be grouped into a bivariate format: 0 = Normal Cognition, and 1 = Some Impairment. The goal of this app is to allow the user to fit three models in an attempt to predict the binned Critical Dementia Rating. Can we predict if someone will have normal cognition or some impairment using the tools provided by this data?")),
                                   
                                   box(background="purple",width=12,
                                       title = "Generalized Linear Model",
                                       box(background = "purple", width = 4, 
                                           title = "Benefits",
                                       h5("Since the outcome variable of interest in binary (0 = NC, 1 = SI), in order to perform linear regression, we must use the generalized linear appreach. GLM allows for non-normal response distributions (such as binary), and performs well with both continuous and categorical predictors.") 
                                       ),
                                       box(background = "purple", width = 4, 
                                           title = "About",
                                           h5("GLMs are linear models that use a ", strong("logit"), ", or ", em("link"), ", function to connect the response to the linear function of the parameters in the model. In this case, the binomial distribution has the logit link function: $$X\\beta = \\ln\\left(\\frac{\\mu}{1-\\mu}\\right), $$ where $$\\mu = \\frac{\\exp(X\\beta)}{1+\\exp(X\\beta)}.$$")
                                       ),
                                       box(background = "purple", width = 4, 
                                           title = "Drawbacks",
                                           h5("One drawback is we have to know or guess the distribution of the response variable in order to choose the correct link function. This can be difficult in certain circumstances. Another drawback is that these models are sensitive to outliers. Strong outliers may pull the entire model towards them and disrupt the fit."))),
                                   
                                   box(background="purple",width=12,
                                       title = "Classification Tree",
                                       box(background = "purple", width = 4, 
                                           title = "Benefits",
                                           h5("Trees have quite a few perks:"),
                                           h5("- They are easy to understand and interpret"),
                                           h5("- It is not necessary to scale the predictors"),
                                           h5("- Trees have automatic variable selection"),
                                           h5("- No statistical assumptions!")
                                       ),
                                       box(background = "purple", width = 4, 
                                           title = "About",
                                           h5("Classification trees provide another way to predict/classify the outcome based on predictor variables. The process involves splitting the predictor space into regions and making a prediction for each region. This split can be made using several different methods, two of which include minimizing either the Gini index or Deviance. $$Gini: 2p(1-p)$$ $$Deviance: -2p\\log(p)-2(1-p)\\log(1-p),$$ where", em("p = P"), "(correct classification). For classification, the most prevalent class of the outcome variable within that region (using the training set) will be the prediction for that region. This split and decision is made several times to create a large tree. Once the tree is made, overfitting can be avoided by using the misclassification rate to prune the tree. ")
                                       ),
                                       box(background = "purple", width = 4, 
                                           title = "Drawbacks",
                                           h5("There's always a catch:"),
                                           h5("- Slight changes in the data may cause a big change in the model"),
                                           h5("- The algorithm does not look ahead (greedy) and is not the most optimal"),
                                           h5("- Pruning is typically necessary and is just another step to perform")
                                           )),
                                   
                                   box(background="purple",width=12,
                                       title = "Random Forest",
                                       box(background = "purple", width = 4, 
                                           title = "Benefits",
                                           h5("In comparison to the basic classification tree models, Random Forest models:"),
                                           h5("- Average across many fitted trees, allowing for a better prediction"),
                                           h5("- Variance over an individually fitted tree is decreased"),
                                           h5("- a strong predictor will not dominate splits, since not all predictors are being used for each boostrapping sample")
                                       ),
                                       box(background = "purple", width = 4, 
                                           title = "About",
                                           h5("Random Forest models are a special case of bagging models. In general, the steps are as follows:"),
                                           h5("1. a bootstrap sample of a random subset of predictors of size $$m=\\sqrt{p}$$ is created from the observations in the data"),
                                           h5("2. a tree is fit to this sample and a prediction for values of ", em("x"), " is saved as $$\\hat{y}^{*1}(x).$$ This process is repeated ", em("B"), " times."),
                                           h5("3. The trees are then averaged to find the final predicted classification"),
                                           h5("MSE is then used to judge performance. $$MSE = \\frac{1}{\\mbox{num of test obs}} \\sum_{i=1}^{\\mbox{num of test obs}}(y_i-\\hat{y}_i)^2$$")
                                       ),
                                       box(background = "purple", width = 4, 
                                           title = "Drawbacks",
                                           h5("The biggest drawback is the loss of interpretability, however one can still investigate importance of variables"))
                                       
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
                                       
                                       checkboxGroupInput("vargroup1", 
                                                          "Choose which variables to include in the model:",
                                                          choices = list("gender", 
                                                                         "Age", 
                                                                         "Educ", 
                                                                         "SES", 
                                                                         "MMSE", 
                                                                         "nWBV",
                                                                         "eTIV",
                                                                         "ASF"),
                                                          selected = list("gender", 
                                                                          "Age", 
                                                                          "Educ", 
                                                                          "SES", 
                                                                          "MMSE", 
                                                                          "nWBV",
                                                                          "eTIV",
                                                                          "ASF")),
                                       numericInput("lmk", 
                                                    "Select a number for cross validation:", 
                                                    value = 10, min = 5, max = 20, step = 1),
                                       checkboxInput("preprocesslm", 
                                                     "Do you want to use PreProcess = c('center', 'scale')?", 
                                                     value = TRUE)
                                        ),
                                   box(background="purple",width=12,
                                       title = "Classification Tree",
                                       checkboxGroupInput("vargroup2", 
                                                          "Choose which variables to include in the model:",
                                                          choices = list("gender", 
                                                                         "Age", 
                                                                         "Educ", 
                                                                         "SES", 
                                                                         "MMSE", 
                                                                         "nWBV",
                                                                         "eTIV",
                                                                         "ASF"),
                                                          selected = list("gender", 
                                                                          "Age", 
                                                                          "Educ", 
                                                                          "SES", 
                                                                          "MMSE", 
                                                                          "nWBV",
                                                                          "eTIV",
                                                                          "ASF")),
                                       numericInput("treek", 
                                                    "Select a number for cross validation:", 
                                                    value = 10, min = 5, max = 20, step = 1),
                                       checkboxInput("preprocesstree", 
                                                     "Do you want to use PreProcess = c('center', 'scale')?", 
                                                     value = TRUE)
                                   ),
                                   box(background="purple",width=12,
                                       title = "Random Forest",
                                       checkboxGroupInput("vargroup3", 
                                                          "Choose which variables to include in the model:",
                                                          choices = list("gender", 
                                                                         "Age", 
                                                                         "Educ", 
                                                                         "SES", 
                                                                         "MMSE", 
                                                                         "nWBV",
                                                                         "eTIV",
                                                                         "ASF"),
                                                          selected = list("gender", 
                                                                          "Age", 
                                                                          "Educ", 
                                                                          "SES", 
                                                                          "MMSE", 
                                                                          "nWBV",
                                                                          "eTIV",
                                                                          "ASF")),
                                       numericInput("rfk", 
                                                    "Select a number for cross validation:", 
                                                    value = 10, min = 5, max = 20, step = 1)
                                       
                                       
                                   ),
                                   box(background="purple",width=12,
                                       title = "Click the button to run the models",
                                       actionButton("B",h3("Ready!"), width = '100%')
                                       
                                   )
                            ),
                            column(9,
                                   #Description of App
                                   h1("Fitting the Models"),
                                   
                                   box(background="purple",width=12,
                                       title = "Linear Regression Model",
                                       verbatimTextOutput("glmsummary")),
                                   
                                   box(background="purple",width=12,
                                       title = "Classification Tree",
                                       plotOutput("treeplot")),
                                   
                                   box(background="purple",width=12,
                                       title = "Random Forest",
                                       plotOutput("rfplot")),
                                   
                                   box(background="purple",width=6,
                                       title = "Compare Accuracies on Training set",
                                       tableOutput("accuracies")),
                                   box(background="purple",width=6,
                                       title = "Compare Accuracies on Testing set",
                                       tableOutput("comptab")),
                                   
                                   box(background="purple",width=12,
                                       textOutput("comparing"))
                          )
                  )), # start next tab here
                  tabItem(tabName = "pred",
                          fluidRow(
                            #add in latex functionality if needed
                            withMathJax(),
                            h1("Prediction"),
                            #two columns for each of the two items
                            column(3,
                                   box(background = "purple", width = 12,
                                       
                                       selectInput("pickmodel", 
                                                   "Choose a model:", 
                                                   choices = c("GLM" = "glm", 
                                                               "Classification Tree" = "tree", 
                                                               "Random Forest" = "rf")),
                                       
                                       selectInput("pickgender", 
                                                   "Choose gender:", 
                                                   choices = c("F", "M"), 
                                                   selected = "F"),
                                       
                                       numericInput("pickage", 
                                                    "Choose an age value:", 
                                                    min = 33, max = 96, step = 1, value = 75),
                                       
                                       numericInput("pickeduc", 
                                                    "Choose a level of education:", 
                                                    min = 1, max = 5, step = 1, value = 3),
                                       
                                       numericInput("pickses", 
                                                    "Choose a level of SES:", 
                                                    min = 1, max = 5, step = 1, value = 3),
                                       
                                       numericInput("pickMMSE", 
                                                    "Choose a level of MMSE:", 
                                                    min = 15, max = 30, step = 1, value = 20),
                                       
                                       numericInput("picknWBV", 
                                                    "Choose a level of nWBV:", 
                                                    min = 0.644, max = 0.841, step = 0.05, value = 0.7),
                                       
                                       numericInput("pickeTIV", 
                                                    "Choose a level of eTIV:", 
                                                    min = 1123, max = 1992, step = 5, value = 1500),
                                       
                                       numericInput("pickASF", 
                                                    "Choose a level of ASF:", 
                                                    min = 0.881, max = 1.563, step = 0.05, value = 1),
                                       
                                       actionButton("B2",h3("Click to predict"), width = '100%')
                                       
                                       )),
                                   
                            
                            column(9,
                                   box(background = "purple", width = 12,
                                       h5(textOutput("predict")))
                                   
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
                                   h1("The Data"),
                                   #box to contain description
                                   box(background="purple",width=6,
                                       checkboxGroupInput("datachoosevars", 
                                                          "Pick what variables to include:", 
                                                          choices = c("ID" = "ID", 
                                                                      "Gender" = "M/F",
                                                                      "Hand" = "Hand",
                                                                      "Age" = "Age",
                                                                      "Education" = "Educ",
                                                                      "Socioeconomic Status" = "SES",
                                                                      "Mini Mental State Examination" = "MMSE",
                                                                      "Clinical Dementia Rating" = "CDR",
                                                                      "Estimated Total Intracranial Volume" = "eTIV",
                                                                      "Normalize Whole Brain Volume" = "nWBV",
                                                                      "Atlas Scaling Factor" = "ASF"),
                                                          selected = c("ID" = "ID", 
                                                                       "Gender" = "M/F",
                                                                       "Hand" = "Hand",
                                                                       "Age" = "Age",
                                                                       "Education" = "Educ",
                                                                       "Socioeconomic Status" = "SES",
                                                                       "Mini Mental State Examination" = "MMSE",
                                                                       "Clinical Dementia Rating" = "CDR",
                                                                       "Estimated Total Intracranial Volume" = "eTIV",
                                                                       "Normalize Whole Brain Volume" = "nWBV",
                                                                       "Atlas Scaling Factor" = "ASF"))),
                                   
                                   box(background="purple",width=6,
                                       radioButtons("filtergender", 
                                                    "Filter data by gender?", 
                                                    choices = c("No" = "no", 
                                                                "Females" = "F", 
                                                                "Males" = "M"), 
                                                    selected = "no")
                                       
                                       ),
                                   
                                   box(background="purple",width=6,
                                       title = "Click here to download the dataset",
                                       downloadButton("download", "Download")
                                       ),
                                   
                                   box(background="purple",width=12,
                                       title = "Oasis Data",
                                       dataTableOutput("alldata"), 
                                       style = "height:500px; overflow-y: scroll;overflow-x: scroll;")
                                  )
                          )
                  )
              ###    
                )
              )
)
