# ST558 Project 3
By: Jordan Tanley

## Purpose

This app allows the user to investigate Alzheimer's Disease through exploratory data analysis and predictive models. The user can choose which graphical and numeric summaries to view, choose which variables and what settings to use in each model (GLM, Classification Tree, Random Forestm), and finally subset the data and even download the subsetted data of their choosing. Dementia and Alzheimer's disease research is growing rapidly every day. This app provides a glimpse into some of that research and allows the user to learn more about some of the variables involved in identifying and/or predicting cognitive impairment. 


## Packages

The necessary packages include:

* [`shiny`](https://shiny.rstudio.com/): Used to create the app  
* [`tidyverse`](https://www.tidyverse.org/): This package loads in several packages that are useful for code organization and making graphs  
* [`shinydashboard`](https://rstudio.github.io/shinydashboard/): Used to format the pages of the app  
* [`gridExtra`](https://cran.r-project.org/web/packages/gridExtra/index.html): Used for modeling  
* [`caret`](https://topepo.github.io/caret/): Used for building models  
* [`DT`](https://rstudio.github.io/DT/): Used to create an HTML widget for displaying the data in the final tab of the app with DataTables  
* [`fontawesome`](https://cran.r-project.org/web/packages/fontawesome/index.html): Used for the icons on the app's tabs  

### Install Packages

`install.packages(c("shiny", "shinydashboard", "fontawesome", "tidyverse", "gridExtra", "caret", "DT"))`

## Run The App

`shiny::runGitHub("project3", "jetanley")`

