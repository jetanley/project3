

library(shiny)
library(tidyverse)
library(shinydashboard)
library(knitr)


shinyServer(function(input, output, session) {
    
  data <- reactive({
    oasis <- na.omit(read_csv("../archive/oasis_cross-sectional.csv"))[-12]
  })
    

  
  output$plot1 <- renderPlot({
    oasis <- data()
    plotchoice <- input$plottype
    
    
    if (plotchoice == "bar"){
      if (input$pickvar1 == "M/F"){
        ggplot(oasis, aes(x = `M/F`)) + geom_bar(aes(fill = as.factor(CDR))) + 
          scale_fill_discrete(name = "Clinical Dementia Rating", 
                              labels = c("NC", "questionable dementia", "MCI", "moderate CI")) +
          labs(title = "Bar Chart for Gender")
      } else if (input$pickvar1 == "Educ"){
        ggplot(oasis, aes(x = Educ)) + geom_bar(aes(fill = as.factor(CDR))) + 
          scale_fill_discrete(name = "Clinical Dementia Rating", 
                              labels = c("NC", "questionable dementia", "MCI", "moderate CI")) +
          labs(title = "Bar Chart for Education Level")
      }
      
    } else if (plotchoice == "hist"){
      if(input$pickvar2 == "eTIV"){
        ggplot(oasis, aes(x = eTIV)) + geom_histogram(bins = 25, aes(fill = as.factor(CDR))) + 
          scale_fill_discrete(name = "Clinical Dementia Rating", 
                              labels = c("NC", "questionable dementia", "MCI", "moderate CI")) +
          labs(title = "Histogram for Estimated Total Intracranial Volume")
      } else if (input$pickvar2 == "nWBV"){
        ggplot(oasis, aes(x = nWBV)) + geom_histogram(bins = 25, aes(fill = as.factor(CDR))) + 
          scale_fill_discrete(name = "Clinical Dementia Rating", 
                              labels = c("NC", "questionable dementia", "MCI", "moderate CI")) +
          labs(title = "Histogram for Normalized Whole Brain Volume")
      } else if (input$pickvar2 == "ASF"){
        ggplot(oasis, aes(x = ASF)) + geom_histogram(bins = 25, aes(fill = as.factor(CDR))) + 
          scale_fill_discrete(name = "Clinical Dementia Rating", 
                              labels = c("NC", "questionable dementia", "MCI", "moderate CI")) +
          labs(title = "Histogram for Atlas Scaling Factor")
      }
      
    } else if (plotchoice == "box"){
      if (input$pickvar3 == "eTIV"){
        ggplot(oasis, aes(x = as.factor(CDR), y = eTIV)) + geom_boxplot() + 
          scale_x_discrete(labels = c("NC", "questionable dementia", "MCI", "moderate CI")) + 
          labs(title = "Box Plot for Estimated Total Intracranial Volume", x = "Clinical Dementia Rating")
      } else if (input$pickvar3 == "nWBV"){
        ggplot(oasis, aes(x = as.factor(CDR), y = nWBV)) + geom_boxplot() + 
          scale_x_discrete(labels = c("NC", "questionable dementia", "MCI", "moderate CI")) + 
          labs(title = "Box Plot for Normalized Whole Brain Volume", x = "Clinical Dementia Rating")
      } else if (input$pickvar3 == "Age"){
        ggplot(oasis, aes(x = as.factor(CDR), y = Age)) + geom_boxplot() + 
          scale_x_discrete(labels = c("NC", "questionable dementia", "MCI", "moderate CI")) + 
          labs(title = "Box Plot for Age", x = "Clinical Dementia Rating")
      }
      
    } 
    
  })
  
  
  output$table1 <- renderTable({
    oasis <- data()
    tabchoice <- input$summtype
    
    if (tabchoice == "tab1"){
      if (input$pickvar4 == "M/F"){
        tab <- table("Gender" = oasis$`M/F`) 
      } else if (input$pickvar4 == "Educ"){
        tab <- table("Education" = oasis$Educ)
      } else if (input$pickvar4 == "SES"){
        tab <- table("SES" = oasis$SES)
      } else if (input$pickvar4 == "CDR"){
        tab <- table("CDR" = oasis$CDR)
      }
    } else if (tabchoice == "tab2"){
      if (input$pickvar5 == "M/F"){
        tab <- table("CDR" = oasis$CDR, "Gender" = oasis$`M/F`)
      } else if (input$pickvar5 == "Educ"){
        tab <- table("CDR" = oasis$CDR, "Education" = oasis$Educ)
      } else if (input$pickvar5 == "SES"){
        tab <- table("CDR" = oasis$CDR, "SES" = oasis$SES)
      }
    } else if (tabchoice == "numsum"){
      if (input$pickvar6 == "Age"){
        tab <- oasis %>%  
                    summarise(Min = min(Age), Q1 = quantile(Age, 0.25), Med = median(Age), 
                    Q3 = quantile(Age, 0.75), Max = max(Age))
      } else if (input$pickvar6 == "eTIV"){
        tab <- oasis %>%  
          summarise(Min = min(eTIV), Q1 = quantile(eTIV, 0.25), Med = median(eTIV), 
                    Q3 = quantile(eTIV, 0.75), Max = max(eTIV))
      } else if (input$pickvar6 == "nWBV"){
        tab <- oasis %>%  
          summarise(Min = min(nWBV), Q1 = quantile(nWBV, 0.25), Med = median(nWBV), 
                    Q3 = quantile(nWBV, 0.75), Max = max(nWBV))
      }
    }
    tab
  })
    

})
