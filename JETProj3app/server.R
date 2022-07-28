

library(shiny)
library(tidyverse)
library(shinydashboard)
library(knitr)
library(gridExtra)
library(caret)
library(tree)
library(DT)


shinyServer(function(input, output, session) {
    
  data <- reactive({
    oasis <- na.omit(read_csv("../archive/oasis_cross-sectional.csv"))[-12]
  })
    

  
  output$plot1 <- renderPlot({
    
    if (input$filtering == "no"){
      oasis <- data() 
    } else {
      oasis <- data() %>% filter(`M/F` == input$filtering)
    }
    
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
    
    
    if (input$filtering == "no"){
      oasis <- data() 
    } else {
      oasis <- data() %>% filter(`M/F` == input$filtering)
    }
    
    
    
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
    
# clean dataset
  oasis3 <- eventReactive(input$B, {
    oasis <- data()
    oasis2 <- oasis %>%
      mutate(CDR2 = as.factor(ifelse(CDR == 0, 0, 1)), gender = as.factor(`M/F`), 
             Educ = as.factor(Educ), SES = as.factor(SES))
    oasis3 <- oasis2[-c(1, 2, 3, 8)]
    
  })
  # create index for testing/training sets
  index <- eventReactive(input$B, {
    oasis3 <- oasis3()
    index <- createDataPartition(oasis3$CDR2, p = input$split, list = FALSE)
  })
  # create training set
  Training <- eventReactive(input$B, {
    oasis3 <- oasis3()
    Training <- oasis3[index(),]
  })
  #create testing set
  Testing <- eventReactive(input$B, {
    oasis3 <- oasis3()
    Testing <- oasis3[-index(),]
  })
  
  
  # fit the glm with selected variables and either with or without centering/scaling
  fitglm <- eventReactive(input$B, {
    Training <- Training()
    
    others <- list(c("CDR2"))
    varvec <- unlist(append(input$vargroup1, others))
    newdata <- Training[, varvec]
    
    if (input$preprocesslm == 1) {
      fitglm <- train(CDR2 ~ ., data = newdata, method = "glm", family = "binomial", 
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "cv", number = input$lmk))
    } else {
      fitglm <- train(CDR2 ~ ., data = newdata, method = "glm", family = "binomial", 
                      trControl = trainControl(method = "cv", number = input$lmk))
    }
  })
  
  # fit classification tree
  treefit <- eventReactive(input$B, {
    Training <- Training()
    
    others <- list(c("CDR2"))
    varvec2 <- unlist(append(input$vargroup2, others))
    newdata2 <- Training[, varvec2]
    
    if (input$preprocesstree == 1) {
      treefit <- train(CDR2 ~ ., data = newdata2, method = "rpart", 
                       preProcess = c("center", "scale"),
                       trControl = trainControl(method = "cv", number = input$treek))
    } else {
      treefit <- train(CDR2 ~ ., data = newdata2, method = "rpart", 
                       trControl = trainControl(method = "cv", number = input$treek))
    }
  })
  
  
  # fit a random forest model
  fitrf <- eventReactive(input$B, {
    Training <- Training()
    
    others <- list(c("CDR2"))
    varvec3 <- unlist(append(input$vargroup3, others))
    newdata3 <- Training[, varvec3]
    
    
    fitrf <- train(CDR2 ~ ., data = newdata3, method = "rf", 
                   trControl = trainControl(method = "cv", number = input$rfk),
                   tuneGrid = expand.grid(mtry = c(1:round(sqrt(ncol(newdata3)-1)))))
    
    
    
  })
  
  
  # table for accuracies
  output$accuracies <- renderTable({
    fitglm <- fitglm()
    treefit <- treefit()
    fitrf <- fitrf()
    
    accglm <- fitglm$results %>% select(Accuracy)
    acctree <- treefit$results %>% filter(cp == treefit$bestTune$cp) %>% select(Accuracy)
    accrf <- fitrf$results %>% filter(mtry == fitrf$bestTune$mtry) %>% select(Accuracy)
    
    accuracyTab <- cbind(accglm, acctree, accrf)
    colnames(accuracyTab) <- list("GLM", "Classification Tree", "Random Forest") 
    
    accuracyTab
  })
  
  # summaries and plots for each model fit
  
  output$glmsummary <- renderPrint({
    summary(fitglm())
  })
  
  output$treeplot <- renderPlot({
    treefit <- treefit()
    
    plot(treefit$finalModel, main = "Classification Tree")
    text(treefit$finalModel, pretty = 0, cex = 0.6)
  })
  
  output$rfplot <- renderPlot({
    varimport <- varImp(fitrf())
    plot(varimport)
  })
  
  
  # compare the models on the testing set
  
  best <- reactive({
    fitglm <- fitglm()
    treefit <- treefit()
    fitrf <- fitrf()
    
    Testing <- Testing()
    
    others <- list(c("CDR2"))
    
    varvec <- unlist(append(input$vargroup1, others))
    newtest <- Testing[, varvec]
    
    varvec2 <- unlist(append(input$vargroup2, others))
    newtest2 <- Testing[, varvec2]
    
    varvec3 <- unlist(append(input$vargroup3, others))
    newtest3 <- Testing[, varvec3]
    
    #predict CDR using testing data
    predglm <- predict(fitglm, newdata = Testing)
    predtree <- predict(treefit, newdata = Testing)
    predrf <- predict(fitrf, newdata = Testing)
    
    # see how we did
    resampglm <- postResample(predglm, obs = Testing$CDR2)
    resamptree <- postResample(predtree, obs = Testing$CDR2)
    resamprf <- postResample(predrf, obs = Testing$CDR2)
    
    Models <- c("GLM", "Classification Tree", "Random Forest")
    comparing <- data.frame(rbind(resampglm, resamptree, resamprf))
    comparisons <- cbind(Models, comparing)
    
    comparisons[,-3]
  })
  # show table comparing
  output$comptab <- renderTable(best())
  
  #output best
  output$comparing <- renderText({
    best <- best() %>% filter(Accuracy == max(Accuracy)) %>% select(Models)
    print(paste0("The model with the highest accuracy on the testing set is ", best))
  })
  
  # get new dataset ready for predicting
  predictdata <- eventReactive(input$B2,{
    
    gender <- as.factor(input$pickgender)
    ASF <- as.double(input$pickASF)
    Age <- as.double(input$pickage)
    Educ <- as.factor(input$pickeduc)
    eTIV <- as.double(input$pickeTIV)
    MMSE <- as.double(input$pickMMSE)
    nWBV <- as.double(input$picknWBV)
    SES <- as.factor(input$pickses)
    
    data <- data.frame(gender, ASF, Age, Educ, eTIV, MMSE, nWBV, SES)
    
    
    
    if (input$pickmodel == "glm") {
      varvec <- unlist(input$vargroup1)
    } else if (input$pickmodel == "tree") {
      varvec <- unlist(input$vargroup2)
    } else {
      varvec <- unlist(input$vargroup3)
    }
    
    predictdata <- data[, varvec]
    
    predictdata
    
  })
  # get binary prediction
  predicting <- eventReactive(input$B2, {
    
    if (input$pickmodel == "glm") {
      fitglm <- fitglm()
      Predicted <- predict(fitglm, newdata = predictdata())
    } else if (input$pickmodel == "tree") {
      treefit <- treefit()
      Predicted <- predict(treefit, newdata = predictdata())
    } else {
      fitrf <- fitrf()
      Predicted <- predict(fitrf, newdata = predictdata())
    }
    
  })
  # get prediction in laymens terms
  output$predict <- renderText({
    if (predicting() == 0) { ans = "Normal Cognition"
    } else {ans = "Some Impairment"}
    
    print(paste0("The Prediction of CDR is ", ans, "."))
  })
  
  
  alldata <- reactive({
    variables <- input$datachoosevars
    
    oasis <- data()
    
    if (input$filtergender == "F") {
      data <- oasis %>% filter(`M/F` == "F") %>% select(variables)
    } else if (input$filtergender == "M") {
      data <- oasis %>% filter(`M/F` == "M") %>% select(variables)
    } else {
      data <- oasis %>% select(variables)
    }
    data

  })
  
  
  
  output$alldata <- DT::renderDataTable({
    DT::datatable(alldata(), options = list(paging = FALSE))
  })
  
  
  output$download <- downloadHandler(
    filename = function(){
      paste("oasisdata-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file){
      write.csv(alldata(), file)
    }
  )
  
  
  
  
  
  

})
