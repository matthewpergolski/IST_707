# Matthew L. Pergolski | IST 707 | hw 5

# HW5: Use Decision Tree to Solve a Mystery in History
# 
# In this homework assignment, you are going to use the decision tree algorithm to solve the
# disputed essay problem. Last week you used clustering techniques to tackle this problem.
# 
# Organize your report using the following template:
#   
#   Section 1: Data preparation
# 
# You will need to separate the original data set to training and testing data for classification
# experiments. Describe what examples in your training and what in your test data.
# 
# Section 2: Build and tune decision tree models
# 
# First build a DT model using default setting, and then tune the parameters to see if better model
# can be generated. Compare these models using appropriate evaluation measures. Describe and
# compare the patterns learned in these models.
# 
# Section 3: Prediction
# 
# After building the classification model, apply it to the disputed papers to find out the authorship.
# Does the DT model reach the same conclusion as the clustering algorithms did?


#######################################################################

  library(tidyverse)
  library(caret)
  library(rpart)
  library(rpart.plot)
  library(e1071)
  library(tree)


#######################################################################

# Data
  
  
  fed.papes <- read.csv('/Users/pergolicious/Downloads/fedPapers85.csv')
  str(fed.papes)
  
  
#######################################################################

# Pre-Processing
  
  
  non.dispt.papes <- fed.papes[fed.papes$author != 'dispt',]
  str(non.dispt.papes)

  dispt.papes <- fed.papes[fed.papes$author == 'dispt',]
  str(dispt.papes)
  
  f.train <- createDataPartition(y = non.dispt.papes$author, p = 0.7, list = FALSE)
  f.traind
  
  train <- non.dispt.papes[f.train,]
  train
  str(train)
  
  test <- non.dispt.papes[-f.train,]
  test
  str(test)

#######################################################################
  
# MODEL
  
  # MODEL 1
  model.1 <- rpart(formula = author ~. -filename, data = train, method = 'class',
                      control = rpart.control(cp=0))
  
  str(model.1)
  summary(model.1)
  
  model.1.viz <- rsq.rpart(model.1)
  
  model.1.tree <- rpart.plot(model.1)

    


  
  
  
  
  
  
  
  # MODEL 2
  
  
  model.2.1 <- rpart(formula = author ~. -filename, data = train, method = 'class',
                   control = rpart.control(cp = 0, minsplit = 7, maxdepth = 5))
  str(model.2.1)
  summary(model.2.1)
  model.2.1.viz <- rsq.rpart(model.2.1)
  
  
  model.2.2 <- rpart(formula = author ~. -filename, data = train,
                          method = 'class', control = rpart.control(cp = 0, minsplit = 10,
                                                                    maxdepth = 5))
  model.2.2.viz <- rsq.rpart(model.2.2)
  
  model.2.tree <- rpart.plot(model.2.2)
  
  
  
  
  
  
  # MODEL 3
  
  model.3 <- rpart(author ~. -filename, 
                 data = train, 
                 method = 'class', control = rpart.control(cp=0, minsplit = 5, 
                                                           maxdepth = 5))

  
  model.3  
  str(model.3)
  summary(model.3)
  
  model.3.viz <- rsq.rpart(model.3)
  model.3.tree <- rpart.plot(model.3)


  
#######################################################################
  
  
  # RESULTS
  
  predic <- data.frame(predict(model.3, non.dispt.papes = test))
  predic
  
  findings <- predic %>% 
    mutate(results = ifelse(Madison == 1, 
                            'Madison', 
                            ifelse(Hamilton == 1,
                                   'Hamilton', 
                                   ifelse(Jay == 1, 'Jay', 'HM'))))
  findings
  row.names(findings) <- NULL
  findings
  
  results <- predic %>% 
    bind_cols(findings)
  
  results$results <- as.factor(findings$results)
  
  str(results)
  View(results)
  
  View(findings)
  
  done <- confusionMatrix(results$results, results$results)
  done
  