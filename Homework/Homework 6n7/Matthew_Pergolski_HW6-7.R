# Homework 6 & 7 (Tied as One Submission as Requested through the Syllabus )
# Matthew L. Pergolski
# IST 707

# Necessary Packages
  library(dplyr)
  library(caret)
  library(rpart)
  library(rpart.plot)
  library(e1071)
  library(naivebayes)
  library(randomForest)
  library(stringr)
    
# HOMEWORK 6
##################################################################################
  
  ## Load Data
    hw6.data.train <- read.csv('/Users/pergolicious/Downloads/train.csv')
    hw6.data.train
    hw6.data.test <-read.csv('/Users/pergolicious/Downloads/test-2.csv')
    hw6.data.test
    dim(hw6.data.train)
    str(hw6.data.train)
    hw6.data.train$label <- as.factor(hw6.data.train$label)
    hw6.data.train$label
    dim(hw6.data.test)
    str(hw6.data.test)
  
  # Split Data
    hw6.split.train <- sample(nrow(hw6.data.train), nrow(hw6.data.train)*.1)
    hw6.split.train
    hw6.split.test <- sample(nrow(hw6.data.test),nrow(hw6.data.test)*.1)
    hw6.split.test
    
    hw6.sub.train <- hw6.data.train[hw6.split.train,]
    hw6.sub.train
    hw6.sub.test <- hw6.data.test[hw6.split.test,]
    hw6.sub.test
  
  # Decision Tree Model
    hw6.dt.train <- rpart(label ~., 
                          data = hw6.sub.train, 
                          method = 'class', 
                          control = rpart.control(cp=0), 
                          minsplit=100, 
                          maxdepth = 10)
    hw6.dt.train
    summary(hw6.dt.train)
    plot(hw6.dt.train)
    View(hw6.dt.train)
    
  # Prediction | Train Data
    hw6.pred.train <- data.frame(predict(hw6.dt.train, hw6.sub.train))
    hw6.pred.train
    View(hw6.pred.train)
    
    hw6.pred.train <- as.data.frame(names(hw6.pred.train[apply(hw6.pred.train, 1, which.max)]))
    hw6.pred.train
    View(hw6.pred.train)
    
    colnames(hw6.pred.train) <- 'Prediction'
    hw6.pred.train
    str(hw6.pred.train)
    
    hw6.pred.train$Num <- substr(hw6.pred.train$Prediction, 2,2)
    hw6.pred.train$Num
    
    hw6.pred.train <- hw6.sub.train %>% 
      bind_cols(hw6.pred.train) %>% 
      select(label, Num) %>% 
      mutate(label = as.factor(label), Num = as.factor(round(as.numeric(Num),0)))
    
    head(hw6.pred.train)
    
  # Confusion Matrix | Train Data
    hw6.cm <- confusionMatrix(hw6.pred.train$label,hw6.pred.train$Num)
    hw6.cm
  
  # Prediction | Test Data
    hw6.pred.test <- data.frame(predict(hw6.dt.train, hw6.sub.test))
    hw6.pred.test
    
    hw6.pred.test <- as.data.frame(names(hw6.pred.test[apply(hw6.pred.test, 1, which.max)]))
    hw6.pred.test
    colnames(hw6.pred.test) <- 'Prediction'
    str(hw6.pred.test)
    
    hw6.pred.test$Num <- substr(hw6.pred.test$Prediction, 2, 2)
    hw6.pred.test$Num
    
    hw6.pred.test
    
  # Overall DF
    hw6.proj.test.df <- data.frame(predict(hw6.dt.train, hw6.data.test))
    hw6.proj.test.df
    str(hw6.proj.test.df)
    View(hw6.proj.test.df)
  
  # NAIVE BAYES
    
    # Classifier
      hw6.nb.train <- naiveBayes(as.factor(label)~., data = hw6.sub.train)
      hw6.nb.train
      
      View(hw6.nb.train)
    
    # Prediction | Train Data Set
      nb.hw6.pred.train <- predict(hw6.nb.train, hw6.sub.train, type = 'class')
      nb.hw6.pred.train
      head(nb.hw6.pred.train)
    
    # Confusion Matrix | Train Data Set
      hw6.cm.1 <- confusionMatrix(nb.hw6.pred.train,as.factor(hw6.sub.train$label))
      hw6.cm.1
    
    # Analysis | Naive Bayes
      nb.hw6.pred.test <- predict(hw6.nb.train, hw6.data.test, type = 'class')
        #nb.hw6.pred.test | TAKES A LONG TIME TO RUN
      nb.hw6.pred.test <- data.frame(nb.hw6.pred.test)
        #nb.hw6.pred.test | TAKES A LONG TIME TO RUN
      colnames(nb.hw6.pred.test)[1] <- 'Label'
      nb.hw6.pred.test$`Image.ID` <- 1:nrow(nb.hw6.pred.test)
      nb.hw6.pred.test <- nb.hw6.pred.test %>% 
        select(Image.ID, Label)
      
      View(nb.hw6.pred.test)
      
    # MODEL COMPARISON
      # Decision Tree
        hw6.cm
      
        # Accuracy 0.8588
        # 95% CI (0.8479, 0.8692)
        # No Information Rate 0.1174
        # P- Value [Acc > NIRT : < 2.2e-16
        #           
        #           Kappa : 0.8431
        
      # Naive Bayes
        hw6.cm.1
        
        # Accuracy 0.5076
        # 95% CI (0.4924, 0.5228)
        # No Information Rate 0.1112
        # P- Value [Acc > NIRI < 2.2e-16
        #           
        #           Kappa 0.4525
      
    # COMMENTARY | HOMEWORK 6 CONCLUSION
    # Not required to submit to Kaggle per syllabus.  The first confusion matrix, dubbed 'hw6.cm' indicated an
    # overall score of "Accuracy : 0.8588" (decision tree) while the second confusion matrix (Naive Bayes)
    # generated a much lower score, that of ~.50 (Accuracy : 0.5076).  If more data was provided, the Naive Bayes
    # algorithm may be able to increase the accuracy score; however, this is often the best 'plea' to 
    # request when working in Data Science -- 'we need more data!'  In summary, if additional data was provided,
    # scores relating to the Naive Bayes algorithm may perform better.
    

# HOMEWORK 7
##################################################################################
  
  # Duplicate Data Generation and Processing Efforts | Distinguishing for HW 7 Use
    hw7.data.train <- hw6.data.train
    hw7.data.train
    hw7.data.test <- hw6.data.test
    hw7.data.test
  
  # Differing Pre-Processing Strategy
    hw7.data.train[is.na(hw7.data.train)] <-0
    n.col <- ncol(hw7.data.train)
    hw7.vars <- list()
    
    for(i in 2:n.col)
    {
      hw7.col.vars <- var(hw7.data.train[[i]])
    }
    
    
    if(hw7.col.vars == 0)
    {
      hw7.vars <- append(hw7.vars, i)
    }
    
    hw7.drop.col <- unlist(hw7.vars)
    
    hw7.data.train <- hw7.data.train[, -hw7.drop.col]
    hw7.data.train
    
  # Split Data
    set.seed(123)
    hw7.split.train <- sample(nrow (hw7.data.train), nrow(hw7.data.train)*.1)
    hw7.split.train
    hw7.sub.train <- hw7.data.train[hw7.split.train,]
    hw7.sub.train
    set.seed(456)
    hw7.split.train <- sample(nrow(hw7.sub.train), nrow(hw7.sub.train)*.1)
    hw7.train <- hw7.sub.train[hw7.split.train,]
    hw7.test <- hw7.sub.train[-hw7.split.train,]
    hw7.train$label <- factor(paste0('X', hw7.train$label), 
                              levels = c('X0', 'X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8', 'X9'))
    hw7.test$label <- factor(paste0('X', hw7.test$label), 
                             levels = c('X0', 'X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8', 'X9'))
  
  # MODELS
    
    # KNN
      tC <- trainControl(method = 'repeatedcv', repeats = 3, classProbs = T)
      knn.train <- train(label ~ ., data = hw7.train, method = 'knn', preProcess = c('center', 'scale'), trControl = tC, metric = 'ROC', tuneLength = 3)
      
      plot(knn.train)
    
    # Prediction | Train Data
      pred.knn <- predict(knn.train, hw7.test, type = 'prob')
      pred.knn <- as.data.frame(pred.knn)
      pred.knn
      
      pred.knn <- data.frame(apply(pred.knn, 1, which.max)-1)
      colnames(pred.knn) <- 'Prediction'
      
      r.knn <- hw7.test %>% 
        select(label) %>% 
        mutate(real = str_remove(label, 'X')) %>% 
        bind_cols(pred.knn) %>% 
        mutate(real = as.factor(real), prediction = as.factor(Prediction))
      
      head(r.knn)
      
      hw.7.cm <- confusionMatrix(r.knn$real, r.knn$prediction)
      hw.7.cm
      
    # SVM
      hw7.svm.train <- svm(label ~., data = hw7.train, type = 'C', kernel = 'linear', cross =3, probability = TRUE)
      hw7.svm.train
      #plot(hw7.svm.train)
      
    # Test
      hw7.pred.svm <- predict(hw7.svm.train, hw7.test, type = 'prob')
      hw7.pred.svm <- as.data.frame(hw7.pred.svm)
      colnames(hw7.pred.svm) <- 'Results'
      View(hw7.svm.train)
      
      
      svm.r <-hw7.test %>%
        select(label) %>%
        bind_cols(hw7.pred.svm) %>%
        mutate(real = factor(as.character(str_remove(label,'X'))), 
               Prediction = factor(as.character(str_remove(Results,'X'))))
      
      head(svm.r)
      
      hw.7.cm.1 <- confusionMatrix(svm.r$real,svm.r$Prediction)
      hw.7.cm.1
      
    # RANDOM FOREST
      random.for <- trainControl(method = 'repeatedcv', number = 3, repeats = 3)
      random.for.train <- train(label ~., data = hw7.train, method = 'rf', 
                                metric = 'Accuracy', 
                                trControl = random.for, 
                                type = 'C')
      random.for.train
      plot(random.for.train)
      
      hw7.pred.random.for <- predict(random.for.train, hw7.test, type = 'prob')
      hw7.pred.random.for <- as.data.frame(hw7.pred.random.for)
      hw7.pred.random.for
      
      hw7.pred.random.for <- data.frame(apply(hw7.pred.random.for, 1, which.max)-1)
      colnames(hw7.pred.random.for) <- 'Prediction'
      hw7.pred.random.for
      
      #hw7.pred.random.for <- data.frame(apply(hw7.pred.random.for, 1, which.max)-1)
      #colnames(hw7.pred.random.for) <- 'Prediction'
      
      random.for.r <- hw7.test %>% 
        select(label) %>% 
        mutate(real = str_remove(label, 'X')) %>% 
        bind_cols(hw7.pred.random.for) %>% 
        mutate(real = as.factor(real), prediction = as.factor(Prediction))
      
      
      head(random.for.r)
      
      random.for.train
      
      summary(random.for.r)
      
      #hw.7.cm.2 <- confusionMatrix(random.for.r$real,random.for.r$Prediction)
      #hw.7.cm.2
      
      
  # COMPARISON OF MODELS
    
    # KNN
      hw.7.cm
      # Accuracy : 0.7521
      # 95% CI (0.738, 0.7658)
      # No Information Rate 0.1698
      # P- Value [Acc > NIR] < 2.2e-16
      # 
      # Kappa 0.7244
    
    # SVM
      hw.7.cm.1
      # Accuracy : 0.8582
      # 95% CI (0.8467, 0.8692)
      # No Information Rate 0.1159
      # P-Value [Acc > NIR] : < 2.2e-16
      # 
      # Kappa 0.8424
    
    # Random Forest
      #hw.7.cm.2
      random.for.train
      # mtry Accuracy Kappa
      # 783 0.7746236 0.7485994
