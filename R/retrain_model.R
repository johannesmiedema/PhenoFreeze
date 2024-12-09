
#' Retraining utility of the PhenoFreeze Package to add more data to the training
#'
#' @param new_data dataframe containing new freezing values MR1 of bins 13 - 24 as a character
#' @param sex "female" or "male" specifying the sex of the new data
#' @param MR 1 or 2 specifying the Memory Retrieval Session 1 or 2 of the new data
#' @param training_iterations description
#' @import mclust
#' @import e1071
#' @import pROC
#' @import caret
#' @import randomForest
#' @import MASS
#' @import reshape2
#' @export retrain_model
#' @export


retrain_model <- function(new_data, sex, MR, training_iterations = 1000){

  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("Make sure the  package ggplot2 is installed before calling retrain_model()")

  #Parameter Validation TBD

  #####################################
  #Perform loglinear regression of new Data and collect variables for Re-clustering

  # 12 time bins during tone was played
  #Initialize variables
  bins <- 1:12
  beta_new <- 0
  int_new <- 0
  #Calculate average freezing of each animal during sustained phase
  freeze_new <- rowMeans(new_data[,6:12])

  #Fit regression model for each animal
  for (i in 1:nrow(new_data)){
    #transpose x and y
    y <- t(new_data[i,])
    #pseudocount if y is zero to allow log operation
    if (0 %in% y){y<-y+1}
    #fit loglinear model
    mod.loglinear <- stats::lm(log(y)~bins)
    #obtain beta coefficients
    beta_new[i] <- mod.loglinear$coefficients[2]
    #obtain intercepts
    int_new[i] <- mod.loglinear$coefficients[1]
  }

  #Initialize data frame for regression parameters
  params_new <- data.frame(freeze=freeze_new, beta=beta_new, int=int_new)

  #####################################
  #Combine existing training-dataset with new data

  if(sex == "female" && MR == 1){old_data = training.f.MR1}
  if(sex == "female" && MR == 2){old_data = training.f.MR2}
  if(sex == "male" && MR == 1){old_data = training.m.MR1}
  if(sex == "male" && MR == 2){old_data = training.m.MR2}

  #Reformat previous training data parameters for merging with new data
  old_data <- old_data[,3:5]
  colnames(old_data) <- c("freeze", "beta", "int")

  #Merge with new data
  data_merged <- rbind(old_data, params_new)

  #####################################
  #Reclustering with GMM mclust
  clust <- suppressMessages(mclust::Mclust(data_merged, G=2, verbose = FALSE))

  #Assign sustained and phasic animals based on higher average freezing per cluster
  data <- data.frame(data_merged$freeze, clust$classification)
  colnames(data) <- c("freeze", "cluster")
  mean1 <- mean(data[data$cluster==1,]$freeze)
  mean2 <- mean(data[data$cluster==2,]$freeze)

  if (mean1 < mean2){
    data$cluster <-c("phasic", "sustained")[ match( data$cluster, c(1,2))]

  } else {

    data$cluster <-c("sustained", "phasic")[ match( data$cluster, c(1,2))]
  }

  data_merged$class <- data$cluster

  message("Training models ...")

  #Init progress bar for console
  pb = txtProgressBar(min = 0, max = training_iterations, initial = 0)

  #Declare target variable as factor
  data_merged$class <- factor(data_merged$class, levels = c("sustained", "phasic"))

  #Initialize empty data.frames to collect all performance metrics
  svm <- data.frame()
  rsvm <- data.frame()
  log <- data.frame()
  lda <- data.frame()
  rf <- data.frame()

  #Monte Carlo Cross Validation: Use Random Subsampling - 1000 iterations as default

  suppressMessages(for (i in 1:training_iterations){

    #Update Progress Bar
    setTxtProgressBar(pb,i)

    #Randomly split data into training and test set
    #use 70% of data set as training set and 30% as test set
    split <- base::sample(c(TRUE, FALSE), nrow(data_merged), replace=TRUE, prob=c(0.7,0.3))
    training  <- data_merged[split, ]
    testing   <- data_merged[!split, ]

    metrics <- c()
    model_names <- c("SVM _linear", "SVM_radial", "logistic", "lda", "rf")

    #-------------------------
    #Fit SVM Model using linear kernel and no scaling
    svm_model <- e1071::svm(class~., data = training, kernel = "linear")

    #Obtain predictions
    svm_pred <- stats::predict(svm_model, newdata = testing)
    svm_cm <- table(testing$class, svm_pred)

    #Calculate performance metrics
    svm_metrics <- caret::confusionMatrix(svm_cm, mode = "everything")
    svm_auroc <- pROC::auc(unclass(testing$class), unclass(svm_pred))

    #Get metrics
    svm_all_metrics <- c()
    svm_all_metrics[1] <- svm_metrics$overall["Accuracy"]
    svm_all_metrics[2] <- svm_metrics$byClass["Sensitivity"]
    svm_all_metrics[3] <- svm_metrics$byClass["Specificity"]
    svm_all_metrics[4] <- svm_metrics$byClass["F1"]
    svm_all_metrics[5] <- svm_auroc

    #-------------------------
    #Fit SVM Model using radial kernel and no scaling
    rsvm_model <- e1071::svm(class~., data = training, kernel = "radial")

    #Obtain predictions
    rsvm_pred <- stats::predict(rsvm_model, newdata = testing)
    rsvm_cm <- table(testing$class, rsvm_pred)

    #Calcuate performance metrics
    rsvm_metrics <- caret::confusionMatrix(rsvm_cm, mode = "everything")
    rsvm_auroc <- pROC::auc(unclass(testing$class), unclass(rsvm_pred))

    #Get metrics
    rsvm_all_metrics <- c()
    rsvm_all_metrics[1] <- rsvm_metrics$overall["Accuracy"]
    rsvm_all_metrics[2] <- rsvm_metrics$byClass["Sensitivity"]
    rsvm_all_metrics[3] <- rsvm_metrics$byClass["Specificity"]
    rsvm_all_metrics[4] <- rsvm_metrics$byClass["F1"]
    rsvm_all_metrics[5] <- rsvm_auroc

    #-------------------------
    #Fit GLM Model for logistic Regression
    log_model <- suppressWarnings(stats::glm(class~., data =training, family = "binomial"))

    #Obtain predictions
    log_pred <- stats::predict(log_model, newdata = testing)

    #Convert values to phenotypes -> is there a more generalized approach?
    log_pred <- ifelse(log_pred>=0.5, "phasic", "sustained")
    log_pred <- factor(log_pred, levels = c("sustained", "phasic"))
    #Calcuate performance metrics
    log_metrics <- caret::confusionMatrix(data=log_pred, reference = testing$class,mode = "everything")
    log_auroc <- pROC::auc(unclass(testing$class), unclass(factor(log_pred)))

    #Get metrics
    log_all_metrics <- c()
    log_all_metrics[1] <- log_metrics$overall["Accuracy"]
    log_all_metrics[2] <- log_metrics$byClass["Sensitivity"]
    log_all_metrics[3] <- log_metrics$byClass["Specificity"]
    log_all_metrics[4] <- log_metrics$byClass["F1"]
    log_all_metrics[5] <- log_auroc

    #-------------------------
    #Fit linear discriminant analysis models
    lda_model <- MASS::lda(class~., data =training)

    #Obtain predictions
    lda_pred <- stats::predict(lda_model, newdata = testing)
    #Get Confusion matrix of the lda model
    lda_cm <- table(testing$class, lda_pred$class)

    #Calcuate performance metrics
    lda_metrics <- caret::confusionMatrix(lda_cm, mode = "everything")
    lda_auroc <- pROC::auc(unclass(testing$class), unclass(factor(lda_pred$class)))


    #Get metrics
    lda_all_metrics <- c()
    lda_all_metrics[1] <- lda_metrics$overall["Accuracy"]
    lda_all_metrics[2] <- lda_metrics$byClass["Sensitivity"]
    lda_all_metrics[3] <- lda_metrics$byClass["Specificity"]
    lda_all_metrics[4] <- lda_metrics$byClass["F1"]
    lda_all_metrics[5] <- lda_auroc

    #-------------------------
    #Fit random Forests
    rf_model <- randomForest::randomForest(class~., data =training)

    #Obtain predictions
    rf_pred <- stats::predict(rf_model, newdata = testing)

    #Get Confusion matrix of the random forest model
    rf_cm <- table(testing$class, rf_pred)

    #Calcuate performance metrics
    rf_metrics <- caret::confusionMatrix(rf_cm, mode = "everything")
    rf_auroc <- pROC::auc(unclass(testing$class), unclass(rf_pred))

    #Get metrics
    rf_all_metrics <- c()
    rf_all_metrics[1] <- rf_metrics$overall["Accuracy"]
    rf_all_metrics[2] <- rf_metrics$byClass["Sensitivity"]
    rf_all_metrics[3] <- rf_metrics$byClass["Specificity"]
    rf_all_metrics[4] <- rf_metrics$byClass["F1"]
    rf_all_metrics[5] <- rf_auroc

    #Add new metrics to dfs
    svm <- rbind(svm, svm_all_metrics)
    rsvm <- rbind(rsvm, rsvm_all_metrics)
    log <- rbind(log, log_all_metrics)
    lda <- rbind(lda, lda_all_metrics)
    rf <- rbind(rf, rf_all_metrics)
  })
  close(pb)

  #Assign Columnnames of Metric dataframes
  colnames(svm) <- c("Accuracy", "Sensitivity", "Specificity", "F1", "Auroc")
  colnames(rsvm) <- c("Accuracy", "Sensitivity", "Specificity", "F1", "Auroc")
  colnames(log) <- c("Accuracy", "Sensitivity", "Specificity", "F1", "Auroc")
  colnames(lda) <- c("Accuracy", "Sensitivity", "Specificity", "F1", "Auroc")
  colnames(rf) <- c("Accuracy", "Sensitivity", "Specificity", "F1", "Auroc")

  #----------------------------
  #Gather mean and standard deviation of all metrics
  acc.stats <- data.frame(SVM=paste(round(mean(svm$Accuracy), digits = 6), " +/- ", round(sd(svm$Accuracy), digits = 6)),
                          RSVM=paste(round(mean(rsvm$Accuracy), digits = 6), " +/- ", round(sd(rsvm$Accuracy), digits = 6)),
                          LOG=paste(round(mean(log$Accuracy), digits = 6), " +/- ", round(sd(log$Accuracy), digits = 6)),
                          LDA=paste(round(mean(lda$Accuracy), digits = 6), " +/- ", round(sd(lda$Accuracy), digits = 6)),
                          RandomForest=paste(round(mean(rf$Accuracy), digits = 6), " +/- ", round(sd(rf$Accuracy), digits = 6)))



  sens.stats <- data.frame(SVM=paste(round(mean(svm$Sensitivity), digits = 6), " +/- ", round(sd(svm$Sensitivity), digits = 6)),
                           RSVM=paste(round(mean(rsvm$Sensitivity), digits = 6), " +/- ", round(sd(rsvm$Sensitivity), digits = 6)),
                           LOG=paste(round(mean(log$Sensitivity), digits = 6), " +/- ", round(sd(log$Sensitivity), digits = 6)),
                           LDA=paste(round(mean(lda$Sensitivity), digits = 6), " +/- ", round(sd(lda$Sensitivity), digits = 6)),
                           RandomForest=paste(round(mean(rf$Sensitivity), digits = 6), " +/- ", round(sd(rf$Sensitivity), digits = 6)))

  spec.stats <- data.frame(SVM=paste(round(mean(svm$Specificity), digits = 6), " +/- ", round(sd(svm$Specificity), digits = 6)),
                           RSVM=paste(round(mean(rsvm$Specificity), digits = 6), " +/- ", round(sd(rsvm$Specificity), digits = 6)),
                           LOG=paste(round(mean(log$Specificity), digits = 6), " +/- ", round(sd(log$Specificity), digits = 6)),
                           LDA=paste(round(mean(lda$Specificity), digits = 6), " +/- ", round(sd(lda$Specificity), digits = 6)),
                           RandomForest=paste(round(mean(rf$Specificity), digits = 6), " +/- ", round(sd(rf$Specificity), digits = 6)))

  f1.stats <- data.frame(SVM=paste(round(mean(svm$F1), digits = 6), " +/- ", round(sd(svm$F1), digits = 6)),
                         RSVM=paste(round(mean(rsvm$F1), digits = 6), " +/- ", round(sd(rsvm$F1), digits = 6)),
                         LOG=paste(round(mean(log$F1), digits = 6), " +/- ", round(sd(log$F1), digits = 6)),
                         LDA=paste(round(mean(lda$F1), digits = 6), " +/- ", round(sd(lda$F1), digits = 6)),
                         RandomForest=paste(round(mean(rf$F1), digits = 6), " +/- ", round(sd(rf$F1), digits = 6)))

  auroc.stats <- data.frame(SVM=paste(round(mean(svm$Auroc), digits = 6), " +/- ", round(sd(svm$Auroc), digits = 6)),
                            RSVM=paste(round(mean(rsvm$Auroc), digits = 6), " +/- ", round(sd(rsvm$Auroc), digits = 6)),
                            LOG=paste(round(mean(log$Auroc), digits = 6), " +/- ", round(sd(log$Auroc), digits = 6)),
                            LDA=paste(round(mean(lda$Auroc), digits = 6), " +/- ", round(sd(lda$Auroc), digits = 6)),
                            RandomForest=paste(round(mean(rf$Auroc), digits = 6), " +/- ", round(sd(rf$Auroc), digits = 6)))

  metrics <- rbind(acc.stats, sens.stats, spec.stats, f1.stats, auroc.stats)
  rownames(metrics) <- c("Accuracy", "Sensitivity", "Specificity", "F1", "Auroc")

  #----------------------------
  #Plot Accuracies
  accuracies <- data.frame(SVM=svm$Accuracy, RSVM=rsvm$Accuracy, LogisticRegression=log$Accuracy,
                           LDA=lda$Accuracy, randomForest=rf$Accuracy)

  accuracies.long <- suppressMessages(reshape2::melt(accuracies))

  plot.acc <- ggplot(data=accuracies.long, aes(x = variable, y = value, colour = variable)) +
    geom_boxplot(outlier.shape = NA)  +
    theme(axis.text = element_text(colour="black", size=12)) + theme_test(base_size=14) +
    labs(y= "Accuracy", x="") +
    scale_x_discrete(labels=c("linear SVM", "radial SVM", "logistic regression", "LDA", "random Forest")) +
    ggtitle(label = "Accuracy") +
    geom_jitter(size=0.5, width = 0.2, show.legend = F) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
    theme(axis.text.x = element_text(angle=35, hjust=1))

  #----------------------------
  #Plot Sensitivity

  sensitivities <- data.frame(SVM=svm$Sensitivity, RSVM=rsvm$Sensitivity, LogisticRegression=log$Sensitivity,
                              LDA=lda$Sensitivity, randomForest=rf$Sensitivity)

  sensitivities.long <- suppressMessages(reshape2::melt(sensitivities))

  plot.sens <- ggplot2::ggplot(data=sensitivities.long, aes(x = variable, y = value, colour = variable)) +
    geom_boxplot(outlier.shape = NA) + theme_test(base_size = 14) +
    labs(y= "Sensitivity", x="") +
    scale_x_discrete(labels=c("linear SVM", "radial SVM", "logistic regression", "LDA", "random Forest")) +
    ggtitle(label="Sensitivity") +
    geom_jitter(size=1, width = 0.2, show.legend = F) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
    theme(axis.text.x = element_text(angle=35, hjust=1))

  #----------------------------
  #Plot Specificity

  specificities <- data.frame(SVM=svm$Specificity, RSVM=rsvm$Specificity, LogisticRegression=log$Specificity,
                              LDA=lda$Specificity, randomForest=rf$Specificity)

  specificities.long <- suppressMessages(reshape2::melt(specificities))

  plot.spec <- ggplot2::ggplot(data=specificities.long, aes(x = variable, y = value, colour = variable)) +
    geom_boxplot(outlier.shape = NA)  + theme_test(base_size = 14) +
    labs(y= "Specificity", x="") + scale_x_discrete(labels=c("linear SVM", "radial SVM", "logistic regression", "LDA", "random Forest")) +
    ggtitle(label="Specificity") +
    geom_jitter(size=1, width = 0.2, show.legend = F) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
    theme(axis.text.x = element_text(angle=35, hjust=1))

  #----------------------------
  #Plot F1 Scores

  f1 <- data.frame(SVM=svm$F1, RSVM=rsvm$F1, LogisticRegression=log$F1,
                   LDA=lda$F1, randomForest=rf$F1)

  f1.long <- suppressMessages(reshape2::melt(f1))

  plot.f1 <- ggplot2::ggplot(data=f1.long, aes(x = variable, y = value, colour = variable)) +
    geom_boxplot(outlier.shape = NA)  + theme_test(base_size = 14) +
    labs(y= "F1", x="") + scale_x_discrete(labels=c("linear SVM", "radial SVM", "logistic regression", "LDA", "random Forest")) +
    ggtitle(label="F1") +
    geom_jitter(size=1, width = 0.2, show.legend = F) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
    theme(axis.text.x = element_text(angle=35, hjust=1))

  #----------------------------
  #Plot Auroc Scores
  aurocs <- data.frame(SVM=svm$Auroc, RSVM=rsvm$Auroc, LogisticRegression=log$Auroc,
                       LDA=lda$Auroc, randomForest=rf$Auroc)

  aurocs.long <- suppressMessages(reshape2::melt(aurocs))

  plot.aurocs <- ggplot2::ggplot(data=aurocs.long, aes(x = variable, y = value, colour = variable)) +
    geom_boxplot(outlier.shape = NA)  + theme_test(base_size = 14) +
    labs(y= "Auroc", x="") +
    scale_x_discrete(labels=c("linear SVM", "radial SVM", "logistic regression", "LDA", "random Forest")) +
    ggtitle(label="Auroc") +
    geom_jitter(size=1, width = 0.2, show.legend = F) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
    theme(axis.text.x = element_text(angle=35, hjust=1))

  ##############################################################################
  #Train final models on all data
  SVM <- suppressMessages(e1071::svm(class~., data = data_merged, kernel = "linear"))
  RSVM <- suppressMessages(e1071::svm(class~., data = data_merged, kernel = "radial"))
  Logistic <- suppressWarnings(stats::glm(class~., data = data_merged, family = "binomial"))
  LDA <- suppressMessages(MASS::lda(class~., data = data_merged))
  RF <- suppressMessages(randomForest::randomForest(class~., data = data_merged))

  message("Finished Training successfully")

  #Return plots and Models
  return(list(Accuracy=plot.acc, Sensitivity=plot.sens, Specificity=plot.spec,
              F1=plot.f1, Auroc=plot.aurocs, SVM=SVM, RSVM=RSVM, Logistic=Logistic,
              LDA=LDA, RF=RF, Stats=metrics))

}
