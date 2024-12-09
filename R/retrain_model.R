
#' Retraining utility of the PhenoFreeze Package to add more data to the training
#'
#' @param new_data dataframe containing new freezing values MR1 of bins 13 - 24 as a character
#' @param sex "female" or "male" specifying the sex of the new data
#' @param MR 1 or 2 specifying the Memory Retrieval Session 1 or 2 of the new data
#' @param training_iterations description
#' @import mclust
#' @export retrain_model
#' @export



retrain_model <- function(new_data, sex, MR, training_iterations = 1000){
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
    y <- t(data_new[i,])
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

  #Reformat previous trainings data for merging with new data
  old_data <- old_data[,3:5]
  colnames(old_data) <- c("freeze", "beta", "int")

  #Merge with new data
  data_merged <- rbind(old_data, new_data)

  #####################################
  #Reclustering with GMM mclust
  clust <- invisible(mclust::Mclust(data_merged, G=2))

  #Assign sustained and phasic animals based on higher average freezing per cluster
  data <- data.frame(data$freeze, clust$classification)
  colnames(data) <- c("freeze", "cluster")
  mean1 <- mean(data[data$cluster==1,]$freeze)
  mean2 <- mean(data[data$cluster==2,]$freeze)

  if (mean1 < mean2){
    data$cluster <-c("phasic", "sustained")[ match( data$cluster, c(1,2))]

  } else {

    data$cluster <-c("sustained", "phasic")[ match( data$cluster, c(1,2))]
  }

  data_merged$class <- data$cluster

  return(data_merged)

}







