
#' Classification of phasic and sustained freezers
#'
#' @param data dataframe containing freezing values of bins 13 - 24 as a character
#' @param sex "female" or "male" specifying the sex
#' @param MR 1 or 2 specifying the Memory Retrieval Session 1 or 2
#' @import randomForest
#' @import e1071
#' @export classify_freezer
#' @export

classify_freezer <- function(data, sex, MR){
  #####################################
  #-------PARAMETER VALIDATION--------#

  #Validate Input data
  if (!is.data.frame(data)){
    stop("Data has to be an object of type data.frame")
  }

  if (ncol(data) < 12){
    stop("Data must consist of at least 12 columns of tone presentation")
  }

  if ((all(as.character(c(13:24)) %in% colnames(data))) == FALSE){
    stop("Data must have column names 13 to 24 of type character")
  }

  #Transform input data if it does not already consist of 12 time bins
  #Extract column names 13 to 24
  if (ncol(data) > 12){
    data <- base::subset(data, select = as.character(c(13:24)))
  }

  #Validate sex paramter
  if (sex != "female" & sex != "male"){
    stop("Sex must be a string of type 'female' or 'male'")
  }

  #Validate Memory Retrieval parameter
  if (MR != 1 & MR != 2){
    stop("MR must be an Integer of 1 or 2")
  }

  #####################################
  #------------REGRESSION-------------#

  # 12 time bins during tone was played
  #Initialize variables
  bins <- 1:12
  beta <- 0
  int <- 0
  #Calculate average freezing of each animal
  freeze <- rowMeans(data)

  #Fit regression model for each animal
  for (i in 1:nrow(data)){
    #transpose x and y
    y <- t(data[i,])
    #pseudocount if y is zero to allow log operation
    if (0 %in% y){y<-y+1}
    #fit loglinear model
    mod.loglinear <- stats::lm(log(y)~bins)
    #obtain beta coefficients
    beta[i] <- mod.loglinear$coefficients[2]
    #obtain intercepts
    int[i] <- mod.loglinear$coefficients[1]
  }

  #Initialize data frame for regression parameters
  params <- data.frame(freeze=freeze,beta=beta, int=int)

  #####################################
  #----------CLASSIFICATION-----------#
  #Prediction using lazy loaded model

  #Predict MR1 Phenotype of females
  if (sex == "female" & MR == 1){
    message("Predicting female MR1 Phenotypes using MR1 freezing data")
    phenotypes <- stats::predict(female_MR1, newdata = params)
    #Transform values as the model is a glm model
    phenotypes <- ifelse(phenotypes>=0.5, "phasic", "sustained")
    phenotypes <- factor(phenotypes, levels = c("sustained", "phasic"))
  }
  #Predict MR2 Phenotypes of females
  if (sex == "female" & MR == 2){
    message("Predicting female MR2 Phenotypes including shifters using MR1 freezing data")
    phenotypes <- stats::predict(female_MR2, newdata = params)
    #Transform values as the model is a glm model
    phenotypes <- ifelse(phenotypes>=0.5, "phasic", "sustained")
    phenotypes <- factor(phenotypes, levels = c("sustained", "phasic"))
  }
  #Predict MR1 Phenotype of males
  if (sex == "male" & MR == 1){
    message("Predicting male MR1 Phenotypes using MR1 freezing data")
    phenotypes <- stats::predict(male_MR1, newdata = params)
  }
  #Predict MR2 Phenotypes of males
  if (sex == "male" & MR == 2 ){
    message("Predicting male MR2 Phenotypes including shifters using MR1 freezing data")
    phenotypes <- stats::predict(male_MR2, newdata = params)
  }

  #Return classified animals - prompt success message on console
  message("Classified  ", length(phenotypes), " animals. ")
  return(phenotypes)
}
