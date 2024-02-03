
#' Classification of phasic and sustained freezers
#'
#' @param data_MR1 dataframe containing freezing values MR1 of bins 13 - 24 as a character
#' @param data_MR2 dataframe containing freezing values MR2 of bins 13 - 24 as a character
#' @param sex "female" or "male" specifying the sex
#' @param MR 1 or 2 specifying the Memory Retrieval Session 1 or 2
#' @import randomForest
#' @import e1071
#' @export classify_freezer
#' @export

classify_freezer <- function(data_MR1, data_MR2, sex, MR){

  #####################################
  #-------PARAMETER VALIDATION--------#

  #Validate data_MR1 for MR1 Classification
  if (MR == 1 & missing(data_MR1)){
    stop("data_MR1 is required for MR1 classification")
  }
  #Validate data_MR2 for MR2 Classification
  if (MR == 2 & missing(data_MR2)){
    stop("data_MR1 AND data_MR2 are required for MR2 classification")
  }
  #If MR2 Classification is requested, both MR1 and MR2 dataset should be input
  if (MR == 2 & missing(data_MR1)){
    stop("data_MR1 AND data_MR2 are required for MR2 classification")
  }
  #Validate type data_MR1 as data.frame
  if (!is.data.frame(data_MR1)){
    stop("data_MR1 has to be an object of type data.frame")
  }

  #Validate length of data_MR1
  if (ncol(data_MR1) < 12){
    stop("data_MR1 must consist of at least 12 columns of tone presentation")
  }

  #Validate column names "13" to "24"
  if ((all(as.character(c(13:24)) %in% colnames(data_MR1))) == FALSE){
    stop("data_MR1 must have column names 13 to 24 of type character")
  }

  #Validate Input data_MR2 if MR = 2 is specified
  if (MR == 2){

    #Validate type data_MR2 as data.frame
    if (!is.data.frame(data_MR2)){
      stop("data_MR2 has to be an object of type data.frame")
    }

    #Validate length of data_MR2
    if (ncol(data_MR2) < 12){
      stop("data_MR2 must consist of at least 12 columns of tone presentation")
    }

    #Validate column names "13" to "24" of data_MR2
    if ((all(as.character(c(13:24)) %in% colnames(data_MR2))) == FALSE){
      stop("data_MR1 must have column names 13 to 24 of type character")
    }
  }

  #Transform input data_MR1 if it does not already consist of 12 time bins
  #Extract column names 13 to 24
  if (ncol(data_MR1) > 12){
    data_MR1 <- base::subset(data_MR1, select = as.character(c(13:24)))
  }

  #Transform input data_MR2 if it does not already consist of 12 time bins
  #Extract column names 13 to 24
  if(MR == 2){
    data_MR2 <- base::subset(data_MR2, select = as.character(c(13:24)))
  }

  #Validate sex parameter
  if (sex != "female" & sex != "male"){
    stop("Sex must be a string of type 'female' or 'male'")
  }

  #Validate Memory Retrieval parameter
  if (MR != 1 & MR != 2){
    stop("MR must be an Integer of 1 or 2")
  }

  #####################################
  #--------MR1-REGRESSION-------------#

  # 12 time bins during tone was played
  #Initialize variables
  bins <- 1:12
  beta_MR1 <- 0
  int_MR1 <- 0
  #Calculate average freezing of each animal
  freeze_MR1 <- rowMeans(data_MR1)

  #Fit regression model for each animal
  for (i in 1:nrow(data_MR1)){
    #transpose x and y
    y <- t(data_MR1[i,])
    #pseudocount if y is zero to allow log operation
    if (0 %in% y){y<-y+1}
    #fit loglinear model
    mod.loglinear <- stats::lm(log(y)~bins)
    #obtain beta coefficients
    beta_MR1[i] <- mod.loglinear$coefficients[2]
    #obtain intercepts
    int_MR1[i] <- mod.loglinear$coefficients[1]
  }

  #Initialize data frame for regression parameters
  params_MR1 <- data.frame(freeze_MR1=freeze_MR1,beta_MR1=beta_MR1, int_MR1=int_MR1)

  #If MR = 1 specified: Return classified animals - prompt success message on console
  if (MR == 1){
    #####################################
    #----------CLASSIFICATION-----------#
    #Predict MR1 Phenotype of females
    if (sex == "female" & MR == 1){
      message("Predicting female MR1 Phenotypes using MR1 freezing data")
      phenotypes <- stats::predict(female_MR1, newdata = params_MR1)
      #Transform values as the model is a glm model
      phenotypes <- ifelse(phenotypes>=0.5, "phasic", "sustained")
      phenotypes <- factor(phenotypes, levels = c("sustained", "phasic"))
    }
    #Predict MR1 Phenotype of males
    if (sex == "male" & MR == 1){
      message("Predicting male MR1 Phenotypes using MR1 freezing data")
      phenotypes <- stats::predict(male_MR1, newdata = params_MR1)
    }
    if (length(phenotypes) == nrow(data_MR1)){
      message("MR1 classification successfull")
      message("Classified  ", length(phenotypes), " animals. ")
      return(phenotypes)
    } else {
      stop("Classification error, recheck your input parameters")
    }
  }

  #If MR = 2 specified: Perform Regressionanalysis on MR2 and classify using both
  #MR1 and MR2 regression parameters as suggested before
  if (MR == 2){
    #####################################
    #------------REGRESSION-------------#

    # 12 time bins during tone was played
    #Initialize variables
    bins <- 1:12
    beta_MR2 <- 0
    int_MR2 <- 0
    #Calculate average freezing of each animal
    freeze_MR2 <- rowMeans(data_MR2)

    #Fit regression model for each animal
    for (i in 1:nrow(data_MR2)){
      #transpose x and y
      y <- t(data_MR2[i,])
      #pseudocount if y is zero to allow log operation
      if (0 %in% y){y<-y+1}
      #fit loglinear model
      mod.loglinear <- stats::lm(log(y)~bins)
      #obtain beta coefficients
      beta_MR2[i] <- mod.loglinear$coefficients[2]
      #obtain intercepts
      int_MR2[i] <- mod.loglinear$coefficients[1]
    }

    #Initialize data frame for regression parameters
    params_MR2 <- data.frame(freeze_MR2=freeze_MR2,beta_MR2=beta_MR2, int_MR2=int_MR2)

    #Merge MR1 and MR2 regression parameters for MR2 classification
    params_merged <- base::cbind(params_MR1, params_MR2)

    #####################################
    #-------MR2-CLASSIFICATION----------#
    #Predict MR2 Phenotypes of females
    if (sex == "female"){
      message("Predicting female MR2 Phenotypes including shifters using MR1 and MR2 freezing data")
      phenotypes <- stats::predict(female_MR2, newdata = params_merged)
    }
    #Predict MR2 Phenotypes of males
    if (sex == "male"){
      message("Predicting male MR2 Phenotypes including shifters using MR1 and MR2 freezing data")
      phenotypes <- stats::predict(male_MR2, newdata = params_merged)
    }
    if (length(phenotypes) == nrow(data_MR2)){
      message("MR2 classification successfull")
      message("Classified  ", length(phenotypes), " animals. ")
      return(phenotypes)
    } else {
      stop("Classification error, recheck your input parameters")
    }
  }
}
