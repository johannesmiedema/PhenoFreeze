
#' Classification of phasic and sustained freezers
#'
#' @param data dataframe containing freezing values of bins 13 - 24
#'
#' @param sex "female" or "male" specifying the sex
#' @param MR 1 or 2 specifying the Memory Retrieval Session 1 or 2
#' @param ... further parameters
#' @export predict.phenotype
#' @export

predict.phenotype <- function(data, ..., sex, MR){

  # LOAD ML MODELS --------------------------
  #Load ML Models - MR1 phenotype classification
  m.MR1.model <- male_MR1
  f.MR1.model <- female_MR1

  #Load MR2 Prediction Models including shifter using MR1 freezing data
  m.MR2pred.shifter.model <- male_MR2
  f.MR2pred.shifter.model <- female_MR2

  # 12 time bins during tone was played
  bins <- 1:12
  beta <- 0
  int <- 0
  freeze <- rowMeans(data)
  params <- data.frame()

  #Fit regression model for each animal
  for (i in 1:nrow(data)){
    #transpose x and y
    y <- t(data[i,])
    #impute +1 if y is zero to allow log operation
    if (0 %in% y){y<-y+1}
    #fit model
    mod.loglinear <- stats::lm(log(y)~bins)
    #obtain beta coefficient
    beta[i] <- mod.loglinear$coefficients[2]
    #obtain intercepts
    int[i] <- mod.loglinear$coefficients[1]
  }

  #Initialize data frame for regression parameters
  params <- data.frame(freeze=freeze,beta=beta, int=int)

  #Prediction using loaded model

  #Predict MR1 Phenotype of females
  if (sex == "female" & MR == 1){
    message("Predicting female MR1 Phenotypes using MR1 freezing data")
    phenotypes <- stats::predict(f.MR1.model, newdata = params)
    #Transform values as the model is a glm model
    phenotypes <- ifelse(phenotypes>=0.5, "phasic", "sustained")
    phenotypes <- factor(phenotypes, levels = c("sustained", "phasic"))
  }
  #Predict MR2 Phenotypes of females
  if (sex == "female" & MR == 2){
    message("Predicting female MR2 Phenotypes including shifters using MR1 freezing data")
    phenotypes <- stats::predict(f.MR2pred.shifter.model, newdata = params)
    #Transform values as the model is a glm model
    phenotypes <- ifelse(phenotypes>=0.5, "phasic", "sustained")
    phenotypes <- factor(phenotypes, levels = c("sustained", "phasic"))
  }
  #Predict MR1 Phenotype of males
  if (sex == "male" & MR == 1){
    message("Predicting male MR1 Phenotypes using MR1 freezing data")
    phenotypes <- stats::predict(m.MR1.model, newdata = params)
  }
  #Predict MR2 Phenotypes of males
  if (sex == "male" & MR == 2 ){
    message("Predicting male MR2 Phenotypes including shifters using MR1 freezing data")
    phenotypes <- stats::predict(m.MR2pred.shifter.model, newdata = params)
  }
  message("Classified  ", length(phenotypes), " animals. ")
  return(list(phenotypes, params))
}

