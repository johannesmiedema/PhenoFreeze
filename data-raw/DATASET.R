## code to prepare `DATASET` dataset goes here

library(openxlsx)
library(stats)
library(randomForest)
library(e1071)
library(usethis)

#read labelled data set
f_MR1 <- openxlsx::read.xlsx("../Phenotyping-Project/labelled_tables/females_MR1.xlsx", colNames = T)
f_MR2 <- openxlsx::read.xlsx("../Phenotyping-Project/labelled_tables/females.MR2.xlsx", colNames = T)

m_MR1 <- openxlsx::read.xlsx("../Phenotyping-Project/labelled_tables/males_MR1.xlsx", colNames = T)
m_MR2 <- openxlsx::read.xlsx("../Phenotyping-Project/labelled_tables/males.MR2.xlsx", colNames = T)

#Declare class as factor
f_MR1$class <- as.factor(f_MR1$class)
f_MR2$class <- as.factor(f_MR2$class)

m_MR1$class <- as.factor(m_MR1$class)
m_MR2$class <- as.factor(m_MR2$class)

#Prepare MR2 datasets as merge of MR1 and MR2
f_MR2 <- merge(f_MR1, f_MR2, by = "id")
f_MR2 <- f_MR2[,c(3:5, 8:11)]
colnames(f_MR2) <- c("freeze_MR1", "beta_MR1", "int_MR1", "freeze_MR2", "beta_MR2",
                     "int_MR2", "class")

m_MR2 <- merge(m_MR1, m_MR2, by = "id")
m_MR2 <- m_MR2[,c(3:5, 8:11)]
colnames(m_MR2) <- c("freeze_MR1", "beta_MR1", "int_MR1", "freeze_MR2", "beta_MR2",
                     "int_MR2", "class")

#Train Female Models - logistic regression and randomForest
female_MR1 <- stats::glm(class ~., data =f_MR1[,3:6], family = "binomial")
female_MR2 <- randomForest::randomForest(class~., data =f_MR2)

#Train Male Models - RandomForest and SVM using a linear kernel
male_MR1 <- randomForest::randomForest(class~., data =m_MR1[,3:6])
male_MR2 <- e1071::svm(class~., data = m_MR2, kernel = "linear")

#Save the models internally to sys-data to be lazy loaded later on
usethis::use_data(female_MR1, female_MR2, male_MR1, male_MR2, internal = TRUE,
                  overwrite = TRUE)
