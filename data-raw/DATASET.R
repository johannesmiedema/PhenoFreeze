## code to prepare `DATASET` dataset goes here

library(openxlsx)
library(stats)
library(randomForest)
library(e1071)
library(usethis)

#read labelled data set
f_MR1 <- openxlsx::read.xlsx("../Phenotyping-Project/labelled_tables/females_MR1.xlsx", colNames = T)
f_MR2 <- openxlsx::read.xlsx("../Phenotyping-Project/labelled_tables/females_MR2shifter.xlsx", colNames = T)
f_MR2_noshifter <- openxlsx::read.xlsx("../Phenotyping-Project/labelled_tables/females.MR2.xlsx", colNames = T)

m_MR1 <- openxlsx::read.xlsx("../Phenotyping-Project/labelled_tables/males_MR1.xlsx", colNames = T)
m_MR2 <- openxlsx::read.xlsx("../Phenotyping-Project/labelled_tables/males_MR2shifter.xlsx", colNames = T)
m_MR2_noshifter <- openxlsx::read.xlsx("../Phenotyping-Project/labelled_tables/males.MR2.xlsx", colNames = T)

#Declare class as factor
f_MR1$class <- as.factor(f_MR1$class)
f_MR2$class <- as.factor(f_MR2$class)
f_MR2_noshifter$class <- as.factor(f_MR2_noshifter$class)

m_MR1$class <- as.factor(m_MR1$class)
m_MR2$class <- as.factor(m_MR2$class)
m_MR2_noshifter$class <- as.factor(m_MR2_noshifter$class)

#Rename MR1 column names
colnames(f_MR1) <- c("batch", "id", "freeze_MR1", "beta_MR1",
                     "int_MR1", "class")

colnames(m_MR1) <- c("batch", "id", "freeze_MR1", "beta_MR1",
                     "int_MR1", "class")

colnames(f_MR2_noshifter) <- c("batch", "id", "freeze_MR2", "beta_MR2",
                     "int_MR2", "class")

colnames(m_MR2_noshifter) <- c("batch", "id", "freeze_MR2", "beta_MR2",
                     "int_MR2", "class")

#Prepare MR2 datasets as merge of MR1 and MR2
f_MR2 <- merge(f_MR1, f_MR2, by = "id")
f_MR2 <- f_MR2[,c(3:5, 8:11)]
colnames(f_MR2) <- c("freeze_MR1", "beta_MR1", "int_MR1", "freeze_MR2", "beta_MR2",
                     "int_MR2", "class")

m_MR2 <- merge(m_MR1, m_MR2, by = "id")
m_MR2 <- m_MR2[,c(3:5, 8:11)]
colnames(m_MR2) <- c("freeze_MR1", "beta_MR1", "int_MR1", "freeze_MR2", "beta_MR2",
                     "int_MR2", "class")

#Train Female Models
female_MR1 <- stats::glm(class ~., data =f_MR1[,3:6], family = "binomial")
female_MR2 <- randomForest::randomForest(class~., data = f_MR2)
female_MR2_noshifter <- stats::glm(class ~., data =f_MR2_noshifter[,3:6], family = "binomial")

#Train Male Models
male_MR1 <- randomForest::randomForest(class~., data = m_MR1[,3:6])
male_MR2 <- randomForest::randomForest(class~., data =m_MR2)
male_MR2_noshifter <- e1071::svm(class~., data = m_MR2_noshifter[,3:6], kernel = "linear")

#Save the models internally to sys-data to be lazy loaded later on
usethis::use_data(female_MR1, female_MR2, female_MR2_noshifter,
                  male_MR1, male_MR2, male_MR2_noshifter,
                  internal = TRUE, overwrite = TRUE)

#Save batch fc83 females as an example dataset

fc83 <- openxlsx::read.xlsx("../Phenotyping-Project/data_tables/fc83_f.xlsx", sheet = "retrieval1")
rownames(fc83) <- fc83$id
fc83 <- fc83[,4:39]

freezingDataset <- fc83
usethis::use_data(freezingDataset, overwrite = TRUE)
