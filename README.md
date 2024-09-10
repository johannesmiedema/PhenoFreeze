![cmd](https://github.com/johannesmiedema/FreezerClassifier/actions/workflows/R-CMD-check.yaml/badge.svg) ![R](https://github.com/johannesmiedema/FreezerClassifier/actions/workflows/r.yml/badge.svg) 

# PhenoFreeze
PhenoFreeze is an R package which can be used to predict behavioral endophenotypes after auditory aversive conditioning (AAC) [1]. Designed for the classification using the described experimental setup [1], PhenoFreeze can classify animals into sustained and phasic freezers using sex-specific machine-learning models. 

<img width="1070" alt="workflow" src="https://github.com/johannesmiedema/FreezerClassifier/assets/105965619/23cf17b1-947c-490f-8a56-8ee37016cca4">

First, PhenoFreeze performs regression analysis, obtaining coefficients of a loglinear model for each individual freezing curve. These model coefficients are then used by sex-specifc machine-learning models to classify animals into sustained and phasic freezers. 

## Installation
This package can be directly installed from github using devtools:
```
if(!require(devtools)){
    install.packages("devtools")
    library(devtools)
}
devtools::install_github("johannesmiedema/PhenoFreeze")
```

## Usage 
The main utility of this package is the function classify_freezer(). To use this function for classification, a dataset of freezing values during Memory Retrieval (MR) sessions is needed. Using the described phenotyping pipeline in this particular experimental setup [1], the data table should contain the time bins during tone presentation (bins 13 - 24). The columns should have the proper column names accordingly (e.g. "13", "14", ...). The classifier can then  be used for female MR1 classification:
```
results <- classify_freezer(data_MR1 = dataset_MR1, sex = "female", MR = 1)
```
The function returns a vector containing the classified phenotypes "sustained" and "phasic". 

PhenoFreeze is also able to predict MR2 phenotypes including shifters, which are animals who shifted their phenotype from MR1 to MR2. However, to classify MR2 phenotypes, classify_freezer() requires both MR1 and MR2 freezing datasets. It is necessary that both datasets represent each animals freezing values in the same order, please check before using the MR2 classification. Then, MR2 classification can be done:
```
results <- classify_freezer(data_MR1 = dataset_MR1, data_MR2 = dataset_MR2, sex = "female", MR = 2)

```

## References 
[1] Kovlyagina I, Wierczeiko A, Todorov H, Jacobi E, Tevosian M, et al. (2024) Leveraging interindividual variability in threat conditioning of inbred mice to model trait anxiety. PLOS Biology 22(5): e3002642. https://doi.org/10.1371/journal.pbio.3002642
