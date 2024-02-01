![cmd](https://github.com/johannesmiedema/FreezerClassifier/actions/workflows/R-CMD-check.yaml/badge.svg) ![R](https://github.com/johannesmiedema/FreezerClassifier/actions/workflows/r.yml/badge.svg) 

# phenoFreeze
phenoFreeze is an R package which can be used to predict behavioral endophenotypes after auditory aversive conditioning (AAC)[1]. Designed for the classification using the described experimental setup[1], phenoFreeze can classify animals into sustained and phasic freezers using sex-specific machine-learning models. 

<img width="1070" alt="workflow" src="https://github.com/johannesmiedema/FreezerClassifier/assets/105965619/23cf17b1-947c-490f-8a56-8ee37016cca4">

First, phenoFreeze performs regression analysis, obtaining coefficients of a loglinear model for each individual freezing curve. These model coefficients are then used by sex-specifc machine-learning models to classify animals into sustained and phasic freezers. 

## Installation
This package can be directly installed from github using devtools:
```
if(!require(devtools)){
    install.packages("devtools")
    library(devtools)
}
devtools::install_github("johannesmiedema/phenoFreeze")
```

## Usage 
The main utility of this package is the function classify_freezer(). To use this function for classification, a dataset of freezing values during Memory Retrieval (MR) sessions is needed. Using the described phenotyping pipeline in this particular experimental setup[1], the data table should contain only the time bins during tone presentation (bins 13 - 24). The classifier can be used for female MR1 classification as:
```
results <- classify_freezer(data, sex = "female", MR = 1)
```
The function returns a vector containing the classified phenotypes "sustained" and "phasic". 
## Project status
This package is currently under developement. 

## References 
Irina Kovlyagina, Anna Wierczeiko, Hristo Todorov, Eric Jacobi, Margarita Tevosian, Jakob von Engelhardt, Susanne Gerber*, Beat Lutz*. Freezing responses during prolonged threat memory retrieval reflect trait-like anxiety endophenotypes in female and male inbred mice. bioRxiv, doi: https://doi.org/10.1101/2023.11.22.568236 (2023).
