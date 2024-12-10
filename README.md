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
Using PhenoFreeze is explained in detail in the vignette, which can be found here: https://johannesmiedema.github.io/PhenoFreezeDoc/ 

## References 
[1] Kovlyagina I, Wierczeiko A, Todorov H, Jacobi E, Tevosian M, et al. (2024) Leveraging interindividual variability in threat conditioning of inbred mice to model trait anxiety. PLOS Biology 22(5): e3002642. https://doi.org/10.1371/journal.pbio.3002642
