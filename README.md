# Predicting household appliance energy use

A reproducible analysis for STAT 200 (UC Santa Cruz, Fall 2026). 

## How to run

R version: 4.6.1.

Start R in the project root (the folder containing this README). Install the required packages once:

```r
install.packages(c("tidyverse", "caret", "car", "withr"))
```

Each script in [Code/](Code/) runs independently using the dataset in [Data/](Data/). For example, to reproduce the paper’s OLS results, run:

```r
source("Code/01-Reproduction.R")
```

## Analysis

1. [Reproduction of the original paper’s OLS results](Code/01-Reproduction.R)
2. [Extended diagnostics for the original OLS model](Code/02-Diagnostic.R)
3. [Extension using PCA and PCR](Code/03-PCR.R)
4. [Extension using temporal models](Code/04-TS.R)

Scripts 02–04 save their figures in [Plots/](Plots/).

## Data and code attribution

This project uses the dataset and adapts the original R code for the OLS reproduction from [Luis M. Candanedo’s GitHub repository](https://github.com/LuisM78/Appliances-energy-prediction-data). 

Candanedo, L. M., Feldheim, V., & Deramaix, D. (2017). [Data driven prediction models of energy use of appliances in a low-energy house](https://doi.org/10.1016/j.enbuild.2017.01.083). *Energy and Buildings, 140*, 81–97.
