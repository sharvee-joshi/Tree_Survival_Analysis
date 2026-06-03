# Survival Analysis of Tree Seedling Mortality Under Light and Microbial Treatments
**Methods:** Survival Analysis, Kaplan-Meier, Cox Proportional Hazards

**Language:** R

## Author: Sharvee Joshi  

## Overview

This project applies survival analysis methods to a greenhouse experiment studying survival outcomes in newly germinated tree seedlings. The dataset includes three temperate Acer species exposed to different light and microbial treatments. The main goal is to evaluate whether species identity, light availability, and microbial community treatment are associated with differences in seedling survival.

## Research Question

How do species, light availability, and microbial filtrate treatment affect seedling survival over time?

More specifically, this project asks:

1. Do survival curves differ across Acer species?
2. Does light availability affect seedling survival?
3. Do microbial filtrate treatments influence survival?
4. After adjusting for species, light, and microbe treatment, which factors are associated with increased or decreased hazard of seedling death?

## Data

The data come from a greenhouse experiment involving three Acer species:

- `Acsa`: Acer saccharum
- `Acru`: Acer rubrum
- `Acne`: Acer negundo

The experimental treatments include:

- Light treatment: low light and high light
- Microbial filtrate treatment: small filtrate, large filtrate, combined filtrate, and sterilized control

The outcome is time-to-event survival data, where seedlings are followed until either harvest, death, or the experiment ended.

## Statistical Methods 

The project uses the following survival analysis methods: 
1. Kaplan-Meier survival curves to estimate survival probabilities over time.
2. Log-rank tests to compare survival distributions across groups.
3. Cox proportional hazards regression to estimate the association between treatment variables and hazard of seedling death.
4. Proportional hazards diagnostics using Schoenfeld residuals.

## Required R Packages

The script `00_libraries.R` automatically installs and loads the required packages. These include:
- tidyverse
- survival
- survminer
- broom
- kableExtra
- janitor
- naniar
- skimr

No manual installation is required. Package versions are recorded using `renv`. 

## Reproducibility Instructions

To reproduce the analysis, run the scripts in the following order:
```
source("scripts/00_libraries.R")
source("scripts/01_data_cleaning.R")
source("scripts/02_missingness_summary.R")
source("scripts/03_eda.R")
source("scripts/04_km_analysis.R")
source("scripts/05_cox_models.R")
source("scripts/06_model_diagnostics.R")
```
### Output
Running the scripts will generate:
- add here






