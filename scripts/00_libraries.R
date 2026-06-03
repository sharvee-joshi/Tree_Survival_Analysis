############################################################
# 00_libraries.R
# Tree Survival Analysis Project
#
# Purpose: Load all packages needed for data cleaning, exploratory
#   data analysis, survival analysis, model diagnostics,
#   tables, and figures.
############################################################

# ------------------------------------------------------------------
# 1. List required packages
# ------------------------------------------------------------------

required_packages <- c(
  "tidyverse",
  "janitor",
  "here",
  "naniar",
  "skimr",
  "survival",
  "survminer",
  "broom",
  "gtsummary",
  "knitr",
  "kableExtra",
  "patchwork"
)

# ------------------------------------------------------------------
# 2. Install missing packages
# ------------------------------------------------------------------

install_if_missing <- function(pkg){
  if(!pkg %in% installed.packages()){
    install.packages(pkg)
  }
}

lapply(required_packages, install_if_missing)
lapply(required_packages, library, character.only = TRUE)

# ------------------------------------------------------------------
# 3. Set global options
# ------------------------------------------------------------------

options(
  stringsAsFactors = FALSE,
  scipen = 999
)

theme_set(theme_minimal())

# ------------------------------------------------------------------
# 4. Reproducibility
# ------------------------------------------------------------------

set.seed(218)

# ------------------------------------------------------------------
# 5. Print session info
# ------------------------------------------------------------------

sessionInfo()
