############################################################
# 01_data_cleaning.R
# Tree Survival Analysis Project
#
# Purpose: Load the raw tree survival data, clean variable names,
#   recode variables, create survival-analysis variables,
#   and save the cleaned dataset.
############################################################

# ------------------------------------------------------------------
# 0. Load required packages
# ------------------------------------------------------------------

source("scripts/00_libraries.R")

# ------------------------------------------------------------------
# 1. Load raw data
# ------------------------------------------------------------------
tree_raw <- read_csv("data/Tree_Data.csv")

# ------------------------------------------------------------------
# 2. Inspect data
# ------------------------------------------------------------------

glimpse(tree_raw)
names(tree_raw)
summary(tree_raw)

# ------------------------------------------------------------------
# 3. Clean variable names
# ------------------------------------------------------------------

tree_clean <- tree_raw %>%
  clean_names()

# ------------------------------------------------------------------
# 4. Recode variables
# ------------------------------------------------------------------

tree_clean <- tree_clean %>%
  mutate(
    species = factor(species),
    light = factor(light),
    microbe = factor(microbe)
  )

# ------------------------------------------------------------------
# 5. Save cleaned data
# ------------------------------------------------------------------

save(
  tree_clean,
  file = "data/tree_clean.RData"
)

