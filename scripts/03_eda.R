############################################################
# 03_eda.R
# Tree Survival Analysis Project
#
# Purpose: Exploratory data analysis for the tree survival dataset
############################################################

# ------------------------------------------------------------------
# 0. Load libraries
# ------------------------------------------------------------------

source("00_libraries.R")

# ------------------------------------------------------------------
# 1. Load cleaned data
# ------------------------------------------------------------------

load("tree_clean.RData")

# ------------------------------------------------------------------
# 2. Grab Data Dimensions
# ------------------------------------------------------------------

data_dimensions <- tibble(
  `Number of Observations` = nrow(tree_clean),
  `Number of Variables` = ncol(tree_clean)
)

data_dimensions %>%
  knitr::kable(
    caption = "Dataset Dimensions",
    align = "rr"
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )

# ------------------------------------------------------------------
# 3. Categorical variable summaries
# ------------------------------------------------------------------

species_summary <- tree_clean %>%
  count(species, name = "Count") %>%
  mutate(
    Percent = round(100 * Count / sum(Count), 2)
  )

light_summary <- tree_clean %>%
  count(light, name = "Count") %>%
  mutate(
    Percent = round(100 * Count / sum(Count), 2)
  )

microbe_summary <- tree_clean %>%
  count(microbe, name = "Count") %>%
  mutate(
    Percent = round(100 * Count / sum(Count), 2)
  )

species_summary %>%
  knitr::kable(
    caption = "Seedling Counts by Species",
    digits = 2,
    align = "lrr"
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )

light_summary %>%
  knitr::kable(
    caption = "Seedling Counts by Light Treatment",
    digits = 2,
    align = "lrr"
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )

microbe_summary %>%
  knitr::kable(
    caption = "Seedling Counts by Microbial Treatment",
    digits = 2,
    align = "lrr"
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )

# ------------------------------------------------------------------
# 4. Survival outcome summary
# ------------------------------------------------------------------

survival_summary <- tree_clean %>%
  summarise(
    `Minimum Time` = min(time, na.rm = TRUE),
    `Median Time` = median(time, na.rm = TRUE),
    `Mean Time` = round(mean(time, na.rm = TRUE), 2),
    `Maximum Time` = max(time, na.rm = TRUE),
    `Number of Events` = sum(event == 1, na.rm = TRUE),
    `Number Censored` = sum(event == 0, na.rm = TRUE),
    `Percent Events` = round(100 * mean(event == 1, na.rm = TRUE), 2)
  )

survival_summary %>%
  knitr::kable(
    caption = "Summary of Survival Time and Event Status",
    digits = 2,
    align = "rrrrrrr"
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )



# ------------------------------------------------------------------
# 5. Survival outcome by species, light, and microbe
# ------------------------------------------------------------------

survival_by_species <- tree_clean %>%
  group_by(species) %>%
  summarise(
    `Number of Seedlings` = n(),
    `Number of Events` = sum(event == 1, na.rm = TRUE),
    `Number Censored` = sum(event == 0, na.rm = TRUE),
    `Median Time` = median(time, na.rm = TRUE),
    `Mean Time` = round(mean(time, na.rm = TRUE), 2),
    .groups = "drop"
  )

survival_by_light <- tree_clean %>%
  group_by(light) %>%
  summarise(
    `Number of Seedlings` = n(),
    `Number of Events` = sum(event == 1, na.rm = TRUE),
    `Number Censored` = sum(event == 0, na.rm = TRUE),
    `Median Time` = median(time, na.rm = TRUE),
    `Mean Time` = round(mean(time, na.rm = TRUE), 2),
    .groups = "drop"
  )

survival_by_microbe <- tree_clean %>%
  group_by(microbe) %>%
  summarise(
    `Number of Seedlings` = n(),
    `Number of Events` = sum(event == 1, na.rm = TRUE),
    `Number Censored` = sum(event == 0, na.rm = TRUE),
    `Median Time` = median(time, na.rm = TRUE),
    `Mean Time` = round(mean(time, na.rm = TRUE), 2),
    .groups = "drop"
  )

survival_by_species %>%
  knitr::kable(
    caption = "Survival Outcome Summary by Species",
    digits = 2
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )

survival_by_light %>%
  knitr::kable(
    caption = "Survival Outcome Summary by Light Treatment",
    digits = 2
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )

survival_by_microbe %>%
  knitr::kable(
    caption = "Survival Outcome Summary by Microbial Treatment",
    digits = 2
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )


# ------------------------------------------------------------------
# 6. Imputation Distributions
# ------------------------------------------------------------------

trait_compare <- tree_raw %>%
  select(
    AMF, AMF_Imp,
    Phenolics, PHN_Imp,
    NSC, NSC_Imp,
    Lignin, LIG_Imp
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Trait = case_when(
      Variable %in% c("AMF", "AMF_Imp") ~ "AMF Colonization",
      Variable %in% c("Phenolics", "PHN_Imp") ~ "Phenolics",
      Variable %in% c("NSC", "NSC_Imp") ~ "NSC",
      Variable %in% c("Lignin", "LIG_Imp") ~ "Lignin"
    ),
    Data_Type = case_when(
      Variable %in% c("AMF", "Phenolics", "NSC", "Lignin") ~ "Observed",
      TRUE ~ "Imputed"
    )
  )

trait_plot <- ggplot(trait_compare, aes(x = Value, fill = Data_Type)) +
  geom_density(alpha = 0.35, na.rm = TRUE) +
  facet_wrap(~ Trait, scales = "free", ncol = 2) +
  scale_fill_manual(
    values = c(
      "Observed" = "#FFFFFF",  # soft sage
      "Imputed"  = "#0F5132"   # forest green
    )
  ) +
  labs(
    x = "Trait value",
    y = "Density",
    fill = "Data type"
  ) 

trait_plot

# ============================================================
# Remove imputed variables and all remaining rows with NAs
# ============================================================

tree_main <- tree_clean %>%
  select(-ends_with("_imp")) %>%
  drop_na(time, event, species, light, microbe)

save(
  tree_main,
  file = "tree_main.RData"
)
