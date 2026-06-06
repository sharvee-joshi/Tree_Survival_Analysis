############################################################
# 02_missingness_summary.R
# Tree Survival Analysis Project
#
# Purpose: Summarize missingness in the tree survival dataset
############################################################

# ------------------------------------------------------------------
# 0. Load libraries
# ------------------------------------------------------------------

source("scripts/00_libraries.R")

# ------------------------------------------------------------------
# 1. Load cleaned data
# ------------------------------------------------------------------

load("data/tree_clean.RData")

# ------------------------------------------------------------------
# 2. Create missingness summary table
# ------------------------------------------------------------------
missing_summary <- tree_clean %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "n_missing"
  ) %>%
  mutate(
    n_total = nrow(tree_clean),
    percent_missing = round(100 * n_missing / n_total, 2)
  ) %>%
  arrange(desc(percent_missing))

# ------------------------------------------------------------------
# 3. Print table
# ------------------------------------------------------------------
missing_by_group <- tree_clean %>%
  group_by(species, light, microbe) %>%
  summarise(
    n_seedlings = n(),
    n_missing_total = sum(is.na(across(everything()))),
    percent_missing_total = round(
      100 * n_missing_total / (n_seedlings * ncol(tree_clean)),
      2
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(percent_missing_total))

# View table
missing_by_group %>%
  knitr::kable(
    caption = "Missingness Summary by Variable",
    digits = 2,
    align = "lrrr"
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )

# ------------------------------------------------------------------
# 4. Missingness visualization
# ------------------------------------------------------------------

missing_plot <- gg_miss_var(tree_clean) +
  labs(
    title = "Missing Values by Variable",
    x = "Variables",
    y = "Number of Missing Values"
  )

print(missing_plot)

ggsave(
  filename = here::here("figures", "missingness_by_variable.png"),
  plot = missing_plot,
  width = 8,
  height = 5
)