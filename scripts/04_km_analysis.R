############################################################
# 04_km_analysis.R
# Tree Survival Analysis Project
#
# Purpose: Kaplan-Meier survival curves and log-rank tests
############################################################

# ------------------------------------------------------------------
# 0. Load libraries
# ------------------------------------------------------------------

source("00_libraries.R")

# ------------------------------------------------------------------
# 1. Load cleaned data
# ------------------------------------------------------------------

load("tree_main.RData")

# ------------------------------------------------------------------
# 2. Check Data Again
# ------------------------------------------------------------------

glimpse(tree_main)

tree_main <- tree_main %>%
  mutate(
    species = factor(species),
    light = factor(light),
    microbe = factor(microbe),
    event = as.numeric(event)
  )

event_check <- tree_main %>%
  count(event, name = "Count") %>%
  mutate(
    Status = case_when(
      event == 0 ~ "Censored",
      event == 1 ~ "Event",
      TRUE ~ "Missing"
    ),
    Percent = round(100 * Count / sum(Count), 2)
  ) %>%
  select(Status, event, Count, Percent)

# ------------------------------------------------------------------
# 3. Helper function for log-rank test table
# ------------------------------------------------------------------

make_logrank_table <- function(logrank_object, comparison_name) {
  
  degrees_freedom <- length(logrank_object$n) - 1
  
  tibble(
    Comparison = comparison_name,
    `Chi-Square` = round(logrank_object$chisq, 3),
    `Degrees of Freedom` = degrees_freedom,
    `P-Value` = round(
      1 - pchisq(logrank_object$chisq, df = degrees_freedom),
      4
    )
  )
}

# ------------------------------------------------------------------
# 4. Kaplan-Meier curve and log-rank test by species
# ------------------------------------------------------------------

km_species <- survival::survfit(
  survival::Surv(time, event) ~ species,
  data = tree_main
)

km_species_plot <- ggsurvplot(
  km_species,
  data = tree_main,
  pval = FALSE,
  risk.table = FALSE,
  conf.int = FALSE,
  legend.title = "Species",
  legend.labs = c("Acne", "Acru", "Acsa"),
  xlab = "Time",
  ylab = "Estimated Survival Probability",
  xlim = c(0, 60),
  break.time.by = 10,
  palette = c(
    "#16402B",  # deep forest green
    "#6B8E23",  # olive green
    "#B8860B"   # dark goldenrod
  ),
  ggtheme = theme_minimal(base_size = 14)
)

km_species_plot



logrank_species <- survival::survdiff(
  survival::Surv(time, event) ~ species,
  data = tree_main
)

logrank_species_table <- make_logrank_table(
  logrank_object = logrank_species,
  comparison_name = "Species"
)

logrank_species_table %>%
  knitr::kable(
    caption = "Log-Rank Test Comparing Survival Curves by Species",
    digits = 4,
    align = "lrrr"
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )
# ------------------------------------------------------------------
# 5. Kaplan-Meier curve and log-rank test by light treatment
# ------------------------------------------------------------------

km_light <- survival::survfit(
  survival::Surv(time, event) ~ light,
  data = tree_main
)

km_light_plot <- survminer::ggsurvplot(
  km_light,
  data = tree_main,
  conf.int = FALSE,
  risk.table = FALSE,
  pval = FALSE,
  xlab = "Time",
  ylab = "Estimated Survival Probability",
  legend.title = "Light",
  legend.labs = c("High", "Low"),
  xlim = c(0, 60),
  break.time.by = 10,
  palette = c(
    "#16402B",  # High = dark forest green
    "#B8860B"   # Low = warm golden brown
  ),
  ggtheme = theme_minimal(base_size = 14)
)

km_light_plot


logrank_light <- survival::survdiff(
  survival::Surv(time, event) ~ light,
  data = tree_main
)

logrank_light_table <- make_logrank_table(
  logrank_object = logrank_light,
  comparison_name = "Light Treatment"
)

logrank_light_table %>%
  knitr::kable(
    caption = "Log-Rank Test Comparing Survival Curves by Light Treatment",
    digits = 4,
    align = "lrrr"
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )

# ------------------------------------------------------------------
# 6. Kaplan-Meier curve and log-rank test by microbial treatment
# ------------------------------------------------------------------

km_microbe <- survival::survfit(
  survival::Surv(time, event) ~ microbe,
  data = tree_main
)

km_microbe_plot <- survminer::ggsurvplot(
  km_microbe,
  data = tree_main,
  conf.int = FALSE,
  risk.table = FALSE,
  pval = FALSE,
  xlab = "Time",
  ylab = "Estimated Survival Probability",
  legend.title = "Microbe",
  legend.labs = c("Combined", "Control", "Large", "None", "Small"),
  xlim = c(0, 60),
  break.time.by = 10,
  palette = c(
    "#16402B",  # Combined = dark forest green
    "#6B8E23",  # Control = olive green
    "#B8860B",  # Large = golden brown
    "#8B5A2B",  # None = brown
    "#A7C957"   # Small = light leaf green
  ),
  ggtheme = theme_minimal(base_size = 14)
)

print(km_microbe_plot)

logrank_microbe <- survival::survdiff(
  survival::Surv(time, event) ~ microbe,
  data = tree_main
)

logrank_microbe_table <- make_logrank_table(
  logrank_object = logrank_microbe,
  comparison_name = "Microbial Treatment"
)

logrank_microbe_table %>%
  knitr::kable(
    caption = "Log-Rank Test Comparing Survival Curves by Microbial Treatment",
    digits = 4,
    align = "lrrr"
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )

# ------------------------------------------------------------------
# 7. Combine log-rank test results into one table
# ------------------------------------------------------------------

logrank_all <- bind_rows(
  logrank_species_table,
  logrank_light_table,
  logrank_microbe_table
)

save(
  logrank_species,
  logrank_light,
  logrank_microbe,
  logrank_all,
  file = "km_logrank_results.RData"
)


logrank_all %>%
  knitr::kable(
    caption = "Summary of Log-Rank Tests",
    digits = 4,
    align = "lrrr"
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )



