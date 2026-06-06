############################################################
# 04_km_analysis.R
# Tree Survival Analysis Project
#
# Purpose: Kaplan-Meier survival curves and log-rank tests
############################################################

# ------------------------------------------------------------------
# 0. Load libraries
# ------------------------------------------------------------------

source("scripts/00_libraries.R")

# ------------------------------------------------------------------
# 1. Load cleaned data
# ------------------------------------------------------------------

load("data/tree_main.RData")

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

km_species <- survfit(
  Surv(time, event) ~ species,
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
    "#16402B",  
    "#6B8E23",  
    "#B8860B" 
  ),
  ggtheme = theme_minimal(base_size = 14)
)

km_species_plot

ggsave(
  filename = here::here("figures", "km_species.png"),
  plot = km_species_plot$plot,
  width = 8,
  height = 5
)

logrank_species <- survdiff(
  Surv(time, event) ~ species,
  data = tree_main
)

logrank_species_table <- make_logrank_table(
  logrank_object = logrank_species,
  comparison_name = "Species"
)

# ------------------------------------------------------------------
# 5. Kaplan-Meier curve and log-rank test by light treatment
# ------------------------------------------------------------------
km_light <- survfit(
  Surv(time, event) ~ light,
  data = tree_main
)

km_light_plot <- ggsurvplot(
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
    "#16402B",  
    "#B8860B" 
  ),
  ggtheme = theme_minimal(base_size = 14)
)

km_light_plot
ggsave(
  filename = here::here("figures", "km_light.png"),
  plot = km_light_plot$plot,
  width = 8,
  height = 5
)

logrank_light <- survdiff(
  Surv(time, event) ~ light,
  data = tree_main
)

logrank_light_table <- make_logrank_table(
  logrank_object = logrank_light,
  comparison_name = "Light Treatment"
)


# ------------------------------------------------------------------
# 6. Kaplan-Meier curve and log-rank test by microbial treatment
# ------------------------------------------------------------------
tree_no_none <- tree_main %>%
  filter(microbe != "None") %>%
  mutate(microbe = droplevels(factor(microbe)))

km_microbe <- survfit(
  Surv(time, event) ~ microbe,
  data = tree_no_none
)

km_microbe_plot <- ggsurvplot(
  km_microbe,
  data = tree_no_none,
  conf.int = FALSE,
  risk.table = FALSE,
  pval = FALSE,
  xlab = "Time",
  ylab = "Estimated Survival Probability",
  legend.title = "Microbe",
  legend.labs = c("Combined", "Control", "Large", "Small"),
  xlim = c(0, 60),
  break.time.by = 10,
  palette = c(
    "#16402B",
    "#6B8E23",
    "#B8860B",
    "#A7C957"
  ),
  ggtheme = theme_minimal(base_size = 14)
)

print(km_microbe_plot)
ggsave(
  filename = here::here("figures", "km_microbe.png"),
  plot = km_microbe_plot$plot,
  width = 8,
  height = 5
)

logrank_microbe <- survdiff(
  Surv(time, event) ~ microbe,
  data = tree_main
)

logrank_microbe_table <- make_logrank_table(
  logrank_object = logrank_microbe,
  comparison_name = "Microbial Treatment"
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
  file = "results/km_logrank_results.RData"
)


logrank_all %>%
  kable(
    caption = "Summary of Log-Rank Tests",
    digits = 4,
    align = "lrrr"
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )



save(
  tree_no_none,
  file = "data/tree_no_none.RData"
)
