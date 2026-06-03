############################################################
# 05_cox_models.R
# Tree Survival Analysis Project
#
# Purpose: Fit Cox proportional hazards models for tree survival
############################################################

# ------------------------------------------------------------------
# 0. Load libraries
# ------------------------------------------------------------------

source("00_libraries.R")

# ------------------------------------------------------------------
# 1. Load cleaned data
# ------------------------------------------------------------------

load("tree_main.RData")
load("tree_clean.RData")

# ------------------------------------------------------------------
# 2. Make sure categorical variables are coded as factors
# ------------------------------------------------------------------
tree_main <- tree_main %>%
  mutate(
    species = factor(species),
    light = factor(light),
    microbe = factor(microbe),
    event = as.numeric(event)
  )

glimpse(tree_main)

tree_amf <- tree_clean %>%
  drop_na(time, event, species, light, microbe) %>%
  mutate(
    species = factor(species),
    light = factor(light),
    microbe = factor(microbe),
    event = as.numeric(event),
    amf_imp_z = as.numeric(scale(amf_imp))
  )

glimpse(tree_amf)

# ------------------------------------------------------------------
# 3. Helper function for Cox model tables
# ------------------------------------------------------------------

make_cox_table <- function(cox_model) {
  
  broom::tidy(
    cox_model,
    exponentiate = TRUE,
    conf.int = TRUE
  ) %>%
    mutate(
      estimate = round(estimate, 3),
      std.error = round(std.error, 3),
      statistic = round(statistic, 3),
      p.value = round(p.value, 4),
      conf.low = round(conf.low, 3),
      conf.high = round(conf.high, 3)
    ) %>%
    rename(
      Term = term,
      `Hazard Ratio` = estimate,
      `Standard Error` = std.error,
      `Test Statistic` = statistic,
      `P-Value` = p.value,
      `Lower 95% CI` = conf.low,
      `Upper 95% CI` = conf.high
    )
}
# ------------------------------------------------------------------
# 4. Create complete-case dataset for plant trait model
# ------------------------------------------------------------------

tree_amf <- tree_clean %>%
  drop_na(time, event, species, light, microbe) %>%
  mutate(
    species = factor(species),
    light = factor(light),
    microbe = factor(microbe),
    event = as.numeric(event),
    amf_imp_z = as.numeric(scale(amf_imp))
  )

# Check whether amf_imp actually has missing values
amf_missing_check <- tibble(
  `Total Rows` = nrow(tree_amf),
  `Missing amf_imp` = sum(is.na(tree_amf$amf_imp)),
  `Missing amf_imp_z` = sum(is.na(tree_amf$amf_imp_z))
)

amf_missing_check %>%
  knitr::kable(
    caption = "Missingness Check for Imputed AMF Variable",
    digits = 2
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )

amf_model_data <- tree_clean %>%
  select(time, event, species, light, microbe, amf_imp) %>%
  drop_na(time, event, species, light, microbe, amf_imp) %>%
  mutate(
    species = factor(species),
    light = factor(light),
    microbe = factor(microbe),
    event = as.numeric(event),
    amf_imp_z = as.numeric(scale(amf_imp))
  )

nrow(amf_model_data)
# ------------------------------------------------------------------
# 5. Main Cox model refit on AMF model dataset
# ------------------------------------------------------------------
cox_main_amf <- survival::coxph(
  survival::Surv(time, event) ~ species + light + microbe,
  data = amf_model_data,
  model = TRUE
)

cox_main_amf_table <- make_cox_table(cox_main_amf)

cox_main_amf_table %>%
  knitr::kable(
    caption = "Main Cox Model Refit on AMF Model Dataset",
    digits = 4,
    align = "lrrrrrr"
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )
# ------------------------------------------------------------------
# 6. Exploratory Cox model including imputed AMF
# ------------------------------------------------------------------

cox_amf <- survival::coxph(
  survival::Surv(time, event) ~ species + light + microbe + amf_imp_z,
  data = amf_model_data,
  model = TRUE
)

cox_amf_table <- make_cox_table(cox_amf)

write.csv(
  cox_amf_table,
  file = "cox_amf_imp_results.csv",
  row.names = FALSE
)

cox_amf_table %>%
  knitr::kable(
    caption = "Exploratory Cox Model Including Imputed AMF",
    digits = 4,
    align = "lrrrrrr"
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )
# ------------------------------------------------------------------
# 7. Likelihood ratio test for adding imputed AMF
# ------------------------------------------------------------------

amf_lrt <- anova(
  cox_main_amf,
  cox_amf,
  test = "LRT"
)


print(amf_lrt)


# ------------------------------------------------------------------
# 8. Save Cox model objects and results
# ------------------------------------------------------------------

save(
  cox_main_amf,
  cox_amf,
  cox_main_amf_table,
  cox_amf_table,
  amf_lrt,
  amf_model_data,
  file = "cox_model_results.RData"
)
