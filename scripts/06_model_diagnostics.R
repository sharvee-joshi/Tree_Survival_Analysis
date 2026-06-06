############################################################
# 06_model_diagnositcs.R
# Tree Survival Analysis Project
#
# Purpose: Check Cox proportional hazards model diagnostics
############################################################

# ------------------------------------------------------------------
# 0. Load libraries
# ------------------------------------------------------------------

source("scripts/00_libraries.R")

# ------------------------------------------------------------------
# 1. Load cleaned data
# ------------------------------------------------------------------
load("results/cox_model_results.RData")

# ------------------------------------------------------------------
# 2. Helper function for proportional hazards test tables
# ------------------------------------------------------------------

make_ph_table <- function(ph_object, model_name) {
  
  as.data.frame(ph_object$table) %>%
    rownames_to_column("term") %>%
    clean_names() %>%
    mutate(
      model = model_name,
      chisq = round(chisq, 3),
      p = round(p, 4)
    ) %>%
    rename(
      Model = model,
      Term = term,
      `Chi-Square` = chisq,
      `Degrees of Freedom` = df,
      `P-Value` = p
    ) %>%
    select(Model, Term, `Chi-Square`, `Degrees of Freedom`, `P-Value`)
}
# ------------------------------------------------------------------
# 3. Proportional hazards test: main AMF-dataset Cox model
# ------------------------------------------------------------------

ph_main_amf <- cox.zph(cox_main_amf)

print(ph_main_amf)

ph_main_amf_table <- make_ph_table(
  ph_object = ph_main_amf,
  model_name = "Main Cox Model on AMF Dataset"
)

write.csv(
  ph_main_amf_table,
  file = "results/ph_test_main_amf_model.csv",
  row.names = FALSE
)

ph_main_amf_table %>%
  kable(
    caption = "Proportional Hazards Test for Main Cox Model on AMF Dataset",
    digits = 4,
    align = "llrrr"
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )
# ------------------------------------------------------------------
# 4. Proportional hazards test: exploratory Cox model with AMF
# ------------------------------------------------------------------

ph_amf <- cox.zph(cox_amf)

print(ph_amf)

ph_amf_table <- make_ph_table(
  ph_object = ph_amf,
  model_name = "Exploratory AMF Cox Model"
)

write.csv(
  ph_amf_table,
  file = "results/ph_test_amf_model.csv",
  row.names = FALSE
)

ph_amf_table %>%
  kable(
    caption = "Proportional Hazards Test for Exploratory AMF Cox Model",
    digits = 4,
    align = "llrrr"
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )
# ------------------------------------------------------------------
# 5. Combine proportional hazards test results
# ------------------------------------------------------------------

ph_all_table <- bind_rows(
  ph_main_amf_table,
  ph_amf_table
)

write.csv(
  ph_all_table,
  file = "results/ph_test_all_models.csv",
  row.names = FALSE
)

ph_all_table %>%
  kable(
    caption = "Summary of Proportional Hazards Tests",
    digits = 4,
    align = "llrrr"
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )
# ------------------------------------------------------------------
# 6. Save proportional hazards diagnostic plots
# ------------------------------------------------------------------
png(
  filename = "figures/ph_diagnostic_main__model.png",
  width = 1000,
  height = 800
)

plot(ph_main_amf)

dev.off()


png(
  filename = "figures/ph_diagnostic_exploratory_model.png",
  width = 1000,
  height = 800
)

plot(ph_amf)

dev.off()
# ------------------------------------------------------------------
# 7. Martingale residual plot for AMF
# ------------------------------------------------------------------
# This checks whether the continuous AMF term looks reasonably linear
# on the Cox model scale.

martingale_data <- tibble(
  amf_imp_z = amf_model_data$amf_imp_z,
  martingale_resid = residuals(cox_amf, type = "martingale")
)

martingale_amf_plot <- ggplot(
  martingale_data,
  aes(x = amf_imp_z, y = martingale_resid)
) +
  geom_point(alpha = 0.4) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Martingale Residuals for Imputed AMF",
    x = "Standardized Imputed AMF",
    y = "Martingale Residuals"
  )

print(martingale_amf_plot)
ggsave(
  filename = here::here("figures", "martingale_amf_plot.png"),
  plot = martingale_amf_plot,
  width = 8,
  height = 5
)


# ------------------------------------------------------------------
# 8. Model comparison using AIC
# ------------------------------------------------------------------

aic_table <- tibble(
  Model = c(
    "Main Cox model on AMF dataset",
    "Exploratory Cox model with imputed AMF"
  ),
  AIC = c(
    AIC(cox_main_amf),
    AIC(cox_amf)
  ),
  Delta_AIC = AIC - min(AIC)
) %>%
  mutate(
    AIC = round(AIC, 2),
    Delta_AIC = round(Delta_AIC, 2)
  )

write.csv(
  aic_table,
  file = "results/cox_aic_comparison.csv",
  row.names = FALSE
)

aic_table %>%
  kable(
    caption = "AIC Comparison of Cox Models",
    digits = 2,
    align = "lrr"
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )
