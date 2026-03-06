#Load libraries
library(effectsize)
library(dplyr)
library(parameters)

#Read in data
Data <- readRDS("~/Documents/BRAINS Lab/Household structure and SES/household_structure.rds") 

#Simple regression models to test for basic associations

#Married vs Never married as a predictor, no SES covariates
mod_education <- lm(parent_education ~ married_binary, data = Data)
mod_income <- lm(demo_comb_income_v2_b ~ married_binary, data = Data)
mod_dsm_adhd <- lm(cbcl_scr_dsm5_adhd_r_b ~ married_binary, data = Data)
mod_dsm_anxiety <- lm(cbcl_scr_dsm5_anxdisord_r_b ~ married_binary, data = Data)
mod_dsm_conduct <- lm(cbcl_scr_dsm5_conduct_r_b ~ married_binary, data = Data)
mod_dsm_depression <- lm(cbcl_scr_dsm5_depress_r_b ~ married_binary, data = Data)
mod_dsm_ODD <- lm(cbcl_scr_dsm5_opposit_r_b ~ married_binary, data = Data)
mod_dsm_somatic <- lm(cbcl_scr_dsm5_somaticpr_r_b ~ married_binary, data = Data)
mod_cbcl_aggressive <- lm(cbcl_scr_syn_aggressive_r_b ~ married_binary, data = Data)
mod_cbcl_anxdep <- lm(cbcl_scr_syn_anxdep_r_b ~ married_binary, data = Data)
mod_cbcl_attention <- lm(cbcl_scr_syn_attention_r_b ~ married_binary, data = Data)
mod_cbcl_externalizing <- lm(cbcl_scr_syn_external_r_b ~ married_binary, data = Data)
mod_cbcl_internalizing <- lm(cbcl_scr_syn_internal_r_b ~ married_binary, data = Data)
mod_cbcl_rulebreak <- lm(cbcl_scr_syn_rulebreak_r_b ~ married_binary, data = Data)
mod_cbcl_social <- lm(cbcl_scr_syn_social_r_b ~ married_binary, data = Data)
mod_cbcl_somatic <- lm(cbcl_scr_syn_somatic_r_b ~ married_binary, data = Data)
mod_cbcl_thought <- lm(cbcl_scr_syn_thought_r_b ~ married_binary, data = Data)
mod_cbcl_total <- lm(cbcl_scr_syn_totprob_r_b ~ married_binary, data = Data)
mod_cbcl_depression <- lm(cbcl_scr_syn_withdep_r_b ~ married_binary, data = Data)

#Print model results
summary(mod_education)
summary(mod_income)
summary(mod_dsm_adhd)
summary(mod_dsm_anxiety)
summary(mod_dsm_conduct)
summary(mod_dsm_depression)
summary(mod_dsm_ODD)
summary(mod_dsm_somatic)
summary(mod_cbcl_aggressive)
summary(mod_cbcl_anxdep)
summary(mod_cbcl_attention)
summary(mod_cbcl_externalizing)
summary(mod_cbcl_internalizing)
summary(mod_cbcl_rulebreak)
summary(mod_cbcl_social)
summary(mod_cbcl_somatic)
summary(mod_cbcl_thought)
summary(mod_cbcl_total)
summary(mod_cbcl_depression)

#Extract results to csv
models <- list(
  mod_education          = mod_education,
  mod_income             = mod_income,
  mod_dsm_adhd           = mod_dsm_adhd,
  mod_dsm_anxiety        = mod_dsm_anxiety,
  mod_dsm_conduct        = mod_dsm_conduct,
  mod_dsm_depression     = mod_dsm_depression,
  mod_dsm_ODD            = mod_dsm_ODD,
  mod_dsm_somatic        = mod_dsm_somatic,
  mod_cbcl_aggressive    = mod_cbcl_aggressive,
  mod_cbcl_anxdep        = mod_cbcl_anxdep,
  mod_cbcl_attention     = mod_cbcl_attention,
  mod_cbcl_externalizing = mod_cbcl_externalizing,
  mod_cbcl_internalizing = mod_cbcl_internalizing,
  mod_cbcl_rulebreak     = mod_cbcl_rulebreak,
  mod_cbcl_social        = mod_cbcl_social,
  mod_cbcl_somatic       = mod_cbcl_somatic,
  mod_cbcl_thought       = mod_cbcl_thought,
  mod_cbcl_total         = mod_cbcl_total,
  mod_cbcl_depression    = mod_cbcl_depression
)

model_results <- do.call(rbind, lapply(names(models), function(name) {
  coef_table <- summary(models[[name]])$coefficients
  
  data.frame(
    model = name,
    term  = rownames(coef_table),
    coef_table,
    row.names = NULL
  )
}))

pcol <- grep("^Pr", colnames(model_results), value = TRUE)
model_results$padj_fdr <- p.adjust(model_results[[pcol]], method = "fdr")

write.csv(
  model_results,
  "~/Documents/BRAINS Lab/Household structure and SES/noses_model_results.csv",
  row.names = FALSE
)

#Simple regression models with SES, age, sex, and site covariates
#Married vs Never Married as a predictor, with SES, age, sex, and site covariates
mod_dsm_adhd_covars <- lm(cbcl_scr_dsm5_adhd_r_b ~ married_binary + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_anxiety_covars <- lm(cbcl_scr_dsm5_anxdisord_r_b ~ married_binary + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_conduct_covars <- lm(cbcl_scr_dsm5_conduct_r_b ~ married_binary + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_depression_covars <- lm(cbcl_scr_dsm5_depress_r_b ~ married_binary + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_ODD_covars <- lm(cbcl_scr_dsm5_opposit_r_b ~ married_binary + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_somatic_covars <- lm(cbcl_scr_dsm5_somaticpr_r_b ~ married_binary + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_aggressive_covars <- lm(cbcl_scr_syn_aggressive_r_b ~ married_binary + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_anxdep_covars <- lm(cbcl_scr_syn_anxdep_r_b ~ married_binary + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_attention_covars <- lm(cbcl_scr_syn_attention_r_b ~ married_binary + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_externalizing_covars <- lm(cbcl_scr_syn_external_r_b ~ married_binary + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_internalizing_covars <- lm(cbcl_scr_syn_internal_r_b ~ married_binary + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_rulebreak_covars <- lm(cbcl_scr_syn_rulebreak_r_b ~ married_binary + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_social_covars <- lm(cbcl_scr_syn_social_r_b ~ married_binary + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_somatic_covars <- lm(cbcl_scr_syn_somatic_r_b ~ married_binary + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_thought_covars <- lm(cbcl_scr_syn_thought_r_b ~ married_binary + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_total_covars <- lm(cbcl_scr_syn_totprob_r_b ~ married_binary + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_depression_covars <- lm(cbcl_scr_syn_withdep_r_b ~ married_binary + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)

#Print model results
summary(mod_dsm_adhd_covars)
summary(mod_dsm_anxiety_covars)
summary(mod_dsm_conduct_covars)
summary(mod_dsm_depression_covars)
summary(mod_dsm_ODD_covars)
summary(mod_dsm_somatic_covars)
summary(mod_cbcl_aggressive_covars)
summary(mod_cbcl_anxdep_covars)
summary(mod_cbcl_attention_covars)
summary(mod_cbcl_externalizing_covars)
summary(mod_cbcl_internalizing_covars)
summary(mod_cbcl_rulebreak_covars)
summary(mod_cbcl_social_covars)
summary(mod_cbcl_somatic_covars)
summary(mod_cbcl_thought_covars)
summary(mod_cbcl_total_covars)
summary(mod_cbcl_depression_covars)

#Extract model results to csv
models_covars <- list(
  mod_dsm_adhd_covars           = mod_dsm_adhd_covars,
  mod_dsm_anxiety_covars        = mod_dsm_anxiety_covars,
  mod_dsm_conduct_covars        = mod_dsm_conduct_covars,
  mod_dsm_depression_covars     = mod_dsm_depression_covars,
  mod_dsm_ODD_covars            = mod_dsm_ODD_covars,
  mod_dsm_somatic_covars        = mod_dsm_somatic_covars,
  mod_cbcl_aggressive_covars    = mod_cbcl_aggressive_covars,
  mod_cbcl_anxdep_covars        = mod_cbcl_anxdep_covars,
  mod_cbcl_attention_covars     = mod_cbcl_attention_covars,
  mod_cbcl_externalizing_covars = mod_cbcl_externalizing_covars,
  mod_cbcl_internalizing_covars = mod_cbcl_internalizing_covars,
  mod_cbcl_rulebreak_covars     = mod_cbcl_rulebreak_covars,
  mod_cbcl_social_covars        = mod_cbcl_social_covars,
  mod_cbcl_somatic_covars       = mod_cbcl_somatic_covars,
  mod_cbcl_thought_covars       = mod_cbcl_thought_covars,
  mod_cbcl_total_covars         = mod_cbcl_total_covars,
  mod_cbcl_depression_covars    = mod_cbcl_depression_covars
)

# 1) Extract coefficient tables into a single data.frame
model_results_covars <- do.call(rbind, lapply(names(models_covars), function(name) {
  coef_table <- summary(models_covars[[name]])$coefficients
  data.frame(
    model    = name,
    term     = rownames(coef_table),
    Estimate = coef_table[, "Estimate"],
    Std_Error = coef_table[, "Std. Error"],
    t_value  = coef_table[, "t value"],
    p_value  = coef_table[, "Pr(>|t|)"],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
})) %>%
  mutate(model = as.character(model), term = as.character(term))

# 2) Ensure p_value is numeric (some versions return factors/characters)
model_results_covars <- model_results_covars %>%
  mutate(
    p_value = as.character(p_value),
    p_value = ifelse(p_value %in% c("", "NA", "NaN", "Inf", "NA_real_"), NA_character_, p_value),
    p_value = suppressWarnings(as.numeric(p_value))
  )

# 3) Compute FDR-adjusted p-values within each model (padj_fdr)
model_results_covars <- model_results_covars %>%
  group_by(model) %>%
  mutate(padj_fdr = if (all(is.na(p_value))) NA_real_ else p.adjust(p_value, method = "fdr")) %>%
  ungroup()

# 4) Compute standardized betas for every term in every model
std_betas <- do.call(rbind, lapply(names(models_covars), function(name) {
  std_tbl <- as.data.frame(standardize_parameters(models_covars[[name]]))
  param_col <- if ("Parameter" %in% names(std_tbl)) "Parameter" else if ("Term" %in% names(std_tbl)) "Term" else stop("Unexpected column name from standardize_parameters()")
  data.frame(
    model = name,
    term  = std_tbl[[param_col]],
    std_beta = std_tbl$Std_Coefficient,
    std_CI_low = if ("CI_low" %in% names(std_tbl)) std_tbl$CI_low else NA_real_,
    std_CI_high = if ("CI_high" %in% names(std_tbl)) std_tbl$CI_high else NA_real_,
    stringsAsFactors = FALSE
  )
}))

# 5) Merge standardized betas into model_results_covars
model_results_covars <- model_results_covars %>%
  left_join(std_betas, by = c("model", "term"))

# 6) Write CSV
out_path <- "~/Documents/BRAINS Lab/Household structure and SES/covars_model_results.csv"
write.csv(model_results_covars, out_path, row.names = FALSE)
message("Wrote CSV to: ", out_path)

###Sensitivity analyses###
#Married vs never married as a predictor, with SES, age, sex, and site covariates and sex interation
mod_dsm_adhd_sex <- lm(cbcl_scr_dsm5_adhd_r_b ~ married_binary*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_anxiety_sex <- lm(cbcl_scr_dsm5_anxdisord_r_b ~ married_binary*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_conduct_sex <- lm(cbcl_scr_dsm5_conduct_r_b ~ married_binary*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_depression_sex <- lm(cbcl_scr_dsm5_depress_r_b ~ married_binary*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_ODD_sex <- lm(cbcl_scr_dsm5_opposit_r_b ~ married_binary*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_somatic_sex <- lm(cbcl_scr_dsm5_somaticpr_r_b ~ married_binary*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_aggressive_sex <- lm(cbcl_scr_syn_aggressive_r_b ~ married_binary*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_anxdep_sex <- lm(cbcl_scr_syn_anxdep_r_b ~ married_binary*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_attention_sex <- lm(cbcl_scr_syn_attention_r_b ~ married_binary*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_externalizing_sex <- lm(cbcl_scr_syn_external_r_b ~ married_binary*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_internalizing_sex <- lm(cbcl_scr_syn_internal_r_b ~ married_binary*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_rulebreak_sex <- lm(cbcl_scr_syn_rulebreak_r_b ~ married_binary*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_social_sex <- lm(cbcl_scr_syn_social_r_b ~ married_binary*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_somatic_sex <- lm(cbcl_scr_syn_somatic_r_b ~ married_binary*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_thought_sex <- lm(cbcl_scr_syn_thought_r_b ~ married_binary*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_total_sex <- lm(cbcl_scr_syn_totprob_r_b ~ married_binary*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_depression_sex <- lm(cbcl_scr_syn_withdep_r_b ~ married_binary*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)

#Print model results
summary(mod_dsm_adhd_sex)
summary(mod_dsm_anxiety_sex)
summary(mod_dsm_conduct_sex)
summary(mod_dsm_depression_sex)
summary(mod_dsm_ODD_sex)
summary(mod_dsm_somatic_sex)
summary(mod_cbcl_aggressive_sex)
summary(mod_cbcl_anxdep_sex)
summary(mod_cbcl_attention_sex)
summary(mod_cbcl_externalizing_sex)
summary(mod_cbcl_internalizing_sex)
summary(mod_cbcl_rulebreak_sex)
summary(mod_cbcl_social_sex)
summary(mod_cbcl_somatic_sex)
summary(mod_cbcl_thought_sex)
summary(mod_cbcl_total_sex)
summary(mod_cbcl_depression_sex)

#Extract model results to csv
models_sex <- list(
  mod_dsm_adhd_sex           = mod_dsm_adhd_sex,
  mod_dsm_anxiety_sex        = mod_dsm_anxiety_sex,
  mod_dsm_conduct_sex       = mod_dsm_conduct_sex,
  mod_dsm_depression_sex     = mod_dsm_depression_sex,
  mod_dsm_ODD_sex           = mod_dsm_ODD_sex,
  mod_dsm_somatic_sex        = mod_dsm_somatic_sex,
  mod_cbcl_aggressive_sex    = mod_cbcl_aggressive_sex,
  mod_cbcl_anxdep_sex        = mod_cbcl_anxdep_sex,
  mod_cbcl_attention_sex     = mod_cbcl_attention_sex,
  mod_cbcl_externalizing_sex = mod_cbcl_externalizing_sex,
  mod_cbcl_internalizing_sex = mod_cbcl_internalizing_sex,
  mod_cbcl_rulebreak_sex     = mod_cbcl_rulebreak_sex,
  mod_cbcl_social_sex        = mod_cbcl_social_sex,
  mod_cbcl_somatic_sex       = mod_cbcl_somatic_sex,
  mod_cbcl_thought_sex       = mod_cbcl_thought_sex,
  mod_cbcl_total_sex         = mod_cbcl_total_sex,
  mod_cbcl_depression_sex    = mod_cbcl_depression_sex
)

# 1) Extract coefficient tables into a single data.frame
model_results_sex <- do.call(rbind, lapply(names(models_sex), function(name) {
  coef_table <- summary(models_sex[[name]])$coefficients
  data.frame(
    model    = name,
    term     = rownames(coef_table),
    Estimate = coef_table[, "Estimate"],
    Std_Error = coef_table[, "Std. Error"],
    t_value  = coef_table[, "t value"],
    p_value  = coef_table[, "Pr(>|t|)"],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
})) %>%
  mutate(model = as.character(model), term = as.character(term))

# 2) Ensure p_value is numeric (prevents NA results from p.adjust)
model_results_sex <- model_results_sex %>%
  mutate(
    p_value = as.character(p_value),
    p_value = ifelse(p_value %in% c("", "NA", "NaN", "Inf", "NA_real_"), NA_character_, p_value),
    p_value = suppressWarnings(as.numeric(p_value))
  )

# 3) Compute GLOBAL FDR-adjusted p-values (across all rows, preserving order)
if (all(is.na(model_results_sex$p_value))) {
  model_results_sex$padj_fdr <- NA_real_
} else {
  pvec <- model_results_sex$p_value
  non_na_idx <- which(!is.na(pvec))
  padj <- rep(NA_real_, length(pvec))
  padj[non_na_idx] <- p.adjust(pvec[non_na_idx], method = "fdr")
  model_results_sex$padj_fdr <- padj
}

# 4) Compute standardized betas for every term in every model
std_betas_sex <- do.call(rbind, lapply(names(models_sex), function(name) {
  std_tbl <- as.data.frame(standardize_parameters(models_sex[[name]]))
  param_col <- if ("Parameter" %in% names(std_tbl)) "Parameter" else if ("Term" %in% names(std_tbl)) "Term" else stop("Unexpected column name from standardize_parameters()")
  data.frame(
    model = name,
    term  = std_tbl[[param_col]],
    std_beta = std_tbl$Std_Coefficient,
    std_CI_low = if ("CI_low" %in% names(std_tbl)) std_tbl$CI_low else NA_real_,
    std_CI_high = if ("CI_high" %in% names(std_tbl)) std_tbl$CI_high else NA_real_,
    stringsAsFactors = FALSE
  )
}))

# 5) Merge standardized betas into model_results_sex
model_results_sex <- model_results_sex %>%
  left_join(std_betas_sex, by = c("model", "term"))

# 7) Write CSV
out_path <- "~/Documents/BRAINS Lab/Household structure and SES/sex_model_results.csv"
write.csv(model_results_sex, out_path, row.names = FALSE)
message("Wrote CSV to: ", out_path)

#Married vs never married as a predictor, with SES, crpbi AND parent monitor, age, sex, and site covariates
mod_dsm_adhd_parent <- lm(cbcl_scr_dsm5_adhd_r_b ~ married_binary + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_anxiety_parent <- lm(cbcl_scr_dsm5_anxdisord_r_b ~ married_binary + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_conduct_parent <- lm(cbcl_scr_dsm5_conduct_r_b ~ married_binary + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_depression_parent <- lm(cbcl_scr_dsm5_depress_r_b ~ married_binary + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_ODD_parent <- lm(cbcl_scr_dsm5_opposit_r_b ~ married_binary + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_somatic_parent <- lm(cbcl_scr_dsm5_somaticpr_r_b ~ married_binary + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_aggressive_parent <- lm(cbcl_scr_syn_aggressive_r_b ~ married_binary + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_anxdep_parent <- lm(cbcl_scr_syn_anxdep_r_b ~ married_binary + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_attention_parent <- lm(cbcl_scr_syn_attention_r_b ~ married_binary + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_externalizing_parent <- lm(cbcl_scr_syn_external_r_b ~ married_binary + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_internalizing_parent <- lm(cbcl_scr_syn_internal_r_b ~ married_binary + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_rulebreak_parent <- lm(cbcl_scr_syn_rulebreak_r_b ~ married_binary + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_social_parent <- lm(cbcl_scr_syn_social_r_b ~ married_binary + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_somatic_parent <- lm(cbcl_scr_syn_somatic_r_b ~ married_binary + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_thought_parent <- lm(cbcl_scr_syn_thought_r_b ~ married_binary + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_total_parent <- lm(cbcl_scr_syn_totprob_r_b ~ married_binary + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_depression_parent <- lm(cbcl_scr_syn_withdep_r_b ~ married_binary + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)

#Print model results
summary(mod_dsm_adhd_parent)
summary(mod_dsm_anxiety_parent)
summary(mod_dsm_conduct_parent)
summary(mod_dsm_depression_parent)
summary(mod_dsm_ODD_parent)
summary(mod_dsm_somatic_parent)
summary(mod_cbcl_aggressive_parent)
summary(mod_cbcl_anxdep_parent)
summary(mod_cbcl_attention_parent)
summary(mod_cbcl_externalizing_parent)
summary(mod_cbcl_internalizing_parent)
summary(mod_cbcl_rulebreak_parent)
summary(mod_cbcl_social_parent)
summary(mod_cbcl_somatic_parent)
summary(mod_cbcl_thought_parent)
summary(mod_cbcl_total_parent)
summary(mod_cbcl_depression_parent)

#Extract model results to csv
models_parent <- list(
  mod_dsm_adhd_parent           = mod_dsm_adhd_parent,
  mod_dsm_anxiety_parent        = mod_dsm_anxiety_parent,
  mod_dsm_conduct_parent      = mod_dsm_conduct_parent,
  mod_dsm_depression_parent    = mod_dsm_depression_parent,
  mod_dsm_ODD_parent        = mod_dsm_ODD_parent,
  mod_dsm_somatic_parent       = mod_dsm_somatic_parent,
  mod_cbcl_aggressive_parent    = mod_cbcl_aggressive_parent,
  mod_cbcl_anxdep_parent       = mod_cbcl_anxdep_parent,
  mod_cbcl_attention_parent     = mod_cbcl_attention_parent,
  mod_cbcl_externalizing_parent = mod_cbcl_externalizing_parent,
  mod_cbcl_internalizing_parent = mod_cbcl_internalizing_parent,
  mod_cbcl_rulebreak_parent    = mod_cbcl_rulebreak_parent,
  mod_cbcl_social_parent        = mod_cbcl_social_parent,
  mod_cbcl_somatic_parent       = mod_cbcl_somatic_parent,
  mod_cbcl_thought_parent       = mod_cbcl_thought_parent,
  mod_cbcl_total_parent        = mod_cbcl_total_parent,
  mod_cbcl_depression_parent    = mod_cbcl_depression_parent
)

# 1) Extract coefficient tables
model_results_parent <- do.call(rbind, lapply(names(models_parent), function(name) {
  coef_table <- summary(models_parent[[name]])$coefficients
  data.frame(
    model     = name,
    term      = rownames(coef_table),
    Estimate  = coef_table[, "Estimate"],
    Std_Error = coef_table[, "Std. Error"],
    t_value   = coef_table[, "t value"],
    p_value   = coef_table[, "Pr(>|t|)"],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
})) %>%
  mutate(
    model = as.character(model),
    term  = as.character(term),
    p_value = suppressWarnings(as.numeric(p_value))
  )

# 2) GLOBAL FDR correction (across all rows)
model_results_parent$padj_fdr <- p.adjust(model_results_parent$p_value, method = "fdr")

# 3) Extract standardized betas
std_betas_parent <- do.call(rbind, lapply(names(models_parent), function(name) {
  std_tbl <- as.data.frame(standardize_parameters(models_parent[[name]]))
  
  param_col <- if ("Parameter" %in% names(std_tbl)) "Parameter" else "Term"
  
  data.frame(
    model = name,
    term  = std_tbl[[param_col]],
    std_beta = std_tbl$Std_Coefficient,
    stringsAsFactors = FALSE
  )
}))

# 4) Merge standardized betas
model_results_parent <- model_results_parent %>%
  left_join(std_betas_parent, by = c("model", "term"))

# 5) Write CSV
out_path <- "~/Documents/BRAINS Lab/Household structure and SES/parent_model_results.csv"
write.csv(model_results_parent, out_path, row.names = FALSE)

message("Wrote CSV to: ", out_path)

#Married vs never married as a predictor, with SES, crpbi AND parent monitor, age, sex, and site covariates and sex interation
mod_dsm_adhd_sex2 <- lm(cbcl_scr_dsm5_adhd_r_b ~ married_binary*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_anxiety_sex2 <- lm(cbcl_scr_dsm5_anxdisord_r_b ~ married_binary*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_conduct_sex2 <- lm(cbcl_scr_dsm5_conduct_r_b ~ married_binary*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_depression_sex2 <- lm(cbcl_scr_dsm5_depress_r_b ~ married_binary*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_ODD_sex2 <- lm(cbcl_scr_dsm5_opposit_r_b ~ married_binary*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_somatic_sex2 <- lm(cbcl_scr_dsm5_somaticpr_r_b ~ married_binary*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_aggressive_sex2 <- lm(cbcl_scr_syn_aggressive_r_b ~ married_binary*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_anxdep_sex2 <- lm(cbcl_scr_syn_anxdep_r_b ~ married_binary*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_attention_sex2 <- lm(cbcl_scr_syn_attention_r_b ~ married_binary*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_externalizing_sex2 <- lm(cbcl_scr_syn_external_r_b ~ married_binary*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_internalizing_sex2 <- lm(cbcl_scr_syn_internal_r_b ~ married_binary*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_rulebreak_sex2 <- lm(cbcl_scr_syn_rulebreak_r_b ~ married_binary*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_social_sex2 <- lm(cbcl_scr_syn_social_r_b ~ married_binary*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_somatic_sex2 <- lm(cbcl_scr_syn_somatic_r_b ~ married_binary*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_thought_sex2 <- lm(cbcl_scr_syn_thought_r_b ~ married_binary*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_total_sex2 <- lm(cbcl_scr_syn_totprob_r_b ~ married_binary*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_depression_sex2 <- lm(cbcl_scr_syn_withdep_r_b ~ married_binary*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)

#Print model results
summary(mod_dsm_adhd_sex2)
summary(mod_dsm_anxiety_sex2)
summary(mod_dsm_conduct_sex2)
summary(mod_dsm_depression_sex2)
summary(mod_dsm_ODD_sex2)
summary(mod_dsm_somatic_sex2)
summary(mod_cbcl_aggressive_sex2)
summary(mod_cbcl_anxdep_sex2)
summary(mod_cbcl_attention_sex2)
summary(mod_cbcl_externalizing_sex2)
summary(mod_cbcl_internalizing_sex2)
summary(mod_cbcl_rulebreak_sex2)
summary(mod_cbcl_social_sex2)
summary(mod_cbcl_somatic_sex2)
summary(mod_cbcl_thought_sex2)
summary(mod_cbcl_total_sex2)
summary(mod_cbcl_depression_sex2)

#Extract model results to csv
models_sex2 <- list(
  mod_dsm_adhd_sex2           = mod_dsm_adhd_sex2,
  mod_dsm_anxiety_sex2        = mod_dsm_anxiety_sex2,
  mod_dsm_conduct_sex2       = mod_dsm_conduct_sex2,
  mod_dsm_depression_sex2     = mod_dsm_depression_sex2,
  mod_dsm_ODD_sex2           = mod_dsm_ODD_sex2,
  mod_dsm_somatic_sex2        = mod_dsm_somatic_sex2,
  mod_cbcl_aggressive_sex2    = mod_cbcl_aggressive_sex2,
  mod_cbcl_anxdep_sex2        = mod_cbcl_anxdep_sex2,
  mod_cbcl_attention_sex2     = mod_cbcl_attention_sex2,
  mod_cbcl_externalizing_sex2 = mod_cbcl_externalizing_sex2,
  mod_cbcl_internalizing_sex2 = mod_cbcl_internalizing_sex2,
  mod_cbcl_rulebreak_sex2     = mod_cbcl_rulebreak_sex2,
  mod_cbcl_social_sex2        = mod_cbcl_social_sex2,
  mod_cbcl_somatic_sex2       = mod_cbcl_somatic_sex2,
  mod_cbcl_thought_sex2       = mod_cbcl_thought_sex2,
  mod_cbcl_total_sex2         = mod_cbcl_total_sex2,
  mod_cbcl_depression_sex2    = mod_cbcl_depression_sex2
)

# 1) Extract coefficient tables
model_results_sex2 <- do.call(rbind, lapply(names(models_sex2), function(name) {
  coef_table <- summary(models_sex2[[name]])$coefficients
  data.frame(
    model    = name,
    term     = rownames(coef_table),
    Estimate = coef_table[, "Estimate"],
    Std_Error = coef_table[, "Std. Error"],
    t_value  = coef_table[, "t value"],
    p_value  = coef_table[, "Pr(>|t|)"],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
})) %>%
  mutate(model = as.character(model), term = as.character(term),
         p_value = suppressWarnings(as.numeric(p_value)))

# 2) GLOBAL FDR across ALL rows
model_results_sex2$padj_fdr <- p.adjust(model_results_sex2$p_value, method = "fdr")

# 3) Extract standardized betas using standardize_parameters() ONLY
std_betas_sex2 <- do.call(rbind, lapply(names(models_sex2), function(name) {
  std_tbl <- as.data.frame(standardize_parameters(models_sex2[[name]]))
  param_col <- if ("Parameter" %in% names(std_tbl)) "Parameter" else if ("Term" %in% names(std_tbl)) "Term" else stop("Unexpected column name from standardize_parameters()")
  std_col <- if ("Std_Coefficient" %in% names(std_tbl)) "Std_Coefficient" else stop("Std_Coefficient column not found in standardize_parameters() output")
  ci_low_col  <- if ("CI_low" %in% names(std_tbl)) "CI_low" else if ("conf.low" %in% names(std_tbl)) "conf.low" else NA
  ci_high_col <- if ("CI_high" %in% names(std_tbl)) "CI_high" else if ("conf.high" %in% names(std_tbl)) "conf.high" else NA
  
  data.frame(
    model = name,
    term  = as.character(std_tbl[[param_col]]),
    std_beta = as.numeric(std_tbl[[std_col]]),
    std_CI_low = if (!is.na(ci_low_col)) as.numeric(std_tbl[[ci_low_col]]) else NA_real_,
    std_CI_high = if (!is.na(ci_high_col)) as.numeric(std_tbl[[ci_high_col]]) else NA_real_,
    stringsAsFactors = FALSE
  )
}))

# 4) Merge (exact join — no normalization)
model_results_sex2 <- model_results_sex2 %>%
  left_join(std_betas_sex2, by = c("model", "term"))

# 5) Write CSV
out_path <- "~/Documents/BRAINS Lab/Household structure and SES/sex2_model_results.csv"
write.csv(model_results_sex2, out_path, row.names = FALSE)
message("Wrote CSV to: ", out_path)

