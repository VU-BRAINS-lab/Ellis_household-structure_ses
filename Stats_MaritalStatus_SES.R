#Read in data
Data <- readRDS("~/Documents/BRAINS Lab/Household structure and SES/maritalses.data.rds") 

#Simple regression models to test for basic associations

#Marital status as a predictor, no SES covariates
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

#Marital status as a predictor, with SES, age, sex, and site covariates
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

model_results_covars <- do.call(rbind, lapply(names(models_covars), function(name) {
  coef_table <- summary(models_covars[[name]])$coefficients
  
  data.frame(
    model = name,
    term  = rownames(coef_table),
    coef_table,
    row.names = NULL
  )
}))

pcol <- grep("^Pr", colnames(model_results_covars), value = TRUE)
model_results_covars$padj_fdr <- p.adjust(model_results_covars[[pcol]], method = "fdr")

write.csv(
  model_results_covars,
  "~/Documents/BRAINS Lab/Household structure and SES/covars_model_results.csv",
  row.names = FALSE
)

#Sensitivity analyses

#Marital status as a predictor, with SES, age, sex, and site covariates and sex interation
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

model_results_sex <- do.call(rbind, lapply(names(models_sex), function(name) {
  coef_table <- summary(models_sex[[name]])$coefficients
  
  data.frame(
    model = name,
    term  = rownames(coef_table),
    coef_table,
    row.names = NULL
  )
}))

pcol <- grep("^Pr", colnames(model_results_sex), value = TRUE)
model_results_sex$padj_fdr <- p.adjust(model_results_sex[[pcol]], method = "fdr")

write.csv(
  model_results_sex,
  "~/Documents/BRAINS Lab/Household structure and SES/sex_model_results.csv",
  row.names = FALSE
)

