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


###Sensitivity analyses###

# Two parent vs One parent as a predictor, no SES covariates 
mod_education_2 <- lm(parent_education ~ two_adult_household, data = Data)
mod_income_2 <- lm(demo_comb_income_v2_b ~ two_adult_household, data = Data)
mod_dsm_adhd_2 <- lm(cbcl_scr_dsm5_adhd_r_b ~ two_adult_household, data = Data)
mod_dsm_anxiety_2 <- lm(cbcl_scr_dsm5_anxdisord_r_b ~ two_adult_household, data = Data)
mod_dsm_conduct_2 <- lm(cbcl_scr_dsm5_conduct_r_b ~ two_adult_household, data = Data)
mod_dsm_depression_2 <- lm(cbcl_scr_dsm5_depress_r_b ~ two_adult_household, data = Data)
mod_dsm_ODD_2 <- lm(cbcl_scr_dsm5_opposit_r_b ~ two_adult_household, data = Data)
mod_dsm_somatic_2 <- lm(cbcl_scr_dsm5_somaticpr_r_b ~ two_adult_household, data = Data)
mod_cbcl_aggressive_2 <- lm(cbcl_scr_syn_aggressive_r_b ~ two_adult_household, data = Data)
mod_cbcl_anxdep_2 <- lm(cbcl_scr_syn_anxdep_r_b ~ two_adult_household, data = Data)
mod_cbcl_attention_2 <- lm(cbcl_scr_syn_attention_r_b ~ two_adult_household, data = Data)
mod_cbcl_externalizing_2 <- lm(cbcl_scr_syn_external_r_b ~ two_adult_household, data = Data)
mod_cbcl_internalizing_2 <- lm(cbcl_scr_syn_internal_r_b ~ two_adult_household, data = Data)
mod_cbcl_rulebreak_2 <- lm(cbcl_scr_syn_rulebreak_r_b ~ two_adult_household, data = Data)
mod_cbcl_social_2 <- lm(cbcl_scr_syn_social_r_b ~ two_adult_household, data = Data)
mod_cbcl_somatic_2 <- lm(cbcl_scr_syn_somatic_r_b ~ two_adult_household, data = Data)
mod_cbcl_thought_2 <- lm(cbcl_scr_syn_thought_r_b ~ two_adult_household, data = Data)
mod_cbcl_total_2 <- lm(cbcl_scr_syn_totprob_r_b ~ two_adult_household, data = Data)
mod_cbcl_depression_2 <- lm(cbcl_scr_syn_withdep_r_b ~ two_adult_household, data = Data)

#Print model results
summary(mod_education_2)
summary(mod_income_2)
summary(mod_dsm_adhd_2)
summary(mod_dsm_anxiety_2)
summary(mod_dsm_conduct_2)
summary(mod_dsm_depression_2)
summary(mod_dsm_ODD_2)
summary(mod_dsm_somatic_2)
summary(mod_cbcl_aggressive_2)
summary(mod_cbcl_anxdep_2)
summary(mod_cbcl_attention_2)
summary(mod_cbcl_externalizing_2)
summary(mod_cbcl_internalizing_2)
summary(mod_cbcl_rulebreak_2)
summary(mod_cbcl_social_2)
summary(mod_cbcl_somatic_2)
summary(mod_cbcl_thought_2)
summary(mod_cbcl_total_2)
summary(mod_cbcl_depression_2)

#Extract results to csv
models_2 <- list(
  mod_education_2          = mod_education_2,
  mod_income_2             = mod_income_2,
  mod_dsm_adhd_2           = mod_dsm_adhd_2,
  mod_dsm_anxiety_2        = mod_dsm_anxiety_2,
  mod_dsm_conduct_2        = mod_dsm_conduct_2,
  mod_dsm_depression_2     = mod_dsm_depression_2,
  mod_dsm_ODD_2            = mod_dsm_ODD_2,
  mod_dsm_somatic_2        = mod_dsm_somatic_2,
  mod_cbcl_aggressive_2    = mod_cbcl_aggressive_2,
  mod_cbcl_anxdep_2        = mod_cbcl_anxdep_2,
  mod_cbcl_attention_2     = mod_cbcl_attention_2,
  mod_cbcl_externalizing_2 = mod_cbcl_externalizing_2,
  mod_cbcl_internalizing_2 = mod_cbcl_internalizing_2,
  mod_cbcl_rulebreak_2     = mod_cbcl_rulebreak_2,
  mod_cbcl_social_2        = mod_cbcl_social_2,
  mod_cbcl_somatic_2       = mod_cbcl_somatic_2,
  mod_cbcl_thought_2       = mod_cbcl_thought_2,
  mod_cbcl_total_2         = mod_cbcl_total_2,
  mod_cbcl_depression_2    = mod_cbcl_depression_2
)

model_results_2 <- do.call(rbind, lapply(names(models_2), function(name) {
  coef_table <- summary(models_2[[name]])$coefficients
  
  data.frame(
    model = name,
    term  = rownames(coef_table),
    coef_table,
    row.names = NULL
  )
}))

pcol <- grep("^Pr", colnames(model_results_2), value = TRUE)
model_results_2$padj_fdr <- p.adjust(model_results_2[[pcol]], method = "fdr")

write.csv(
  model_results_2,
  "~/Documents/BRAINS Lab/Household structure and SES/2v1_noses_model_results.csv",
  row.names = FALSE
)


##Sensitivity
#Simple regression models with SES, age, sex, and site covariates
#Two vs One parent as a predictor, with SES, age, sex, and site covariates
mod_dsm_adhd_covars_2 <- lm(cbcl_scr_dsm5_adhd_r_b ~ two_adult_household + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_anxiety_covars_2 <- lm(cbcl_scr_dsm5_anxdisord_r_b ~ two_adult_household + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_conduct_covars_2 <- lm(cbcl_scr_dsm5_conduct_r_b ~ two_adult_household + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_depression_covars_2 <- lm(cbcl_scr_dsm5_depress_r_b ~ two_adult_household + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_ODD_covars_2 <- lm(cbcl_scr_dsm5_opposit_r_b ~ two_adult_household + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_somatic_covars_2 <- lm(cbcl_scr_dsm5_somaticpr_r_b ~ two_adult_household + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_aggressive_covars_2 <- lm(cbcl_scr_syn_aggressive_r_b ~ two_adult_household + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_anxdep_covars_2 <- lm(cbcl_scr_syn_anxdep_r_b ~ two_adult_household + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_attention_covars_2 <- lm(cbcl_scr_syn_attention_r_b ~ two_adult_household + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_externalizing_covars_2 <- lm(cbcl_scr_syn_external_r_b ~ two_adult_household + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_internalizing_covars_2 <- lm(cbcl_scr_syn_internal_r_b ~ two_adult_household + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_rulebreak_covars_2 <- lm(cbcl_scr_syn_rulebreak_r_b ~ two_adult_household + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_social_covars_2 <- lm(cbcl_scr_syn_social_r_b ~ two_adult_household + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_somatic_covars_2 <- lm(cbcl_scr_syn_somatic_r_b ~ two_adult_household + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_thought_covars_2 <- lm(cbcl_scr_syn_thought_r_b ~ two_adult_household + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_total_covars_2 <- lm(cbcl_scr_syn_totprob_r_b ~ two_adult_household + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_depression_covars_2 <- lm(cbcl_scr_syn_withdep_r_b ~ two_adult_household + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)

#Print model results
summary(mod_dsm_adhd_covars_2)
summary(mod_dsm_anxiety_covars_2)
summary(mod_dsm_conduct_covars_2)
summary(mod_dsm_depression_covars_2)
summary(mod_dsm_ODD_covars_2)
summary(mod_dsm_somatic_covars_2)
summary(mod_cbcl_aggressive_covars_2)
summary(mod_cbcl_anxdep_covars_2)
summary(mod_cbcl_attention_covars_2)
summary(mod_cbcl_externalizing_covars_2)
summary(mod_cbcl_internalizing_covars_2)
summary(mod_cbcl_rulebreak_covars_2)
summary(mod_cbcl_social_covars_2)
summary(mod_cbcl_somatic_covars_2)
summary(mod_cbcl_thought_covars_2)
summary(mod_cbcl_total_covars_2)
summary(mod_cbcl_depression_covars_2)

#Extract model results to csv
models_covars_2 <- list(
  mod_dsm_adhd_covars_2           = mod_dsm_adhd_covars_2,
  mod_dsm_anxiety_covars_2        = mod_dsm_anxiety_covars_2,
  mod_dsm_conduct_covars_2        = mod_dsm_conduct_covars_2,
  mod_dsm_depression_covars_2     = mod_dsm_depression_covars_2,
  mod_dsm_ODD_covars_2            = mod_dsm_ODD_covars_2,
  mod_dsm_somatic_covars_2        = mod_dsm_somatic_covars_2,
  mod_cbcl_aggressive_covars_2    = mod_cbcl_aggressive_covars_2,
  mod_cbcl_anxdep_covars_2        = mod_cbcl_anxdep_covars_2,
  mod_cbcl_attention_covars_2     = mod_cbcl_attention_covars_2,
  mod_cbcl_externalizing_covars_2 = mod_cbcl_externalizing_covars_2,
  mod_cbcl_internalizing_covars_2 = mod_cbcl_internalizing_covars_2,
  mod_cbcl_rulebreak_covars_2     = mod_cbcl_rulebreak_covars_2,
  mod_cbcl_social_covars_2        = mod_cbcl_social_covars_2,
  mod_cbcl_somatic_covars_2       = mod_cbcl_somatic_covars_2,
  mod_cbcl_thought_covars_2       = mod_cbcl_thought_covars_2,
  mod_cbcl_total_covars_2         = mod_cbcl_total_covars_2,
  mod_cbcl_depression_covars_2    = mod_cbcl_depression_covars_2
)

model_results_covars_2 <- do.call(rbind, lapply(names(models_covars_2), function(name) {
  coef_table <- summary(models_covars_2[[name]])$coefficients
  
  data.frame(
    model = name,
    term  = rownames(coef_table),
    coef_table,
    row.names = NULL
  )
}))

pcol <- grep("^Pr", colnames(model_results_covars_2), value = TRUE)
model_results_covars_2$padj_fdr <- p.adjust(model_results_covars_2[[pcol]], method = "fdr")

write.csv(
  model_results_covars_2,
  "~/Documents/BRAINS Lab/Household structure and SES/2v1_covars_model_results.csv",
  row.names = FALSE
)


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

#Two vs one parents as a predictor, with SES, age, sex, and site covariates and sex interation
mod_dsm_adhd_sex_2 <- lm(cbcl_scr_dsm5_adhd_r_b ~ two_adult_household*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_anxiety_sex_2 <- lm(cbcl_scr_dsm5_anxdisord_r_b ~ two_adult_household*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_conduct_sex_2 <- lm(cbcl_scr_dsm5_conduct_r_b ~ two_adult_household*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_depression_sex_2 <- lm(cbcl_scr_dsm5_depress_r_b ~ two_adult_household*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_ODD_sex_2 <- lm(cbcl_scr_dsm5_opposit_r_b ~ two_adult_household*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_somatic_sex_2 <- lm(cbcl_scr_dsm5_somaticpr_r_b ~ two_adult_household*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_aggressive_sex_2 <- lm(cbcl_scr_syn_aggressive_r_b ~ two_adult_household*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_anxdep_sex_2 <- lm(cbcl_scr_syn_anxdep_r_b ~ two_adult_household*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_attention_sex_2 <- lm(cbcl_scr_syn_attention_r_b ~ two_adult_household*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_externalizing_sex_2 <- lm(cbcl_scr_syn_external_r_b ~ two_adult_household*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_internalizing_sex_2 <- lm(cbcl_scr_syn_internal_r_b ~ two_adult_household*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_rulebreak_sex_2 <- lm(cbcl_scr_syn_rulebreak_r_b ~ two_adult_household*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_social_sex_2 <- lm(cbcl_scr_syn_social_r_b ~ two_adult_household*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_somatic_sex_2 <- lm(cbcl_scr_syn_somatic_r_b ~ two_adult_household*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_thought_sex_2 <- lm(cbcl_scr_syn_thought_r_b ~ two_adult_household*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_total_sex_2 <- lm(cbcl_scr_syn_totprob_r_b ~ two_adult_household*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_depression_sex_2 <- lm(cbcl_scr_syn_withdep_r_b ~ two_adult_household*FEMALE_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)

#Print model results
summary(mod_dsm_adhd_sex_2)
summary(mod_dsm_anxiety_sex_2)
summary(mod_dsm_conduct_sex_2)
summary(mod_dsm_depression_sex_2)
summary(mod_dsm_ODD_sex_2)
summary(mod_dsm_somatic_sex_2)
summary(mod_cbcl_aggressive_sex_2)
summary(mod_cbcl_anxdep_sex_2)
summary(mod_cbcl_attention_sex_2)
summary(mod_cbcl_externalizing_sex_2)
summary(mod_cbcl_internalizing_sex_2)
summary(mod_cbcl_rulebreak_sex_2)
summary(mod_cbcl_social_sex_2)
summary(mod_cbcl_somatic_sex_2)
summary(mod_cbcl_thought_sex_2)
summary(mod_cbcl_total_sex_2)
summary(mod_cbcl_depression_sex_2)

#Extract model results to csv
models_sex_2 <- list(
  mod_dsm_adhd_sex_2           = mod_dsm_adhd_sex_2,
  mod_dsm_anxiety_sex_2        = mod_dsm_anxiety_sex_2,
  mod_dsm_conduct_sex_2       = mod_dsm_conduct_sex_2,
  mod_dsm_depression_sex_2     = mod_dsm_depression_sex_2,
  mod_dsm_ODD_sex_2           = mod_dsm_ODD_sex_2,
  mod_dsm_somatic_sex_2        = mod_dsm_somatic_sex_2,
  mod_cbcl_aggressive_sex_2    = mod_cbcl_aggressive_sex_2,
  mod_cbcl_anxdep_sex_2        = mod_cbcl_anxdep_sex_2,
  mod_cbcl_attention_sex_2     = mod_cbcl_attention_sex_2,
  mod_cbcl_externalizing_sex_2 = mod_cbcl_externalizing_sex_2,
  mod_cbcl_internalizing_sex_2 = mod_cbcl_internalizing_sex_2,
  mod_cbcl_rulebreak_sex_2     = mod_cbcl_rulebreak_sex_2,
  mod_cbcl_social_sex_2        = mod_cbcl_social_sex_2,
  mod_cbcl_somatic_sex_2       = mod_cbcl_somatic_sex_2,
  mod_cbcl_thought_sex_2       = mod_cbcl_thought_sex_2,
  mod_cbcl_total_sex_2         = mod_cbcl_total_sex_2,
  mod_cbcl_depression_sex_2    = mod_cbcl_depression_sex_2
)

model_results_sex_2 <- do.call(rbind, lapply(names(models_sex_2), function(name) {
  coef_table <- summary(models_sex_2[[name]])$coefficients
  
  data.frame(
    model = name,
    term  = rownames(coef_table),
    coef_table,
    row.names = NULL
  )
}))

pcol <- grep("^Pr", colnames(model_results_sex_2), value = TRUE)
model_results_sex_2$padj_fdr <- p.adjust(model_results_sex_2[[pcol]], method = "fdr")

write.csv(
  model_results_sex_2,
  "~/Documents/BRAINS Lab/Household structure and SES/2v1_sex_model_results.csv",
  row.names = FALSE
)

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

model_results_parent <- do.call(rbind, lapply(names(models_parent), function(name) {
  coef_table <- summary(models_parent[[name]])$coefficients
  
  data.frame(
    model = name,
    term  = rownames(coef_table),
    coef_table,
    row.names = NULL
  )
}))

pcol <- grep("^Pr", colnames(model_results_parent), value = TRUE)
model_results_parent$padj_fdr <- p.adjust(model_results_parent[[pcol]], method = "fdr")

write.csv(
  model_results_parent,
  "~/Documents/BRAINS Lab/Household structure and SES/parent_model_results.csv",
  row.names = FALSE
)

#Two vs One parent as a predictor, with SES, crpbi AND parent monitor, age, sex, and site covariates
mod_dsm_adhd_parent_2 <- lm(cbcl_scr_dsm5_adhd_r_b ~ two_adult_household + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_anxiety_parent_2 <- lm(cbcl_scr_dsm5_anxdisord_r_b ~ two_adult_household + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_conduct_parent_2 <- lm(cbcl_scr_dsm5_conduct_r_b ~ two_adult_household + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_depression_parent_2 <- lm(cbcl_scr_dsm5_depress_r_b ~ two_adult_household + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_ODD_parent_2 <- lm(cbcl_scr_dsm5_opposit_r_b ~ two_adult_household + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_somatic_parent_2 <- lm(cbcl_scr_dsm5_somaticpr_r_b ~ two_adult_household + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_aggressive_parent_2 <- lm(cbcl_scr_syn_aggressive_r_b ~ two_adult_household + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_anxdep_parent_2 <- lm(cbcl_scr_syn_anxdep_r_b ~ two_adult_household + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_attention_parent_2 <- lm(cbcl_scr_syn_attention_r_b ~ two_adult_household + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_externalizing_parent_2 <- lm(cbcl_scr_syn_external_r_b ~ two_adult_household + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_internalizing_parent_2 <- lm(cbcl_scr_syn_internal_r_b ~ two_adult_household + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_rulebreak_parent_2 <- lm(cbcl_scr_syn_rulebreak_r_b ~ two_adult_household + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_social_parent_2 <- lm(cbcl_scr_syn_social_r_b ~ two_adult_household + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_somatic_parent_2 <- lm(cbcl_scr_syn_somatic_r_b ~ two_adult_household + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_thought_parent_2 <- lm(cbcl_scr_syn_thought_r_b ~ two_adult_household + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_total_parent_2 <- lm(cbcl_scr_syn_totprob_r_b ~ two_adult_household + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_depression_parent_2 <- lm(cbcl_scr_syn_withdep_r_b ~ two_adult_household + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + FEMALE_b + parent_education + demo_comb_income_v2_b, data = Data)

#Print model results
summary(mod_dsm_adhd_parent_2)
summary(mod_dsm_anxiety_parent_2)
summary(mod_dsm_conduct_parent_2)
summary(mod_dsm_depression_parent_2)
summary(mod_dsm_ODD_parent_2)
summary(mod_dsm_somatic_parent_2)
summary(mod_cbcl_aggressive_parent_2)
summary(mod_cbcl_anxdep_parent_2)
summary(mod_cbcl_attention_parent_2)
summary(mod_cbcl_externalizing_parent_2)
summary(mod_cbcl_internalizing_parent_2)
summary(mod_cbcl_rulebreak_parent_2)
summary(mod_cbcl_social_parent_2)
summary(mod_cbcl_somatic_parent_2)
summary(mod_cbcl_thought_parent_2)
summary(mod_cbcl_total_parent_2)
summary(mod_cbcl_depression_parent_2)

#Extract model results to csv
models_parent_2 <- list(
  mod_dsm_adhd_parent_2           = mod_dsm_adhd_parent_2,
  mod_dsm_anxiety_parent_2        = mod_dsm_anxiety_parent_2,
  mod_dsm_conduct_parent_2      = mod_dsm_conduct_parent_2,
  mod_dsm_depression_parent_2    = mod_dsm_depression_parent_2,
  mod_dsm_ODD_parent_2        = mod_dsm_ODD_parent_2,
  mod_dsm_somatic_parent_2       = mod_dsm_somatic_parent_2,
  mod_cbcl_aggressive_parent_2    = mod_cbcl_aggressive_parent_2,
  mod_cbcl_anxdep_parent_2       = mod_cbcl_anxdep_parent_2,
  mod_cbcl_attention_parent_2     = mod_cbcl_attention_parent_2,
  mod_cbcl_externalizing_parent_2 = mod_cbcl_externalizing_parent_2,
  mod_cbcl_internalizing_parent_2 = mod_cbcl_internalizing_parent_2,
  mod_cbcl_rulebreak_parent_2    = mod_cbcl_rulebreak_parent_2,
  mod_cbcl_social_parent_2        = mod_cbcl_social_parent_2,
  mod_cbcl_somatic_parent_2       = mod_cbcl_somatic_parent_2,
  mod_cbcl_thought_parent_2       = mod_cbcl_thought_parent_2,
  mod_cbcl_total_parent_2        = mod_cbcl_total_parent_2,
  mod_cbcl_depression_parent_2    = mod_cbcl_depression_parent_2
)

model_results_parent_2 <- do.call(rbind, lapply(names(models_parent_2), function(name) {
  coef_table <- summary(models_parent_2[[name]])$coefficients
  
  data.frame(
    model = name,
    term  = rownames(coef_table),
    coef_table,
    row.names = NULL
  )
}))

pcol <- grep("^Pr", colnames(model_results_parent_2), value = TRUE)
model_results_parent_2$padj_fdr <- p.adjust(model_results_parent_2[[pcol]], method = "fdr")

write.csv(
  model_results_parent_2,
  "~/Documents/BRAINS Lab/Household structure and SES/2v1_parent_model_results.csv",
  row.names = FALSE
)

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

model_results_sex2 <- do.call(rbind, lapply(names(models_sex2), function(name) {
  coef_table <- summary(models_sex2[[name]])$coefficients
  
  data.frame(
    model = name,
    term  = rownames(coef_table),
    coef_table,
    row.names = NULL
  )
}))

pcol <- grep("^Pr", colnames(model_results_sex2), value = TRUE)
model_results_sex2$padj_fdr <- p.adjust(model_results_sex2[[pcol]], method = "fdr")

write.csv(
  model_results_sex2,
  "~/Documents/BRAINS Lab/Household structure and SES/sex2_model_results.csv",
  row.names = FALSE
)

#Two v One parent as a predictor, with SES, crpbi AND parent monitor, age, sex, and site covariates and sex interation
mod_dsm_adhd_sex2_2 <- lm(cbcl_scr_dsm5_adhd_r_b ~ two_adult_household*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_anxiety_sex2_2 <- lm(cbcl_scr_dsm5_anxdisord_r_b ~ two_adult_household*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_conduct_sex2_2 <- lm(cbcl_scr_dsm5_conduct_r_b ~ two_adult_household*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_depression_sex2_2 <- lm(cbcl_scr_dsm5_depress_r_b ~ two_adult_household*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_ODD_sex2_2 <- lm(cbcl_scr_dsm5_opposit_r_b ~ two_adult_household*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_dsm_somatic_sex2_2 <- lm(cbcl_scr_dsm5_somaticpr_r_b ~ two_adult_household*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_aggressive_sex2_2 <- lm(cbcl_scr_syn_aggressive_r_b ~ two_adult_household*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_anxdep_sex2_2 <- lm(cbcl_scr_syn_anxdep_r_b ~ two_adult_household*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_attention_sex2_2 <- lm(cbcl_scr_syn_attention_r_b ~ two_adult_household*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_externalizing_sex2_2 <- lm(cbcl_scr_syn_external_r_b ~ two_adult_household*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_internalizing_sex2_2 <- lm(cbcl_scr_syn_internal_r_b ~ two_adult_household*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_rulebreak_sex2_2 <- lm(cbcl_scr_syn_rulebreak_r_b ~ two_adult_household*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_social_sex2_2 <- lm(cbcl_scr_syn_social_r_b ~ two_adult_household*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_somatic_sex2_2 <- lm(cbcl_scr_syn_somatic_r_b ~ two_adult_household*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_thought_sex2_2 <- lm(cbcl_scr_syn_thought_r_b ~ two_adult_household*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_total_sex2_2 <- lm(cbcl_scr_syn_totprob_r_b ~ two_adult_household*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)
mod_cbcl_depression_sex2_2 <- lm(cbcl_scr_syn_withdep_r_b ~ two_adult_household*FEMALE_b + pmq_y_ss_mean_b + crpbi_y_ss_parent_b + siten_b + interview_age_years + parent_education + demo_comb_income_v2_b, data = Data)

#Print model results
summary(mod_dsm_adhd_sex2_2)
summary(mod_dsm_anxiety_sex2_2)
summary(mod_dsm_conduct_sex2_2)
summary(mod_dsm_depression_sex2_2)
summary(mod_dsm_ODD_sex2_2)
summary(mod_dsm_somatic_sex2_2)
summary(mod_cbcl_aggressive_sex2_2)
summary(mod_cbcl_anxdep_sex2_2)
summary(mod_cbcl_attention_sex2_2)
summary(mod_cbcl_externalizing_sex2_2)
summary(mod_cbcl_internalizing_sex2_2)
summary(mod_cbcl_rulebreak_sex2_2)
summary(mod_cbcl_social_sex2_2)
summary(mod_cbcl_somatic_sex2_2)
summary(mod_cbcl_thought_sex2_2)
summary(mod_cbcl_total_sex2_2)
summary(mod_cbcl_depression_sex2_2)

#Extract model results to csv
models_sex2_2 <- list(
  mod_dsm_adhd_sex2_2           = mod_dsm_adhd_sex2_2,
  mod_dsm_anxiety_sex2_2        = mod_dsm_anxiety_sex2_2,
  mod_dsm_conduct_sex2_2       = mod_dsm_conduct_sex2_2,
  mod_dsm_depression_sex2_2     = mod_dsm_depression_sex2_2,
  mod_dsm_ODD_sex2_2           = mod_dsm_ODD_sex2_2,
  mod_dsm_somatic_sex2_2        = mod_dsm_somatic_sex2_2,
  mod_cbcl_aggressive_sex2_2    = mod_cbcl_aggressive_sex2_2,
  mod_cbcl_anxdep_sex2_2        = mod_cbcl_anxdep_sex2_2,
  mod_cbcl_attention_sex2_2     = mod_cbcl_attention_sex2_2,
  mod_cbcl_externalizing_sex2_2 = mod_cbcl_externalizing_sex2_2,
  mod_cbcl_internalizing_sex2_2 = mod_cbcl_internalizing_sex2_2,
  mod_cbcl_rulebreak_sex2_2     = mod_cbcl_rulebreak_sex2_2,
  mod_cbcl_social_sex2_2        = mod_cbcl_social_sex2_2,
  mod_cbcl_somatic_sex2_2       = mod_cbcl_somatic_sex2_2,
  mod_cbcl_thought_sex2_2       = mod_cbcl_thought_sex2_2,
  mod_cbcl_total_sex2_2         = mod_cbcl_total_sex2_2,
  mod_cbcl_depression_sex2_2    = mod_cbcl_depression_sex2_2
)

model_results_sex2_2 <- do.call(rbind, lapply(names(models_sex2_2), function(name) {
  coef_table <- summary(models_sex2_2[[name]])$coefficients
  
  data.frame(
    model = name,
    term  = rownames(coef_table),
    coef_table,
    row.names = NULL
  )
}))

pcol <- grep("^Pr", colnames(model_results_sex2_2), value = TRUE)
model_results_sex2_2$padj_fdr <- p.adjust(model_results_sex2_2[[pcol]], method = "fdr")

write.csv(
  model_results_sex2_2,
  "~/Documents/BRAINS Lab/Household structure and SES/2v1_sex2_model_results.csv",
  row.names = FALSE
)

