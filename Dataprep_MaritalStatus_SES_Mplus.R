#Load libraries
library(purrr)
library(broom)
library(dplyr)

#Read in data
ABCD <- readRDS("~/Desktop/ABCD_5.1.rds")

GMV_variables <- c(
  "smri_vol_cdk_banksstslh_b",
  "smri_vol_cdk_cdacatelh_b",
  "smri_vol_cdk_cdmdfrlh_b",
  "smri_vol_cdk_cuneuslh_b",
  "smri_vol_cdk_ehinallh_b",
  "smri_vol_cdk_fusiformlh_b",
  "smri_vol_cdk_ifpllh_b",
  "smri_vol_cdk_iftmlh_b",
  "smri_vol_cdk_ihcatelh_b",
  "smri_vol_cdk_locclh_b",
  "smri_vol_cdk_lobfrlh_b",
  "smri_vol_cdk_linguallh_b",
  "smri_vol_cdk_mobfrlh_b",
  "smri_vol_cdk_mdtmlh_b",
  "smri_vol_cdk_parahpallh_b",
  "smri_vol_cdk_paracnlh_b",
  "smri_vol_cdk_parsopclh_b",
  "smri_vol_cdk_parsobislh_b",
  "smri_vol_cdk_parstgrislh_b",
  "smri_vol_cdk_pericclh_b",
  "smri_vol_cdk_postcnlh_b",
  "smri_vol_cdk_ptcatelh_b",
  "smri_vol_cdk_precnlh_b",
  "smri_vol_cdk_pclh_b",
  "smri_vol_cdk_rracatelh_b",
  "smri_vol_cdk_rrmdfrlh_b",
  "smri_vol_cdk_sufrlh_b",
  "smri_vol_cdk_supllh_b",
  "smri_vol_cdk_sutmlh_b",
  "smri_vol_cdk_smlh_b",
  "smri_vol_cdk_frpolelh_b",
  "smri_vol_cdk_tmpolelh_b",
  "smri_vol_cdk_trvtmlh_b",
  "smri_vol_cdk_insulalh_b",
  "smri_vol_cdk_banksstsrh_b",
  "smri_vol_cdk_cdacaterh_b",
  "smri_vol_cdk_cdmdfrrh_b",
  "smri_vol_cdk_cuneusrh_b",
  "smri_vol_cdk_ehinalrh_b",
  "smri_vol_cdk_fusiformrh_b",
  "smri_vol_cdk_ifplrh_b",
  "smri_vol_cdk_iftmrh_b",
  "smri_vol_cdk_ihcaterh_b",
  "smri_vol_cdk_loccrh_b",
  "smri_vol_cdk_lobfrrh_b",
  "smri_vol_cdk_lingualrh_b",
  "smri_vol_cdk_mobfrrh_b",
  "smri_vol_cdk_mdtmrh_b",
  "smri_vol_cdk_parahpalrh_b",
  "smri_vol_cdk_paracnrh_b",
  "smri_vol_cdk_parsopcrh_b",
  "smri_vol_cdk_parsobisrh_b",
  "smri_vol_cdk_parstgrisrh_b",
  "smri_vol_cdk_periccrh_b",
  "smri_vol_cdk_postcnrh_b",
  "smri_vol_cdk_ptcaterh_b",
  "smri_vol_cdk_precnrh_b",
  "smri_vol_cdk_pcrh_b",
  "smri_vol_cdk_rracaterh_b",
  "smri_vol_cdk_rrmdfrrh_b",
  "smri_vol_cdk_sufrrh_b",
  "smri_vol_cdk_suplrh_b",
  "smri_vol_cdk_sutmrh_b",
  "smri_vol_cdk_smrh_b",
  "smri_vol_cdk_frpolerh_b",
  "smri_vol_cdk_tmpolerh_b",
  "smri_vol_cdk_trvtmrh_b",
  "smri_vol_cdk_insularh_b",
  "smri_vol_cdk_total_b",
  "smri_vol_scs_crbcortexlh_b",
  "smri_vol_scs_tplh_b",
  "smri_vol_scs_caudatelh_b",
  "smri_vol_scs_putamenlh_b",
  "smri_vol_scs_pallidumlh_b",
  "smri_vol_scs_bstem_b",
  "smri_vol_scs_hpuslh_b",
  "smri_vol_scs_amygdalalh_b",
  "smri_vol_scs_aal_b",
  "smri_vol_scs_vedclh_b",
  "smri_vol_scs_crbcortexrh_b",
  "smri_vol_scs_tprh_b",
  "smri_vol_scs_caudaterh_b",
  "smri_vol_scs_putamenrh_b",
  "smri_vol_scs_pallidumrh_b",
  "smri_vol_scs_hpusrh_b",
  "smri_vol_scs_amygdalarh_b",
  "smri_vol_scs_aar_b",
  "smri_vol_scs_vedcrh_b",
  "smri_vol_scs_subcorticalgv_b"
)

Data <- ABCD %>%
  dplyr::select(
    subnum_char_b,
    rel_family_id_b,
    np_ps_weight,
    motion_binary,
    motion_continuous,
    excluded_missing,
    siten_b,
    FEMALE_b,
    interview_age_b,
    race_ethnicity_b,
    demo_prnt_marital_v2_b,
    demo_comb_income_v2_b,
    parent_education,
    cbcl_scr_dsm5_adhd_r_b,
    cbcl_scr_dsm5_anxdisord_r_b,
    cbcl_scr_dsm5_conduct_r_b,
    cbcl_scr_dsm5_depress_r_b,
    cbcl_scr_dsm5_opposit_r_b,
    cbcl_scr_dsm5_somaticpr_r_b,
    cbcl_scr_syn_aggressive_r_b,
    cbcl_scr_syn_anxdep_r_b,
    cbcl_scr_syn_attention_r_b,
    cbcl_scr_syn_external_r_b,
    cbcl_scr_syn_internal_r_b,
    cbcl_scr_syn_rulebreak_r_b,
    cbcl_scr_syn_social_r_b,
    cbcl_scr_syn_somatic_r_b,
    cbcl_scr_syn_thought_r_b,
    cbcl_scr_syn_totprob_r_b,
    cbcl_scr_syn_withdep_r_b,
    all_of(GMV_variables),
    COIL2_b,
    COIL3_b,
    COIL4_b,
    COIL5_b,
    device_1,
    device_2,
    device_3,
    device_4,
    device_5,
    device_6,
    device_7,
    device_8,
    device_9,
    device_10,
    device_11,
    device_12,
    device_13,
    device_14,
    device_15,
    device_16,
    device_17,
    device_18,
    device_19,
    device_20,
    device_21,
    device_22,
    device_23,
    device_24,
    device_25,
    device_26,
    device_27,
    device_28,
    device_29
  )

nrow(Data)

# Convert age to years
Data$interview_age_years <- Data$interview_age_b / 12

# Recode marital status and create binary variable
Data$married_factor <- factor(Data$demo_prnt_marital_v2_b,
                              levels = c("1","2","3","4","5","6"),
                              labels = c("Married","Widowed","Divorced",
                                         "Separated","Never married","Living with partner"))
Data$married_binary <- ifelse(Data$married_factor == "Married", 1,
                              ifelse(Data$married_factor == "Never married", 0, NA))

vars_of_interest <- c(
  "interview_age_years",
  "FEMALE_b",
  "race_ethnicity_b",
  "demo_comb_income_v2_b",
  "parent_education",
  "married_binary",
  "cbcl_scr_dsm5_adhd_r_b",
  "cbcl_scr_dsm5_anxdisord_r_b",
  "cbcl_scr_dsm5_conduct_r_b",
  "cbcl_scr_dsm5_depress_r_b",
  "cbcl_scr_dsm5_opposit_r_b",
  "cbcl_scr_dsm5_somaticpr_r_b",
  "cbcl_scr_syn_aggressive_r_b",
  "cbcl_scr_syn_anxdep_r_b",
  "cbcl_scr_syn_attention_r_b",
  "cbcl_scr_syn_external_r_b",
  "cbcl_scr_syn_internal_r_b",
  "cbcl_scr_syn_rulebreak_r_b",
  "cbcl_scr_syn_social_r_b",
  "cbcl_scr_syn_somatic_r_b",
  "cbcl_scr_syn_thought_r_b",
  "cbcl_scr_syn_totprob_r_b",
  "cbcl_scr_syn_withdep_r_b"
)
# Keep only rows with no missing values in these columns
Data <- Data[complete.cases(Data[, vars_of_interest]), ]
nrow(Data)

# Save as .rds
saveRDS(Data, "~/Documents/BRAINS Lab/Household structure and SES/maritalses.data.rds")

# Exclude participants who failed MRI quality control
Data <- Data[Data$excluded_missing == '1', ]

# Remove old factor column
Data$married_factor <- NULL

# Recode parent_education to ≤10 categories
Data$parent_education_recode <- case_when(
  Data$parent_education %in% 1:11 ~ 1,      # Less than high school
  Data$parent_education == 12 ~ 2,         # High school
  Data$parent_education == 13 ~ 3,         # Some college
  Data$parent_education == 14 ~ 4,         # Associate's
  Data$parent_education == 16 ~ 5,         # Bachelor's
  Data$parent_education == 18 ~ 6,         # Master's
  Data$parent_education == 20 ~ 7          # Professional/Doctoral
)

# Convert all factor variables to numeric
factor_cols <- sapply(Data, is.factor)
Data[factor_cols] <- lapply(Data[factor_cols], as.numeric)

# Save variable names for reference
write.table(names(Data),
            "~/Documents/BRAINS Lab/Household structure and SES/maritalses.names.csv",
            row.names = FALSE,
            col.names = FALSE,
            sep = ",")

# Write Mplus-ready .dat file (tab-separated)
write.table(Data, "~/Documents/BRAINS Lab/Household structure and SES/maritalses.data.dat",
            quote = FALSE,
            sep = " ",
            row.names = FALSE,
            col.names = FALSE,
            na = ".")