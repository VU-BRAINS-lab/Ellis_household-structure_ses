# Load libraries
library(purrr)
library(broom)
library(dplyr)

# Read in data
ABCD <- readRDS("~/Desktop/ABCD_5.1.rds")

# Define brain regions for analyses
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
  "smri_vol_scs_subcorticalgv_b",
  "smri_vol_scs_intracranialv_b"
)

# Pull all variables needed
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
    pmq_y_ss_mean_b,
    crpbi_y_ss_parent_b,
    crpbi_y_ss_caregiver_b,
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
    famhx_ss_fath_prob_alc_p_b,
    famhx_ss_fath_prob_dg_p_b,
    famhx_ss_fath_prob_dprs_p_b,
    famhx_ss_fath_prob_hspd_p_b,
    famhx_ss_fath_prob_ma_p_b,
    famhx_ss_fath_prob_nrv_p_b,
    famhx_ss_fath_prob_prf_p_b,
    famhx_ss_fath_prob_scd_p_b,
    famhx_ss_fath_prob_trb_p_b,
    famhx_ss_fath_prob_vs_p_b,
    famhx_ss_moth_prob_alc_p_b,
    famhx_ss_moth_prob_dg_p_b,
    famhx_ss_moth_prob_dprs_p_b,
    famhx_ss_moth_prob_hspd_p_b,
    famhx_ss_moth_prob_ma_p_b,
    famhx_ss_moth_prob_nrv_p_b,
    famhx_ss_moth_prob_prf_p_b,
    famhx_ss_moth_prob_scd_p_b,
    famhx_ss_moth_prob_trb_p_b,
    famhx_ss_moth_prob_vs_p_b,
    all_of(GMV_variables),
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

# Check the sample size
nrow(Data)

# Convert age to years and move it next to the original variable
Data$interview_age_years <- Data$interview_age_b / 12
Data <- Data %>% relocate(interview_age_years, .after=interview_age_b)

# ICV variable has high variance diving by 1000
Data$smri_vol_scs_intracranialv_adj <- Data$smri_vol_scs_intracranialv_b / 1000
Data <- Data %>% relocate(smri_vol_scs_intracranialv_adj, .after=smri_vol_scs_intracranialv_b)

# Create parent_mental_health variable
Data$parent_mental_health <- rowSums(
  Data[, c(
    "famhx_ss_fath_prob_alc_p_b",
    "famhx_ss_fath_prob_dg_p_b",
    "famhx_ss_fath_prob_dprs_p_b",
    "famhx_ss_fath_prob_hspd_p_b",
    "famhx_ss_fath_prob_ma_p_b",
    "famhx_ss_fath_prob_nrv_p_b",
    "famhx_ss_fath_prob_prf_p_b",
    "famhx_ss_fath_prob_scd_p_b",
    "famhx_ss_fath_prob_trb_p_b",
    "famhx_ss_fath_prob_vs_p_b",
    "famhx_ss_moth_prob_alc_p_b",
    "famhx_ss_moth_prob_dg_p_b",
    "famhx_ss_moth_prob_dprs_p_b",
    "famhx_ss_moth_prob_hspd_p_b",
    "famhx_ss_moth_prob_ma_p_b",
    "famhx_ss_moth_prob_nrv_p_b",
    "famhx_ss_moth_prob_prf_p_b",
    "famhx_ss_moth_prob_scd_p_b",
    "famhx_ss_moth_prob_trb_p_b",
    "famhx_ss_moth_prob_vs_p_b"
  )],
  na.rm = TRUE
)

# Recode marital status, create binary variables, and move next to original variable
Data$married_factor <- factor(
  as.character(Data$demo_prnt_marital_v2_b),
  levels = c("1","2","3","4","5","6"),
  labels = c(
    "Married",
    "Widowed",
    "Divorced",
    "Separated",
    "Never Married",
    "Living with Partner"
  )
)
Data <- Data %>% relocate(married_factor, .after = demo_prnt_marital_v2_b)

# Married = 1, Never married = 0, all others = NA
Data$married_binary <- ifelse(
  Data$married_factor == "Married", 1,
  ifelse(Data$married_factor == "Never Married", 0, NA)
)
Data <- Data %>% relocate(married_binary, .after = demo_prnt_marital_v2_b)

# Married or Living with partner = 1
# Never married, Divorced, Separated, Widowed = 0
Data$two_adult_household <- ifelse(
  Data$married_factor %in% c("Married", "Living with Partner"), 1,
  ifelse(
    Data$married_factor %in% c(
      "Never Married", "Divorced", "Separated", "Widowed"
    ), 0, NA
  )
)
Data <- Data %>% relocate(two_adult_household, .after = demo_prnt_marital_v2_b)

# Create 5-level relationship_status variable (Married as reference = 0)
Data$relationship_status <- factor(
  Data$married_factor,
  levels = c(
    "Married",
    "Never married",
    "Divorced",
    "Separated",
    "Living with Partner"
  ),
  labels = c(
    "0",  # Married (reference)
    "1",  # Never Married
    "2",  # Divorced
    "3",  # Separated
    "4"   # Living with Partner
  )
)

# Move next to original variable
Data <- Data %>% relocate(relationship_status, .after = demo_prnt_marital_v2_b)

# convert factor labels "0".."4" to numeric 0-4
Data$relationship_status_num <- as.numeric(as.character(Data$relationship_status))

Data$rel_never     <- ifelse(Data$relationship_status_num == 1, 1, 0)
Data$rel_divorced  <- ifelse(Data$relationship_status_num == 2, 1, 0)
Data$rel_separated <- ifelse(Data$relationship_status_num == 3, 1, 0)
Data$rel_lwp       <- ifelse(Data$relationship_status_num == 4, 1, 0)

isna_rs <- is.na(Data$relationship_status_num)
Data$rel_never[isna_rs]     <- NA
Data$rel_divorced[isna_rs]  <- NA
Data$rel_separated[isna_rs] <- NA
Data$rel_lwp[isna_rs]       <- NA

Data <- Data %>%
  relocate(
    rel_never,
    rel_divorced,
    rel_separated,
    rel_lwp,
    .after = relationship_status
  )

# Define variables that will be included in the psychopathology models and the demographics table
vars_of_interest <- c(
  "interview_age_years",
  "FEMALE_b",
  "race_ethnicity_b",
  "demo_comb_income_v2_b",
  "parent_education",
  "married_factor",
  "parent_mental_health",
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

# Sample size for psychopathology analyses
nrow(Data)

# Save as .rds
saveRDS(Data, "~/Documents/BRAINS Lab/Household structure and SES/household_structure.rds")

## Pull brain data for Mplus
# Exclude participants who failed MRI quality control (excluded_missing = 0 or NA)
BrainData <- Data[Data$excluded_missing == 1 & !is.na(Data$excluded_missing), ]

# Sample size for brain analyses
nrow(BrainData)

# Remove the marital factor column since Mplus can't take string variables
BrainData$married_factor <- NULL

# Recode family 0 as 1 since Mplus won't allow non-positive integers for clustering variables in version 8.11
# There is no family = 1 in ABCD, so this won't overwrite anything
BrainData$rel_family_id_b[BrainData$rel_family_id_b == 0] <- 1

# Save variable names for the Mplus script
write.table(names(BrainData),
            "~/Documents/BRAINS Lab/Household structure and SES/household_structure_variables.csv",
            row.names = FALSE,
            col.names = FALSE,
            sep = ",")

# Write Mplus-ready .dat file (tab-separated)
write.table(BrainData, "~/Documents/BRAINS Lab/Household structure and SES/household_structure.dat",
            quote = FALSE,
            sep = " ",
            row.names = FALSE,
            col.names = FALSE,
            na = ".")