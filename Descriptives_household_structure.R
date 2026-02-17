#Download libraries
library(dplyr)
library(tidyr)

#Read in data
Data <- readRDS("~/Documents/BRAINS Lab/Household structure and SES/household_structure.rds") 

# Filter to married_binary
Data_married_binary <- Data %>%
  select(
    married_binary,
    FEMALE_b,
    interview_age_years,
    race_ethnicity_b,
    parent_education,
    demo_comb_income_v2_b
  ) %>%
  drop_na()

#Calculating descriptives on continuous data (age)
mean(Data_married_binary$interview_age_years, na.rm = TRUE)
sd(Data_married_binary$interview_age_years, na.rm = TRUE)

#Calculating descriptives on count data or categorical data (sex, race/ethnicity, parent education, and income)
count(Data_married_binary, FEMALE_b) 
count(Data_married_binary, race_ethnicity_b)
count(Data_married_binary, parent_education)
count(Data_married_binary, demo_comb_income_v2_b)
count(Data_married_binary, married_binary)

### To get percentages -> Percentages = (Variable frequency / total sample size) * 100
#Sex: Male
(4542/nrow(Data_married_binary)) * 100
#Sex: Female
(4182/nrow(Data_married_binary)) * 100

#Race_ethnicity: 1=White
(5018/nrow(Data_married_binary))*100
#Race_ethnicity: 2=Black/ African American
(1116/nrow(Data_married_binary)) * 100 
#Race_ethnicity: 3=Hispanic
(1486/nrow(Data_married_binary)) * 100
#Race_ethnicity: 4=Other + 5=Other
(1104/nrow(Data_married_binary)) * 100

# Parent education -> parent_education_b 
# 1 - 11 = No degree;
(269/nrow(Data_married_binary))*100
# 12 = High school
(860/nrow(Data_married_binary))*100
# 13 = Some college
(1318/nrow(Data_married_binary))*100
# 14 = Associate's degree
(1060/nrow(Data_married_binary))*100
# 16 = Bachelor's degree
(2699/nrow(Data_married_binary))*100
# 18 = Master's degree
(1900/nrow(Data_married_binary))*100
# 20 = Professional/ Doctoral degree)
(618/nrow(Data_married_binary))*100

# Income -> demo_comb_income_v2_b
# 1 = < 5000
(277/nrow(Data_married_binary))*100
# 2 = 5000-11999
(273/nrow(Data_married_binary))*100
# 3 = 12000-15999
(174/nrow(Data_married_binary))*100
# 4 = 16000-24999
(316/nrow(Data_married_binary))*100
# 5 = 25000 - 34999
(429/nrow(Data_married_binary))*100
# 6 = 35000 - 49999
(604/nrow(Data_married_binary))*100
# 7 = 50000 - 74999;
(1102/nrow(Data_married_binary))*100
# 8 = 75000 - 99999; 
(1331/nrow(Data_married_binary))*100
# 9 = 100000 - 199999; 
(3042/nrow(Data_married_binary))*100
# 10 = > or equal to 200000; 
(1176/nrow(Data_married_binary))*100

#Marital Status -> demo_prnt_marital_v2_b
# Never Married
(1252/nrow(Data_married_binary)) * 100
# Married
(7472/nrow(Data_married_binary)) * 100

### Supplement Table

# 1A. Continuous descriptives by married_factor (mean, sd, N)
cont_summary <- Data %>%
  group_by(married_factor) %>%
  summarise(
    n = n(),
    mean_age = mean(interview_age_years, na.rm = TRUE),
    sd_age   = sd(interview_age_years, na.rm = TRUE),
    .groups = "drop"
  )

print(cont_summary)

# 1B. Function to get counts and percentages for any categorical variable
cat_by_group <- function(df, cat_var) {
  # cat_var can be unquoted, e.g. cat_by_group(Data, FEMALE_b)
  df %>%
    group_by(married_factor, !!enquo(cat_var)) %>%
    tally(name = "count") %>%
    group_by(married_factor) %>%
    mutate(percent = count / sum(count) * 100) %>%
    arrange(married_factor, desc(count)) %>%
    ungroup()
}

# Examples: sex, race, parent education, income, married_binary
sex_table    <- cat_by_group(Data, FEMALE_b)
race_table   <- cat_by_group(Data, race_ethnicity_b)
edu_table    <- cat_by_group(Data, parent_education)
income_table <- cat_by_group(Data, demo_comb_income_v2_b)


# View one example
print(sex_table)
print(race_table)
print(edu_table)
print(income_table)