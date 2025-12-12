#Download libraries
library(dplyr)

#Read in data
Data <- readRDS("~/Documents/BRAINS Lab/Household structure and SES/maritalses.data.rds") 

#Calculating descriptives on continuous data (age)
mean(Data$interview_age_years, na.rm = TRUE)
sd(Data$interview_age_years, na.rm = TRUE)

#Calculating descriptives on count data or categorical data (sex, race/ethnicity, parent education, and income)
count(Data, FEMALE_b) 
count(Data, race_ethnicity_b)
count(Data, parent_education)
count(Data, demo_comb_income_v2_b)
count(Data, married_binary)

### To get percentages -> Percentages = (Variable frequency / total sample size) * 100
#Sex: Male
(4542/nrow(Data)) * 100
#Sex: Female
(4182/nrow(Data)) * 100

#Race_ethnicity: 1=White
(5018/nrow(Data))*100
#Race_ethnicity: 2=Black/ African American
(1116/nrow(Data)) * 100 
#Race_ethnicity: 3=Hispanic
(1486/nrow(Data)) * 100
#Race_ethnicity: 4=Other + 5=Other
(1104/nrow(Data)) * 100

# Parent education -> parent_education_b 
# 1 - 11 = No degree;
(269/nrow(Data))*100
# 12 = High school
(860/nrow(Data))*100
# 13 = Some college
(1318/nrow(Data))*100
# 14 = Associate's degree
(1060/nrow(Data))*100
# 16 = Bachelor's degree
(2699/nrow(Data))*100
# 18 = Master's degree
(1900/nrow(Data))*100
# 20 = Professional/ Doctoral degree)
(618/nrow(Data))*100

# Income -> demo_comb_income_v2_b
# 1 = < 5000
(277/nrow(Data))*100
# 2 = 5000-11999
(273/nrow(Data))*100
# 3 = 12000-15999
(174/nrow(Data))*100
# 4 = 16000-24999
(316/nrow(Data))*100
# 5 = 25000 - 34999
(429/nrow(Data))*100
# 6 = 35000 - 49999
(604/nrow(Data))*100
# 7 = 50000 - 74999;
(1102/nrow(Data))*100
# 8 = 75000 - 99999; 
(1331/nrow(Data))*100
# 9 = 100000 - 199999; 
(3042/nrow(Data))*100
# 10 = > or equal to 200000; 
(1176/nrow(Data))*100

#Marital Status -> demo_prnt_marital_v2_b
# Never Married
(1252/nrow(Data)) * 100
# Married
(7472/nrow(Data)) * 100

