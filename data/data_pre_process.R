rm(list = ls())
##------------------------------------------------------------------------------
## Load libraries 
##------------------------------------------------------------------------------
library(haven)
library(dplyr)
library(readr)

##------------------------------------------------------------------------------
## Data pre-processing (water consumption)
##------------------------------------------------------------------------------
## Download original data from 
## https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN1/22633&version=1.1 
## Save 090113_TotWatDat_cor_merge_Price.dta file in data folder

# Load original data
original_data = read_dta("090113_TotWatDat_cor_merge_Price.dta")

# Select variables and save as .csv file
original_data %>% mutate(Y = jun07_3x+jul07_3x+aug07_3x+sep07_3x, D = treatment) %>%
  select(Y,D,jun06,jul06,aug06,sep06,oct06,nov06,dec06,jan07,feb07,mar07,apr07_3x,may07_3x) %>%
  write_csv("data_ferraroprice.csv")

##------------------------------------------------------------------------------
## Data pre-processing (Oregon health insurance experiment)
##------------------------------------------------------------------------------
## Download original data from 
## https://www.nber.org/research/data/oregon-health-insurance-experiment-data
## Save the folder OHIE_Public_Use_Files in data folder

# Load original data
oregonhie_descriptive_vars = read_dta("OHIE_Public_Use_Files/OHIE_Data/oregonhie_descriptive_vars.dta")
oregonhie_survey12m_vars = read_dta("OHIE_Public_Use_Files/OHIE_Data/oregonhie_survey12m_vars.dta")

# Select variables and save as .csv file
dat_12m = oregonhie_survey12m_vars %>%
  filter(sample_12m_resp==1) %>%
  select(person_id, race_hisp_12m, race_white_12m, race_black_12m, race_asian_12m, race_amerindian_12m,
         race_pacific_12m, race_other_qn_12m, edu_12m, hhinc_pctfpl_12m, female_12m,
         doc_num_mod_12m, er_num_mod_12m, hosp_num_mod_12m, happiness_12m, health_gen_12m)

treatment = oregonhie_descriptive_vars %>% select(person_id, treatment, numhh_list)

data = dat_12m %>% 
  inner_join(treatment, by = 'person_id')

data %>% write_csv('data_oregon_12m.csv')