# RDM
# Title:Malaria Papua New Genea
# Subject: "Research Data Management"
# Author: "Sumin Kim"

# Prep -------------------------------------------------------------------------
rm(list = ls()) 
setwd("/Users/suminkim/Desktop/rdm2025_data") 
getwd()

# Install packages and load libraries
install.packages('tidyverse', type = 'binary')
install.packages('dplyr',type = 'binary')
install.packages("ggplot2")
install.packages("dplyr") #Tools to manipulate data
install.packages("janitor") #Tools to clean the dirty data
library(dplyr)
library(ggplot2)
library(haven)
library(dplyr)
library(ggplot2)
library(lubridate)
library(janitor)
library(broom)
library(haven)
library(readr)
library(knitr)
library(skimr)
library(dataMaid)
library(writexl)
library(qacBase)
library(stringr)

# Load the data files
hh_data <-read_dta('hh-data.dta') 
individual <-read_dta('practical_individual.dta')
village <-read.csv('village info.csv')

# Clean ------------------------------------------------------------------------
# 1. Household data

# 1.1 Clean Household ID
hh_data <- hh_data %>% mutate(hhid = trimws(hhid)) %>% mutate(hhid = as.numeric(hhid)) #Delete the leading spaces and convert to numeric

# 1.2 Clean Province name 
hh_data <- hh_data %>% mutate(province = gsub(" ", "", province)) #Excess spaces between character are deleted

# 1.3 Rename Villagecode to vcode
names(hh_data)[names(hh_data) == "villagecode"] <- "vcode"

# 1.4 Replace typo: "IM0" to "IMO"
hh_data <- hh_data %>% mutate(vcode = gsub("0", "O", vcode))

# 1.5. Remove duplicates: duplicate is when a row has same hhid,villagecode, and date_final
hh_data <- hh_data %>% distinct()

# 2. Individual data
# 2.1 Replace typo: '7O' to '70'. Then convert the datatype to numeric.
individual <- individual %>% mutate(age = gsub("0","O",age),age=as.numeric(age)) 

# 2.2 Remove duplicates
individual <- individual %>% distinct()

# 2.3 Change the sex from character to factor 
individual$sex <- as.factor(individual$sex)

# 2.4 Delete the individuals with missing age <- Actually, I decided to keep these subjects. 
sum(is.na(merged_data_all$age)) #Result: 2612 subjects have N/A for age. 


# 3. Village data
# 3.1. Clean 'urban' variable to binary 0/1
unique(village$urban) #Checked result: "0","URBAN","1","urban","yes", "no", "not urban". 
village <- village %>%
  mutate(
    urban = case_when(
      urban %in% c(1, "URBAN", "1", "urban", "yes","YES") ~ 1,
      urban %in% c(0, "0","not urban","no","NOT URBAN","NO") ~ 0,
      TRUE ~ NA_real_  
    )
  )
village <- village %>% mutate(urban = coalesce(urban, 1))
village %>%filter(is.na(urban)) #check if there's N/A

# 3.2. Clean altitude: remove decimal
village <- village %>% mutate(altitude_m = as.integer(altitude_m))

# Merge ------------------------------------------------------------------------

# convert the vector to factor
individual$vcode <- as.factor(individual$vcode) 
hh_data$vcode <- as.factor(hh_data$vcode)
village$vcode <- as.factor(village$vcode)

# full-join 'hh_data' to 'individual' by 'vcode' and 'hhid'
merged_data <- full_join(individual, hh_data, by = c("vcode", "hhid"))
# full-join 'village' to 'individual' by 'vcode'
merged_data_all<- full_join(merged_data, village, by = "vcode")

# full-join village to household data by 'vcode and hhid
merged_hh_village <-full_join(hh_data,village,by = "vcode")

# Create the Codebook ----------------------------------------------------------

# Method 1: using Codebook 
Vinstall.packages("codebook")
library(codebook)
codebook(merge_individual_all)

# Method 2: using skimr
install.packages("skimr")
library(skimr)
skim(merge_individual_all)


# Complete the datashell table 1-3
# Table 1: Interviews ---------------------------------------------

# 1.1 Household response rate

# Assume missing result_rdt = interview not completed.
table(merged_data_all$result_rdt,merged_data_all$urban,useNA="ifany")

merged_data_all <- merged_data_all %>%
  mutate(interview_status = case_when(
    is.na(result_rdt) ~ "not interviewed",
    result_rdt %in% 1:5 ~ "interviewed",
    TRUE ~ "other"
  ))

merged_data_all <- merged_data_all %>%
  mutate(urban_label = ifelse(
    urban==1, "rural","urban"
  ))

table(merged_data_all$interview_status, merged_data_all$urban_label)

# 1.2 Household members with microscopy result 

unique(merged_data_all$result) # 0:Plasmodium positive, 1:Plasmodium negative, N/A: no microscopy result

merged_data_all %>% filter(urban==1,is.na(result)) %>% nrow() #result:1005
merged_data_all %>% filter(urban==0,is.na(result)) %>% nrow() #result:3364
merged_data_all %>% filter(urban==1,!is.na(result)) %>% nrow() #result:1005

merged_data_all <- merged_data_all %>% 
  mutate(microscopy = case_when(
    urban==1 & is.na(result) ~ "urban,no microscopy",
    urban==1 & !is.na(result)~ "urban,microscopy",
    urban==0 & is.na(result) ~ "rural,no microscopy",
    urban==0 & !is.na(result) ~"rural,microscopy",
    TRUE ~ "others"
  ))
table(merged_data_all$microscopy)

# Table 2: Survery Population ---------------------------------------------
# 2.1 Altitude 
merged_data_all <- merged_data_all %>% 
  mutate(altitude = case_when(
    altitude_m < 1200 ~ 0,
    altitude_m < 1600 ~ 1,
    TRUE ~ 2
  ))

table(merged_data_all$altitude)

    #Cross Check
    sum(merged_data_all$altitude_m < 1200, na.rm = TRUE)
    sum(merged_data_all$altitude_m >= 1200 & merged_data_all$altitude_m < 1600, na.rm = TRUE)
    sum(merged_data_all$altitude_m >= 1600, na.rm = TRUE)

# 2.2 Age
    
merged_data_all <- merged_data_all %>%
  mutate(survey_age = case_when(
    age < 5   ~ "<5",
    age < 10  ~ "5-9",
    age < 15  ~ "10-14",
    age < 20  ~ "15-19",
    age < 40  ~ "20-39",
    age >= 40 ~ "40+",
    is.na(age) ~ "N/A"
  ))

  sum(merged_data_all$survey_age >= 15 & merged_data_all$age < 19, na.rm = TRUE)
  sum(is.na(merged_data_all$age)) #Result: 2612 subjects have N/A for age. 

merged_data_all <- merged_data_all %>% 
  mutate(survey_age = factor(survey_age,levels = c("<5", "5-9", "10-14", "15-19", "20-39", "40+", "N/A")))
table(merged_data_all$survey_age)

# 2.3 Sex
individual$sex <- as.factor(individual$sex) #change the sex from character to factor 

merged_data_all <- merged_data_all %>% 
  mutate(gender = ifelse(sex=='M',"female","male"))

table(merged_data_all$gender)


# Table 3: Prevalence of Malaria ---------------------------------------------
merged_data_all %>% count(res_pf, res_pv, result)
##RESULT 
# res_pf res_pv result     n
# <dbl>  <dbl>  <dbl> <int>
# 1      0      0      0 13630
# 2      0      0      1     6
# 3      0      1      1    82
# 4      1      0      1   183
# 5      1      1      1     8
# 6     NA     NA      0     2
# 7     NA     NA     NA  4369

merged_data_all <- merged_data_all %>%
  mutate(diagnosis_group = case_when(
    res_pf == 0 & res_pv == 0 & result == 1 ~ "Any species",
    res_pf == 1 & res_pv == 1 & result == 1 ~ "Any species",
    is.na(res_pf) & is.na(res_pv) & result == 0 ~ "Any species",
    res_pf == 1 & result == 1 ~ "P. falciparum",
    res_pv == 1 & result == 1 ~ "P. vivax",
    !is.na(result) ~ "Tested"
  ))

#Altitude
merged_data_all <- merged_data_all %>% 
  mutate(altitude_group = case_when(
    altitude_m < 1200 ~ "<1200",
    altitude_m < 1600 ~ "1200–1599",
    altitude_m >= 1600 ~ "1600+",
    is.na(altitude_m) ~ "Missing",
    TRUE ~ "Others"
  ))

#pivot
summary_table <- merged_data_all %>%
  count(altitude_group, diagnosis_group) %>%
  pivot_wider(
    names_from = diagnosis_group,
    values_from = n,
    values_fill = 0
  )
print(summary_table)

#age
merged_data_all <- merged_data_all %>%
  mutate(survey_age = factor(survey_age,
                             levels = c("<5", "5-9", "10-14", "15-19", "20-39", "40+", "N/A")))

table(merged_data_all$altitude_group,merged_data_all$diagnosis_group)
table(merged_data_all$survey_age,merged_data_all$diagnosis_group)
table(merged_data_all$sex,merged_data_all$diagnosis_group)


##### Research Question 1  #####
# How many household interviews were conducted per month?

# Extract month string values
merged_data_all$months <- str_sub (merged_data_all$date_final,1,3) %>% as.factor()

# Make a vector for months
month_levels <-c("Jan", "Feb", "Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
month_counts <- merged_data_all %>% count(months) %>% mutate(months = factor(months, levels = month_levels)) %>%
  complete(months, fill = list(n = 0))

# Fill in the vector with count per month
month_counts <- merged_data_all %>%
  count(months) %>%
  mutate(months = factor(months, levels = month_levels)) %>%
  complete(months, fill = list(n = 0)) %>%
  mutate(month_num = as.numeric(months))  # create numeric x for smoothing

# Graph the interviews per month 
ggplot(month_counts, aes(x = months, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_smooth(aes(x = month_num, y = n), 
              method = "loess", se = FALSE, color = "red", size = 1.2) +
  labs(title = "Frequency of interviews per month",
       x = "Month", y = "Count") +
  theme_minimal()

##### Research Question 2  ##### 
# 2.	Are members of households that own at least one mosquito net less likely to be infected with malaria parasites? 
# Run the logistic regression (outcome: infection status, explanatory variable: mosquito net)
# Infected: result_rdt == 1,2,3 
# Not infected: result_rdt == 4
# N/A: 5
# mosquito net: 0,1

# Ensure correct type
merged_data_all$result_rdt <- as.numeric(merged_data_all$result_rdt)

# Create binary infection outcome
merged_data_all <- merged_data_all %>%
  mutate(infection_rate = case_when(
    result_rdt %in% 1:3 ~ 1,
    result_rdt == 4 ~ 0,
    TRUE ~ NA_real_   # correct way to assign missing numeric value
  ))
table(merged_data_all$infection_rate,merged_data_all$mosquitonet)

# Optional: make sure mosquitonet is numeric or factor
merged_data_all$mosquitonet <- as.factor(merged_data_all$mosquitonet)

# Run logistic regression, removing rows with NA in relevant vars
model_logit <- glm(infection_rate ~ mosquitonet,
                   data = merged_data_all,
                   family = binomial,
                   na.action = na.omit)

# View summary
summary(model_logit)

##### Research Question 3  ##### 
#3.	Does prevalence of Plasmodium vivax and P. falciparum differ between altitudinal zones?

# Create binary infection outcome for p.vivax and p.falciparum
merged_data_all <- merged_data_all |>
  mutate(
    prevalen = case_when(
      res_pf == 1 | res_pv == 1 ~ 1,   # infected by any species
      TRUE                      ~ 0    # not infected
    )
  )

# Create categorical variable for altitude
merged_data_all <- merged_data_all |>
  mutate(
    alt_num = case_when(
      altitude_m < 1200  ~ 0,
      altitude_m < 1600  ~ 1,
      TRUE               ~ 2
    ),
    alt = factor(
      alt_num,
      levels = c(0, 1, 2),
      labels = c("<1200", "1200–1599", "1600+")
    )
  )

# Run the logistic regression where reference group is altitude <1200. 
prevalmodel <- glm(
  prevalen ~ alt,
  family = binomial,
  data   = merged_data_all,
  na.action = na.omit        # will now keep all three levels
)

summary(prevalmodel)
