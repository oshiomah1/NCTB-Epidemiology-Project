source("/Users/oshi/Desktop/TB/2_Data Wrangling.R")#Part 2 : imputation
library(tidyverse)



#select the variables and create non imputed dataset
missingness_matrix = clean.full.data.pre.imputation %>% select(age,sex,m_ethnicity, f_ethnicity, Years.of.education, smokes, diabetes, drnk_alcohol,hiv, height)
library("UpSetR")
library("purrr")
library("naniar")
library("ggpubr")
library("miselect")
library("mice")
library(caret)
library(glmnet)
library(dplyr)

#check missingness, patterns of missingness, &
#helpful documentation: https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html
# https://cran.r-project.org/web/packages/naniar/vignettes/getting-started-w-naniar.html
miss_var_summary(missingness_matrix)

gg_miss_var(missingness_matrix)

#upset plot will show patterns of missingness amongst the variables but if you have A LOT OF data  this isn't going to be a super helpful plot
gg_miss_upset(missingness_matrix, nsets = n_var_miss(missingness_matrix))


#check missingness by sex:
gg_miss_var(missingness_matrix,
            facet = sex)

#check missingness by m_ethnicity:
gg_miss_var(missingness_matrix,
            facet = m_ethnicity ) + labs(title ="Mother's Ethnicity")

#check missingness by p_ethnicity:
gg_miss_var(missingness_matrix,
            facet = f_ethnicity ) + labs(title ="Father's Ethnicity")

#check missingness by smokes:
gg_miss_var(missingness_matrix,
            facet = smokes ) + labs(title ="Smoking")

#check missingness by diabetes: 1 is yes, 0 is unknown
gg_miss_var(missingness_matrix,
            facet = diabetes ) + labs(title ="diabetes")

#check missingness by HIV: 0 is negative, 1 is positive, 2 is unkniwn
gg_miss_var(missingness_matrix,
            facet = hiv ) + labs(title ="hiv status")

#check missingness by TB STATUS: 0 is negative, 1 is positive, 2 is unkniwn
#gg_miss_var(ethnicity.recoded,
           # facet = TB_diagnosis ) + labs(title ="tb status")


#get counts of all variables
table(missingness_matrix$sex)
table(missingness_matrix$m_ethnicity)
table(missingness_matrix$f_ethnicity)
table(missingness_matrix$qual)
table(missingness_matrix$smokes)
table(missingness_matrix$diabetes)
table(missingness_matrix$drnk_alcohol)
table(missingness_matrix$hiv)

# change variables to correct format for imputation function (MICE)
missingness_matrix = missingness_matrix %>% mutate(across(c(sex, m_ethnicity,f_ethnicity,smokes, diabetes, drnk_alcohol, hiv),as.factor)) %>% mutate(across(c(age,Years.of.education,height), as.numeric))
                                        ########################
                                        ###IMPUTATION###
                                        ########################

#impute
#NOTE: Imputation output will always vary anytime it's run. I have already imputed
#and saved the rds file as "imputed_matrix_March_22_2022". Make sure you have this file saved
#however, if you want to impute yourself, you can run the mice function right below



#imputed_matrix_April_8_2022 <- mice(missingness_matrix, m=2, method= "cart", maxit= 10, printFlag = TRUE)
#saveRDS(object=imputed_matrix_April_8_2022, file="imputed_matrix_April_8_2022") 
#plot(saved_imputed_matrix3)
saved_imputed_matrix3 <- readRDS("imputed_matrix_March_22_2022")
#saved_imputed_matrix4 <- readRDS("imputed_matrix_April_8_2022")

#########################################################
####Post Imputation data wrangling############ #########
########################################################## 

#pool data

#  now we will extract  data frame to be used for imputation one, then we
# chop of HIV and merge with original dataset
imputed.1 = complete(saved_imputed_matrix3,1) %>% select(-hiv)
miss_var_summary(imputed.1)



#select columns from pre-imputed data set that were not imputed

cols.2.merge = clean.full.data.pre.imputation %>% select(-age, -sex,  -m_ethnicity, -f_ethnicity, -Years.of.education, -smokes, -diabetes, -drnk_alcohol, -height, -qual, -wkday_drink_blank, -wkend_drink_blank) 



#bind preimputed dataset with post imputed dataset

imputed.dataset.1 =  bind_cols(imputed.1,cols.2.merge) %>% relocate(age:height, .after = "sample_id")

#write to csv (OPTONAL)
#write_csv(imputed.dataset.1,"/Users/oshi/Desktop/TB/cleaned_imputed_dataset_01_13_22.csv")

#FILTER coinfected tb/HIV AND also TB UNKNOWN
imputed.dataset.1.filtered = imputed.dataset.1  %>% filter((hiv =="1" & TB_diagnosis == "control")| (hiv ==0))  %>% filter(TB_diagnosis!= "unknown")  %>% mutate(TB_diagnosis = case_when(TB_diagnosis == "case" ~ 1,TB_diagnosis == "control" ~ 0)) %>% mutate(TB_diagnosis = as.factor(TB_diagnosis)) %>% mutate(Clinic_Type = as.factor(Clinic_Type)) %>% mutate(Residence = as.factor(Residence)) %>% mutate(Birthplace = as.factor(Birthplace))

library(stringr)
#add year of birth
dob <- read.csv("/Users/oshi/Desktop/TB/dob_nctb.csv", header=T, na.strings=c(""," ","NA","N/A")) 
#chop of everything and leave last 2 years
dob$yob <- str_sub(dob$dob_final,-4,-1)
# merge dob with data
dob2 = dob %>% select("sample_id","yob")
imputed.dataset.1.dob = left_join(imputed.dataset.1.filtered,dob2,by="sample_id") %>% relocate(yob, .after = age)
imputed.dataset.1.dob$yob = as.numeric(imputed.dataset.1.dob$yob)

#make cohort variable using equation
imputed.dataset.1.dob$cohort = imputed.dataset.1.dob$yob +imputed.dataset.1.dob$Years.of.education+5
  
imputed.dataset.1.dob = imputed.dataset.1.dob %>% relocate(cohort, .after = age)
 #recategorize cohort into during and after
imputed.dataset.1.dob2 = imputed.dataset.1.dob %>% mutate(cohort2 = case_when(cohort > 1994 ~ "after",cohort < 1994 ~ "during", cohort == 1994 ~ "during")) %>% relocate(cohort2, .after = age) 

# tranform education to SES proxy

# didnt complete primary school- < =6 years = 218
# more than primary less than secondary - 7- 11 yrs  = 519
#completed high school - 12 = 122
# Tertiary > 12 yrs = 


SES.transformed = imputed.dataset.1.dob2 %>% 
  mutate(SES = case_when(Years.of.education >= 0 & Years.of.education <= 6 ~ 'Low SES(0-6)',
                         Years.of.education >= 7 & Years.of.education <= 11 ~ 'Med Low SES(7-11)',
                         Years.of.education == 12 ~ 'Med High SES(12)',
                         Years.of.education > 12 ~ 'High SES(>12)' ))%>%
  relocate(SES, .after = Years.of.education)

#SES.transformed$SES<- factor(SES.transformed$SES,levels = c("Low SES(0-7)", "Medium SES(8-11)", "High SES(>11)"), ordered = "TRUE")
#Low SES(0-6)',
 #'Med Low SES(7-11)',
#'Med High SES(12)',
#'High SES(>12)' 

#
#create contingency table
SES_tb_contingency = SES.transformed %>% tabyl(SES, TB_diagnosis)%>% 
  rename( control = "0", case = "1")
SES_group_emp_odds = SES_tb_contingency %>% mutate(empirical_odds = (SES_tb_contingency$case+0.5)/(SES_tb_contingency$control+0.5))  %>%
  mutate(log_empirical_odds = log(empirical_odds)) %>% mutate(pop_size = case+control)

SES_bias = ggplot(data = SES_group_emp_odds, mapping = aes(x = SES, y= empirical_odds, size = pop_size))+geom_point() + scale_size_binned() + labs(size = "Sample Size",y = "Empirical Odds of Active TB", x ="SES ") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text=element_text(size=14, face="bold"),axis.title.y = element_text(color="black", size=14, face="bold"))  + theme(legend.position="bottom")
# 4 new poz controls 2 new coinfected # maybe change the dataset queried?
number.POZ.controls =nrow(filter(imputed.dataset.1,hiv =="1" & TB_diagnosis == "control"))
number.coinfectd =nrow(filter(imputed.dataset.1,hiv =="1" & TB_diagnosis == "case"))
numbr.unknown =nrow(filter(imputed.dataset.1, TB_diagnosis == "unknown"))

#########################################################
####create new education variable (categorical)############ 
########################################################## 

#key: 0 years (None/0) 1-9 years (primary/1), 10-12 (secondary/2), >12 (tertiary/3)
#imputed.dataset.1.filtered.1 = imputed.dataset.1.filtered %>% mutate(Education2 = case_when(Years.of.education == 0 ~ "None",Years.of.education > 12 ~ "Tertiary", Years.of.education == 10 ~ "Secondary",Years.of.education == 11 ~ "Secondary",Years.of.education == 12 ~ "Secondary", TRUE ~ "Primary")) %>% relocate(Education2, .after = Years.of.education)
#####

#select data 4 model
#epi.model.data = imputed.dataset.1.filtered  %>% select(record_id, age,sex, Years.of.education, smokes, diabetes, drnk_alcohol,hiv, Clinic_Type)

#Create imputed dataset with renamed factors to make effect plots easier
imputed.dataset.1.filtered.a = SES.transformed %>% mutate(sex= case_when(sex == "1" ~ "Male", sex == "2" ~ "Female"))  %>% mutate(smokes = case_when(smokes== "1" ~ "Smoker", smokes == "0" ~ "Non-Smoker")) %>% mutate(drnk_alcohol = case_when(drnk_alcohol== "1" ~ "Drinker", drnk_alcohol == "0" ~ "Non-Drinker")) %>% mutate(diabetes = case_when(diabetes== "1" ~ "Diabetic", diabetes == "0" ~ "Non-Diabetic")) %>% mutate(across(c("sex","drnk_alcohol","smokes","diabetes","migration","cohort2"), as.factor))

#effect data
#effect.plot.model.data = imputed.dataset.1.filtered.a  %>% select(record_id, age,sex, Years.of.education, smokes, diabetes, drnk_alcohol,hiv, Clinic_Type)

#ignore below this
#################################################################
########### create data set for ancestry analysis ##############
################################################################ 
library(stringr)
#raw_ancestry <-read.csv("/Users/oshi/Desktop/TB/clinicxancestry.csv", header=T, na.strings=c(""," ","NA"))

#ancestry = raw_ancestry %>% 
#  select(Sample_ID:European)%>%
#  mutate_at("Sample_ID" , str_replace, "NC", "")%>%
#  mutate(across(Sample_ID, as.numeric)) %>%
#  rename(record_id=Sample_ID)

#merge ancestry and epi csv files
#raw_merge<- merge(ancestry,imputed.dataset.1.filtered.a, by ="record_id") #use for corrplot

#crosstable(raw_merge, KhoeSan,TB_diagnosis,smokes, Clinic_Type,Years.of.education, by = sex, funs =c(median, mean, sd), total = "both")



#ignore below this
#use this for model
#ancestry.data4model = raw_merge %>% mutate(sex= case_when(sex == "1" ~ "Male", sex == "2" ~ "Female"))  %>% mutate(smokes = case_when(smokes== "1" ~ "Smoker", smokes == "0" ~ "Non-Smoker")) %>% mutate(drnk_alcohol = case_when(drnk_alcohol== "1" ~ "Drinker", drnk_alcohol == "0" ~ "Non-Drinker")) %>% mutate(diabetes = case_when(diabetes== "1" ~ "Diabetic", diabetes == "0" ~ "Non-Diabetic")) %>% mutate(across(c("sex","drnk_alcohol","smokes","diabetes"), as.factor))
#imputed_matrix_Sep_29_2021 <- mice(missingness_matrix, m=2, method= "cart", maxit= 10, printFlag = TRUE)
#saveRDS(object=imputed_matrix_Sep_29_2021, file="imputed_matrix_Sep_29_2021") 
#saved_imputed_matrix <- readRDS("imputed_matrix_Sep_29_2021")
#imputed_matrix_Feb_13_2022 <- mice(missingness_matrix, m=2, method= "cart", maxit= 10, printFlag = TRUE)
#saveRDS(object=imputed_matrix_Feb_13_2022, file="imputed_matrix_Feb_13_2022") 
#saved_imputed_matrix2 <- readRDS("imputed_matrix_Feb_13_2022")
#plot parameters against the number of iteration to check for convergence (there should be no obvious visible trends)



write_csv(imputed.dataset.1,"/Users/oshi/Desktop/TB/cleaned_imputed_dataset_amrita.csv")

