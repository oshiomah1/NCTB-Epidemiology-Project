#decision trees

# Load important libraries 
library(dplyr)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(effects)

##PART 1a##
#Upload data
raw_data <- read.csv("/Users/oshi/Desktop/TB/NorthernCapeTBCaseCo-AllTheData_DATA_2022-01-19.csv", header=T, na.strings=c(""," ","NA","N/A"))  %>% select(-consent_taken)

#1 - 425 is pilot data
raw_pilot_data = raw_data %>%  slice((1:425)) %>% filter(age >= 18)

################################################################################
############################ PILOT DECISION TREE ###############################
##############################################################################

#extract columns and rows for pilot data decision tree
pilot_diagnosis_data <- raw_pilot_data %>% select(record_id, tb_self_report,prior_tb_self_reported)  

#convert the table to a tibble
pilot_diagnosis_data = as_tibble(pilot_diagnosis_data)

#recode blank cells/NA to Unknown, 2 = Unknown according to codebook
pilot_diagnosis_data = pilot_diagnosis_data %>% replace_na(list(tb_self_report = 2,prior_tb_self_reported = 2))

#Now time for the function. 
pilot_validator <- function(tb_self_report, prior_tb_self_reported) {
  case_when(tb_self_report == 1 ~ 'case',(tb_self_report != 1) & (prior_tb_self_reported == 1) ~ 'case',(tb_self_report == 0) & (prior_tb_self_reported != 1) ~ 'control',TRUE ~ 'unknown')
}

#APPLY function on data frame
true_pilot_tb_result = pilot_diagnosis_data %>% 
  mutate(TB_diagnosis = pilot_validator(tb_self_report, prior_tb_self_reported))

#subset relevant columns
validated_pilot_for_merge = true_pilot_tb_result  %>% select(record_id,TB_diagnosis)

#merge data FROM DECISiOn TREE with covariates
pilot_data_valid_tb = merge(validated_pilot_for_merge,raw_pilot_data,by="record_id") # merge thisw  with #study_data_valid_tb



          ###################################################
          ####### STUDY DATA DECISION TREE ##################
          ################################################### 

#This is a 3 stage tree : a,b,c

#extract relevant columns and filter age for study data
raw_study_data = raw_data %>%  slice((426:1165)) %>% filter(age >= 18)

#convert the table to a tibble
raw_study_data = as_tibble(raw_study_data)


#PRELIMINARY DATA WRANGLING  FOR DECISION TREE A#
#################################################

#I: RECODING TB NON SMEAR TO CASE AND CONTROLS FOR LATER USE##
nonsmear_table = raw_study_data %>% select(record_id, tb_nonsmear_test_result)

# write_csv(nonsmear_table,"/Users/oshi/Desktop/TB/xc.csv") # remove before upload

#upload nonsmear key, change this path to your own file path
nonsmear_key = read.csv("/Users/oshi/Desktop/TB/nonsmearkey.csv", header=T) %>%
 slice(1:36)

#use left join function to merge the nonsmear table with the key
#now just select the record.id and the recoded variable
nonsmear_recoded = left_join(nonsmear_table, nonsmear_key, by = "tb_nonsmear_test_result")%>% select(record_id, case.control.unknown)

# II: CREATE TREAT_OR_DRUG_VARIABLE
#THIS AGRREGATES COLUMNS 98,99,100,102 FROM CODEBOOK
# mutate creates a new variable called "treat or drug"
#it embeds a case when statement where for columns  98,99,100,102 , if any are not blank the variable is assigned as a yes, else it's a no
treatment_or_drug = raw_study_data %>%
  mutate(treat_or_drug = case_when(treatment_srt_date != "NA" | treat_reg_selec != "NA" | drug_taken != "NA" | cmpltd_trtmnt != "NA"   ~ 'yes', TRUE ~ 'no')) 


#select the newly created  treat or drug variable
treatment_or_drug2 = treatment_or_drug %>%
  select(c(record_id, treat_or_drug))

#select the tb test rest
study_subset = raw_study_data %>%
  select(c(record_id, tb_test_rest))

#merge treat or drug variable and nonsmear recoded
table_0 = left_join(study_subset,nonsmear_recoded, by ="record_id")

#merge treat or drug variable with table_0
#rename the case.control.unknown column to tb_nonsmear  in table_01
#rEPLACE NAs with "unknown"
table_A = left_join(table_0,treatment_or_drug2,by ="record_id")%>%
  rename( tb_nonsmear = case.control.unknown)  %>% mutate(tb_test_rest = as.character(tb_test_rest))%>%
  replace_na(list(tb_test_rest = "unknown", treat_or_drug ="unknown" , tb_nonsmear="unknown"))

# This is now the dataframe for decision tree A it has index, test result, non-smear, and treat or drug in one dataframe


##################################
###CREATE DECISION TREE TABLE A###
##################################

# this is created using criteria outlined by the decision table A sketch in dropbox

study_validator_A = function(tb_test_rest, tb_nonsmear, treat_or_drug){
  case_when(tb_test_rest ==1 | (tb_test_rest !=1)&(tb_nonsmear== "case") |(tb_test_rest !=1)&(tb_nonsmear!= "case")&(treat_or_drug == "yes") ~ 'case', (tb_test_rest == 3)&(tb_nonsmear== "unknown")&(treat_or_drug != "yes")|(tb_test_rest == 4)&(tb_nonsmear== "unknown")&(treat_or_drug != "yes")|(tb_test_rest == "unknown")&(tb_nonsmear== "unknown")&(treat_or_drug != "yes")~ 'unknown', TRUE ~ 'control' )
}

##RUN THIS study_validator_A_ FUNCTION on the  dataset

#we are creating a new dataframe called validtable
# the table.A data set is piped(%>%) to a mutate function that creates a new variable called "true diag"
#"true diag" equals to study_validator_A function working on using our first 3 variables; tb_test_rest, tb_nonsmear, treat_or_drug. It does this for every row


#and then  we pipe to select just record id and true_diag
table_A_validated = table_A %>%
  mutate(true_diag = study_validator_A(tb_test_rest, tb_nonsmear, treat_or_drug)) %>% select(record_id,true_diag)

######################################################
###PRELIMINARY DATA WRANGLING  FOR DECISION TREE B###
######################################################

#SUBSET post pilot data to 1,83,84,104 from code book
#change NAs to unknown
#pipe to replace NAs
past_tb_table = raw_study_data %>%
  select(c(record_id,prior_tb_self_reported, prior_tb_n_self_reported, frst_pst_tb_test_date)) %>% mutate(prior_tb_self_reported = as.character(prior_tb_self_reported)) %>% mutate(prior_tb_n_self_reported = as.character(prior_tb_n_self_reported)) %>% replace_na(list(prior_tb_self_reported = "unknown", prior_tb_n_self_reported = "unknown", frst_pst_tb_test_date = "unknown"))


##################################
###CREATE DECISION TREE TABLE B###
##################################

#see sketch in dropbox for explanation

study_validator_B = function(prior_tb_self_reported, prior_tb_n_self_reported, frst_pst_tb_test_date){
  case_when(prior_tb_self_reported == 1 | (prior_tb_self_reported != 1)&(prior_tb_n_self_reported != "unknown") | (prior_tb_self_reported != 1)&(prior_tb_n_self_reported == "unknown")&(frst_pst_tb_test_date != "unknown")~ 'case', (prior_tb_self_reported == "unknown")&(prior_tb_n_self_reported == "unknown")&(frst_pst_tb_test_date == "unknown") ~ "unknown", TRUE ~ "control")
}

# run this function on the dataset
#select relevant variables
table_B_validated = past_tb_table %>%
  mutate(true_diag_b = study_validator_B(prior_tb_self_reported, prior_tb_n_self_reported, frst_pst_tb_test_date)) %>% select(record_id,true_diag_b)

#create new dataframe that merges both true diag a and b
merged_diagnoses = left_join(table_A_validated, table_B_validated , by = "record_id") 

##################################
###CREATE DECISION TREE TABLE C###
##################################
# see decision tree c sketch 

validated_study_for_merge = merged_diagnoses %>%
  mutate(TB_diagnosis = case_when(true_diag == "case"|true_diag_b == "case" ~ 'case',true_diag == "unknown" & true_diag_b == "unknown" ~ 'unknown', TRUE ~ 'control'))  %>% select(c(record_id,TB_diagnosis))

study_data_valid_tb = merge(validated_study_for_merge,raw_study_data,by="record_id")

#Merge both pilot and study data sets now that tb status is validated

joined.data.valid.tb = bind_rows(pilot_data_valid_tb, study_data_valid_tb)





