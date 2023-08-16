


#upload NCTB  (Last downloaded aug 14  2023)
raw_NCTB_TB_test_vars <- read.csv(
  "/Users/oshi/Downloads/NorthernCapeTBCaseCo-TBtestvariables_DATA_2023-08-15_0839.csv",
  sep = ";",
  header = TRUE,
  na.strings = c("", " ", "NA", "N/A"),
) %>%
  mutate(treatment_srt_date = as.Date(treatment_srt_date),
         dateofcollection = as.Date(dateofcollection))

#previous #upload NCTB  (Last downloaded aug3  2023) 
# "/Users/oshi/Downloads/nctb_tb_test_vars_raw_labs.csv",
# 
###################################################
####### STUDY DATA DECISION TREE ##################
################################################### 

#This is a 3 stage tree : a,b,c

#extract relevant columns and filter age for study data
raw_study_data <- raw_NCTB_TB_test_vars %>% 
  filter(record_id >= 426) %>%
  filter(age >= 18)
#convert the table to a tibble
raw_study_data = as_tibble(raw_study_data)


#PRELIMINARY DATA WRANGLING  FOR DECISION TREE A#
#################################################

#I: RECODING TB NON SMEAR TO CASE AND CONTROLS FOR LATER USE##
nonsmear_raw = raw_study_data %>%
  select(record_id, tb_nonsmear_test_result) 

nonsmear_table = raw_study_data %>%
  select(record_id, tb_nonsmear_test_result) %>%
  distinct(tb_nonsmear_test_result)


# write_csv(nonsmear_table,"/Users/oshi/Desktop/TB/non_smear_raw2.csv") # remove before upload

#upload nonsmear key, change this path to your own file path
nonsmear_key = read.csv("/Users/oshi/Desktop/TB/non_smear_key.csv", header=T) 

#use left join function to merge the nonsmear raw with the key
#now just select the record.id and the recoded variable
nonsmear_recoded = left_join(nonsmear_raw, nonsmear_key, by = "tb_nonsmear_test_result")%>% select(record_id, case.control.unknown)

# II: CREATE TREAT_OR_DRUG_VARIABLE
#THIS AGRREGATES COLUMNS 98,99,100,102 FROM CODEBOOK
# mutate creates a new variable called "treat or drug"
#it embeds a case when statement where for columns  98,99,100,102 , if any are not blank the variable is assigned as a yes, else it's a no

treatment_or_drug = raw_study_data %>%
  mutate(treat_or_drug = case_when(
    treatment_srt_date != "NA" | 
      treat_reg_selec != "NA" | 
      cmpltd_trtmnt != "NA" | 
      drug_taken___1 == 1 |
      drug_taken___2 == 1 |
      drug_taken___3 == 1 |
      drug_taken___4 == 1 |
      drug_taken___5 == 1 |
      drug_taken___6 == 1 |
      drug_taken___7 == 1 |
      drug_taken___8 == 1 |
      drug_taken___9 == 1 |
      drug_taken___10 == 1 |
      drug_taken___11 == 1 |
      drug_taken___12 == 1 |
      drug_taken___13 == 1 |
      drug_taken___14 == 1 |
      drug_taken___15 == 1 ~ 'yes',
    TRUE ~ 'no'
  )) 

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

study_validator_A = function(tb_test_rest, tb_nonsmear, treat_or_drug) {
  case_when(
    tb_test_rest == 1 | (tb_test_rest != 1) & (tb_nonsmear == "case") | 
      (tb_test_rest != 1) & (tb_nonsmear != "case") & (treat_or_drug == "yes") ~ 'case',
      (tb_test_rest == 3) & (tb_nonsmear == "unknown") & (treat_or_drug != "yes") | 
      (tb_test_rest == 4) & (tb_nonsmear == "unknown") & (treat_or_drug != "yes") | 
      (tb_test_rest == "unknown") & (tb_nonsmear == "unknown") & (treat_or_drug != "yes") ~ 'unknown',
    TRUE ~ 'control'
  )
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
#change NA's to unknown
#pipe to replace NAs
past_tb_table = raw_study_data %>%
  select(c(record_id,prior_tb_self_reported, prior_tb_n_self_reported, frst_pst_tb_test_date, )) %>% mutate(prior_tb_self_reported = as.character(prior_tb_self_reported)) %>% mutate(prior_tb_n_self_reported = as.character(prior_tb_n_self_reported)) %>% replace_na(list(prior_tb_self_reported = "unknown", prior_tb_n_self_reported = "unknown", frst_pst_tb_test_date = "unknown"))


##################################
###CREATE DECISION TREE TABLE B###
##################################

#see sketch in dropbox for explanation

study_validator_B = function(prior_tb_self_reported, prior_tb_n_self_reported, frst_pst_tb_test_date) {
  case_when(
    prior_tb_self_reported == 1 | 
      (prior_tb_self_reported != 1) & (prior_tb_n_self_reported != "unknown") | 
      (prior_tb_self_reported != 1) & (prior_tb_n_self_reported == "unknown") & 
      (frst_pst_tb_test_date != "unknown") ~ 'case',
      (prior_tb_self_reported == "unknown") & 
      (prior_tb_n_self_reported == "unknown") & 
      (frst_pst_tb_test_date == "unknown") ~ "unknown",
    TRUE ~ "control"
  )
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
  mutate(TB_diagnosis = case_when(
    true_diag == "case" | true_diag_b == "case" ~ 'case',
    true_diag == "unknown" & true_diag_b == "unknown" ~ 'unknown',
    TRUE ~ 'control'
  )) %>%
  select(c(record_id, TB_diagnosis))


study_data_valid_tb = merge(validated_study_for_merge,raw_study_data,by="record_id")%>% select(c(sample_id, TB_diagnosis))

#Add an addendum to this. so afyer study.data.valid.tb we have assigned case control status.
# now first create another function that will only work on cases, and this will label them as fresh_cases or prev_case

#prior case: if first_prior ==1, frst_pst_tb_test_date !=NA , cmpltd_trtmnt ==1(Yes)
#prior_tb_self_reported == 1


  




case_validator <- function(first_prior, frst_pst_tb_test_date, cmpltd_trtmnt, prior_tb_self_reported, treatment_srt_date, dateofcollection) {
  case_when(
    first_prior == 1 | !is.na(frst_pst_tb_test_date) | cmpltd_trtmnt == 1 |
      prior_tb_self_reported == 1 ~ 'previous_case',
    prior_tb_self_reported == 0 & cmpltd_trtmnt == 2 &
      !is.na(treatment_srt_date) & !is.na(dateofcollection) &
      abs(difftime(treatment_srt_date, dateofcollection, units = "days")) >= 15 ~ 'old_case',
    TRUE ~ "fresh_case"
  )
}

#prior_tb_self_reported == 0 & cmpltd_trtmnt is ongoing (2) & abs(trtmnt_srt_date, dateofcollection, units = "days")) <= 14) ~ old case
#prior_tb_self_reported no AND cmpltd_trtmnt ==2

yyy <- study_data_valid_tb %>%
  mutate(TB_status = case_when(
    TB_diagnosis == "case" ~ case_validator(first_prior, frst_pst_tb_test_date, cmpltd_trtmnt, prior_tb_self_reported,treatment_srt_date, dateofcollection),
    TRUE ~ TB_diagnosis
  )) %>%
  select(first_prior, frst_pst_tb_test_date, cmpltd_trtmnt, prior_tb_self_reported, TB_status, everything()) %>%
  relocate(TB_status, .after = TB_diagnosis)





decision_tree_results <- yyy %>% select(c(sample_id, TB_status))


