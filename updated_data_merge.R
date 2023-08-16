#combine both studies
#manually install these packages if you don't have 'em'
library(stringr)
library(tidyverse)
library(stringdist)
library(fuzzyjoin)
if (!requireNamespace("writexl", quietly = TRUE)) {
  install.packages("writexl")
}
library(writexl)

#make a vector of colum names to rename NCTB columns downstream
NCTB_new_name <- c(NCTB_Age = "Age.", NCTB_Saliva = "Saliva.Kit.Barcode.Number", NCTB_Gender = "Gender", NCTB_Recruitment_date = "Date.of.Recruitment")

#upload NCTB  #  (Last downloaded aug 14         (Last downloaded aug3  2023)
raw_NCTB_data <- read.csv(
  "/Users/oshi/Downloads/NorthernCapeTBCaseCo-Thinned_DATA_LABELS_2023-08-14_2223.csv",
  sep = ";",
  header = TRUE,
  na.strings = c("", " ", "NA", "N/A"),
  colClasses = c("Saliva.Kit.Barcode.Number" = "character")
) %>%
  unite("NCTB.Name", Surname, First.Name, sep = " ", remove = FALSE, na.rm = TRUE) %>% rename(all_of(NCTB_new_name)) 


NCR_new_name <- c(Surname = "Participant.Surname", First.Name = "Participant.first.name", NCR_Age = "Age.of.participant", NCR_Gender = "Sex..gender..of.participant.", NCR_Phleb_date ="Date.and.Time.of.Phlebotomy", NCR_Saliva = "Participant.study.saliva.ID", current_other_inf ="Do.you.currently.have.an.infection.such.as.a.cold.or.the.flu.....not.TB." )

#upload nctb downloaded aug 3 2023
#/Users/oshi/Downloads/InvestigationOfAnces-ThinReport_DATA_LABELS_2023-08-03_2310.csv
raw_NCR_data <- read.csv(
  "/Users/oshi/Downloads/InvestigationOfAnces-ThinReport_DATA_LABELS_2023-08-14_2211.csv",
  header = TRUE,
  sep = ";",
  na.strings = c("", " ", "NA", "N/A"),
  colClasses = c("Participant.study.saliva.ID" = "character")
) %>%
  rename(all_of(NCR_new_name)) %>%
  unite("NCR.Name", Surname, First.Name, sep = " ", remove = FALSE, na.rm = TRUE) 

# create vector of TB cases verified from  medical records from  Sr. Andres
cases <- c("NCR035", "NCR044", "NCR047", "NCR051", "NCR052","NCR058","NCR059", "NCR065", "NCR067", "NCR069", "NCR071", "NCR072", "NCR077", "NCR078", "NCR079", "NCR080", "NCR081", "NCR088", "NCR089", "NCR090", "NCR091", "NCR099", "NCR100", "NCR101", "NCR102", "NCR103", "NCR104", "NCR105", "NCR106", "NCR107", "NCR108", "NCR109", "NCR110", "NCR111", "NCR112", "NCR113", "NCR114", "NCR115","NCR116","NCR117","NCR118","NCR119","NCR120","NCR121","NCR122","NCR123","NCR124","NCR125")

#
## assign case control status in NCR based on case vector above
raw_NCR_data2 <- raw_NCR_data %>%
  mutate(pre_status = ifelse(Participant.study.ID %in% cases, "case", "control"))

#Thin out NCR dataset to contain relevant columns, remove NCR093 ecause of missingness issues
raw_NCR_data3 <- raw_NCR_data2 %>%  select(-contains(c("average","Have","Maternal","ther","circum","pigment","you", "Complete","eight","Researcher","anguage","child", "Phone","Participant.s.S")))%>% select(contains(c("Participant","Full.Name", "Age","Sex","cell","Date", "NCR","pre_status", "IGRA")))%>%  filter(!(Participant.study.ID == "NCR093"))

########################################################################
################ Merge NCR and NCTB using Saliva Barcode ###############
########################################################################

# Filter out rows with NA values in Saliva Barcode Column in the matching columns of NCTB dataset
#manually add match codes for people we manually found that match from records
filtered_NCTB_data <- raw_NCTB_data %>%
  filter(!is.na(NCTB_Saliva)) 

# Filter out rows with NA values in Saliva Barcode Column in the matching columns of NCR dataset
#manually add match codes for people we manually found that match from records
filtered_NCR_data3 <- raw_NCR_data3 %>%
  filter(!is.na(NCR_Saliva)) 

# Perform the stringdist_inner_join on the filtered data frames
# I am using EXACT matches for this criteria
# ALSO use relocate function so that its easy to compare  matching columns from both studies as a sanity check 
merged_study_saliva <- stringdist_inner_join(filtered_NCTB_data, filtered_NCR_data3, by = c("NCTB_Saliva" = "NCR_Saliva"), max_dist = 0, distance_col = NULL) %>%
  relocate("Participant.study.ID", .after = Sample.ID) %>%
  relocate("NCR_Age", .after = NCTB_Age)%>%
  relocate("NCR_Gender", .after = NCTB_Gender)%>%
  relocate("NCR_Saliva", .after = NCTB_Saliva)%>%
  relocate("NCR.Name", .after = NCTB.Name)%>%
  relocate("Date.of.recruitment", .after = NCTB_Recruitment_date)

view(merged_study_saliva)

########################################################################
################ Merge NCR and NCTB using manualmatch Names Column #################
########################################################################
manual_match_NCTB <- raw_NCTB_data %>%
  filter(!is.na(NCTB_Saliva)) %>%
  mutate(manual_match = ifelse(Sample.ID == "NC1359", "JH_MANUAL", NA)) %>%
  filter(!is.na(manual_match))

manual_match_NCR <- raw_NCR_data3 %>%
  filter(!is.na(NCR_Saliva)) %>%
  mutate(manual_match = ifelse(Participant.study.ID == "NCR023", "JH_MANUAL", NA)) %>%
  filter(!is.na(manual_match))

merged_study_manual_match <- stringdist_inner_join(manual_nctb_match, manual_match_NCR, by = "manual_match", max_dist = 0, distance_col = NULL) %>%
  relocate("Participant.study.ID", .after = Sample.ID) %>%
  relocate("NCR_Age", .after = NCTB_Age)%>%
  relocate("NCR_Gender", .after = NCTB_Gender)%>%
  relocate("NCR_Saliva", .after = NCTB_Saliva)%>%
  relocate("NCR.Name", .after = NCTB.Name)%>%
  relocate("Date.of.recruitment", .after = NCTB_Recruitment_date)
#%>%
 # relocate("manual_match", .after = Participant.study.ID)

########################################################################
################ Merge NCR and NCTB using Names Column #################
########################################################################

#Use Fuzzy joining method because of human error in name capturing, I allow
#name characters between studies to differ by two characters (max_dist = 2)
merged_study_name  <- stringdist_inner_join(raw_NCTB_data, raw_NCR_data3, by = c("NCTB.Name" = "NCR.Name"),max_dist = 2, distance_col = NULL) %>%
  mutate(Age_Difference = abs(NCTB_Age - NCR_Age)) %>%
  relocate("Participant.study.ID", .after = Sample.ID) %>%
  relocate("NCR_Age", .after = NCTB_Age)%>%
  relocate("NCR_Gender", .after = NCTB_Gender)%>%
  relocate("NCR_Saliva", .after = NCTB_Saliva)%>%
  relocate("NCR.Name", .after = NCTB.Name)%>%
  relocate("Date.of.recruitment", .after = NCTB_Recruitment_date)


merged_study_name <- stringdist_inner_join(
  raw_NCTB_data, 
  raw_NCR_data3, 
  by = c("NCTB.Name" = "NCR.Name"),
  max_dist = 2, 
  distance_col = NULL
) %>%
  mutate(Age_Difference = abs(NCTB_Age - NCR_Age)) %>%
  filter(Age_Difference < 5) %>%
  relocate("Participant.study.ID", .after = Sample.ID) %>%
  relocate("NCR_Age", .after = NCTB_Age) %>%
  relocate("NCR_Gender", .after = NCTB_Gender) %>%
  relocate("NCR_Saliva", .after = NCTB_Saliva) %>%
  relocate("NCR.Name", .after = NCTB.Name) %>%
  relocate("Date.of.recruitment", .after = NCTB_Recruitment_date) %>%
  select(-Age_Difference)



view(merged_study_name)

# Reorder columns in merged_study_saliva to match merged_study_name
merged_study_saliva <- merged_study_saliva %>%
  select(names(merged_study_name))

merged_study_manual_match <- merged_study_manual_match %>%
  select(names(merged_study_name))
########################################################################
################ Now combine Name Merge and Saliva Merge datasets######
########################################################################
# Combine both data frames
combined_merged_study <- bind_rows(merged_study_saliva, merged_study_name,merged_study_manual_match )

# Remove duplicate rows
combined_merged_study <- distinct(combined_merged_study, .keep_all = TRUE)

# miscellaneous cleaning and and rearranging and renaming of columns
combined_merged_study <- combined_merged_study %>% rename(Drnk_Alcohol = "Do.you.drink.Alcohol.", Smoker = "Do.you.smoke.") %>% select(-c(First.Name,Surname,Final.IGRA.Result,Treatment.Regimen.Selection,Rifampicin.result))

###########################################################################
# Now get a list of participant IDs who have are in NCR dataset bit are missing
# NCTB data (i.e demographic data )###########################################

raw_NCR1_ids <- raw_NCR_data$Participant.study.ID

combined_merged_study_ids <- combined_merged_study$Participant.study.ID

# Get the unique values in raw_NCR1 that are not in combined_merged_study
#these are the IDs with no name match or saliva match to NCTB, presumably because
# they have not been shipped from Upington to Capetown
values_not_in_combined <- setdiff(raw_NCR1_ids, combined_merged_study_ids)


# Now Get the values from "values_not_in_combined" that are not in "cases". this # is a subset of above bot only cases
#values_not_in_cases <- setdiff(values_not_in_combined, cases)
# Print the resulting vector
#print(values_not_in_cases)

#convert the vector above to an NCR dataframe
unmatched_NCRs <- raw_NCR_data2 %>%
  filter(Participant.study.ID %in% values_not_in_combined)

# Print the unmatched_data dataframe
print(unmatched_NCRs)

#Now combine this individuals with missing demo data to main merged dataset
Prelim_Merged_Dataset <- bind_rows(combined_merged_study, unmatched_NCRs)


#we will now merge the EPI case-control status to the prelim_merged_dataset

# source("/Users/oshi/Desktop/TB/update _decision tree.R")
#first rename 

Prelim_Merged_Dataset_beta <- left_join(Prelim_Merged_Dataset,semi_validated_tb, 
                 by = c("Sample.ID"="sample_id"  )) %>%
  select(Participant.study.ID, Sample.ID, TB_diagnosis, pre_status, TB.test.result, everything()) %>%
  mutate(Date.of.recruitment = as.Date(Date.of.recruitment))

# now first create another function that will only work on cases, and this will label them as fresh_cases or prev_case

#prior case: if first_prior ==1, frst_pst_tb_test_date !=NA , cmpltd_trtmnt ==1(Yes)
#prior_tb_self_reported == 1

#frst_pst_tb_test_date - First prior TB episode test date
#first_prior - Was there a first prior TB episode? 1(yes), 0 (no)
#cmpltd_trtmnt - Completed treatment, 0-No, 1-Yes, 2-Ongoing, 3-Cured
#prior_tb_self_reported- Have you had TB in the past? (Self-reported), 0-No, 1-Yes, 2-Unsure
#treatment_srt_dat - Treatment start date

case_validator <- function(first_prior, frst_pst_tb_test_date, cmpltd_trtmnt, prior_tb_self_reported, treatment_srt_date, Date.of.recruitment) {
  case_when(
    first_prior == 1 | !is.na(frst_pst_tb_test_date) | cmpltd_trtmnt == 1 |
      prior_tb_self_reported == 1 ~ 'previous_case',
    prior_tb_self_reported == 0 & cmpltd_trtmnt == 2 &
      !is.na(treatment_srt_date) & !is.na(Date.of.recruitment) &
      abs(difftime(treatment_srt_date, Date.of.recruitment, units = "days")) >= 15 ~ 'old_case',
    TRUE ~ "fresh_case"
  )
}

validated_cases <- Prelim_Merged_Dataset_beta %>%
  mutate(TB_status = case_when(
    TB_diagnosis == "case" ~ case_validator(first_prior, frst_pst_tb_test_date, cmpltd_trtmnt, prior_tb_self_reported,treatment_srt_date, Date.of.recruitment),
    TRUE ~ TB_diagnosis
  )) %>%
  select(first_prior, frst_pst_tb_test_date, cmpltd_trtmnt, prior_tb_self_reported, TB_status,tb_test_date, treatment_srt_date, Date.of.recruitment, everything()) %>%
  relocate(TB_status, .after = TB_diagnosis)





#This is the final step in the case-control assignment, we are now considering cold and flu

pre_final_cases <- validated_cases %>%
  mutate(
    Final_TB_Status = case_when(
      TB_status == "fresh_case" & current_other_inf == "No" ~ 
        "fresh_case",
      
      TB_status == "fresh_case" & current_other_inf == "Yes" ~
        "fresh_case(flu)",
      
      TB_status == "fresh_case" & is.na(current_other_inf) ~ "fresh_case(unknown flu)",
      
      TB_status == "previous_case" & current_other_inf == "No" ~
        "previous_case",
      
      TB_status == "previous_case" & current_other_inf == "Yes" ~
        "previous_case(flu)",
      
      TB_status == "previous_case" & is.na(current_other_inf) ~ "previous_case(unknown flu)",
      
      TB_status == "old_case" & current_other_inf == "No" ~
        "old_case",
      
      TB_status == "old_case" & current_other_inf == "Yes" ~
        "old_case(flu)",
      
      TB_status == "old_case" & is.na(current_other_inf) ~ "old_case(unknown flu)",
      
      TB_status == "control" & current_other_inf == "No" ~
        "Strong Control",
      
      TB_status == "control" & current_other_inf == "Yes" ~
        "Weak Control",
      
      TB_status == "control" & is.na(current_other_inf) ~
        "Control (Unknown flu)"
    )
  ) %>%
  select(Participant.study.ID, Sample.ID, Final_TB_Status, TB_status, pre_status, everything())
      
  



#now remove duplicates

#list of record IDs from Brenna's email that are duplicates 
IDs_to_remove <- c(795,1446,383,569,678,917,1230)

#list of NCR IDs from Brenna's email TO REMOVE
NCR_to_remove <- c("NCR017")

 
#from the dataframe Prelim_Merged_Dataset_beta2 , remove all rows where the Record.ID column matches one of the values in IDs_to_remove

final_assignments <- pre_final_cases %>% 
  filter(!Record.ID %in% IDs_to_remove) %>% 
  filter(!Participant.study.ID %in% NCR_to_remove) %>% distinct(.keep_all = TRUE) 

final_tb_status_table <- final_assignments %>% 
  count(Final_TB_Status)


case_to_previous_case <- final_assignments %>%
  filter(pre_status == "case" & TB_status == "previous_case")

previous_case <- final_assignments %>%
  filter(TB_status == "previous_case")

fresh_case <- final_assignments %>%
  filter(TB_status == "fresh_case")

old_case <- final_assignments %>%
  filter(TB_status == "old_case")

/Users/oshi/Library/CloudStorage/Dropbox/NCR Study/Case_decision_tree


# Then save these to excel files
write_xlsx(Prelim_Merged_Dataset_beta2, path = "/Users/oshi/Desktop/TB/Updated_DATASET_15Aug.xlsx")
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
                            #STOP HERE#
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##


#Find values that appear more than once in the NCR.Name column
#duplicates <- Prelim_Merged_Dataset_beta2 %>%
#  group_by(NCR.Name) %>%
#  filter(n() > 1)
# Print the filtered_data dataframe
#view(duplicates)
#write to xlsx file
#write_xlsx(duplicates,"/Users/oshi/Desktop/TB/duplicates.xlsx")


# Columns to check for misisngess
columns_of_interest <- c("NCTB_Gender", "NCR_Gender", "Clinic.Sampling.Location",
                         "Drnk_Alcohol", "Smoker", "Highest.Qualification",
                         "NCTB_Age", "NCR_Age", "NCTB_Recruitment_date",
                         "Date.of.recruitment", "NCTB_Saliva", "NCR_Saliva",
                         "Cell.Viability", "Live.Cell.Count", "NCR_Phleb_date",
                         "Date.of.IGRA.blood.draw", "Final.IGRA.results")

# Create a dataframe to record missingness and Participant.study.ID values
missingness_data <- data.frame(Column = character(),
                               Missingness = integer(),
                               Participant.ID_Values = character(),
                               stringsAsFactors = FALSE)

# Loop through each column
for (col in columns_of_interest) {
  missing_rows <- Prelim_Merged_Dataset_beta2 %>%
    filter(is.na(!!sym(col))) %>%
    pull(Participant.study.ID) %>%
    unique() %>%
    toString()
  
  missing_count <- sum(is.na(combined_merged_study[[col]]))
  
  missingness_data <- bind_rows(missingness_data, data.frame(Column = col,
                                                             Missingness = missing_count,
                                                             Participant.ID_Values = missing_rows))
}





# Print the missingness_data dataframe
view(missingness_data)

write_xlsx(missingness_data, "/Users/oshi/Desktop/TB/missingness_data.xlsx")
#beta mode ignore below
# Split and convert the Participant.ID_Values to a list of vectors
id_values_list <- strsplit(missingness_data$Participant.ID_Values, ", ")

# Get the unique Participant.study.ID values across all columns
unique_ids <- unique(unlist(id_values_list))

# Print the list of unique Participant.study.ID values
print(unique_ids)




####    #### #### ####    #### 
####  SANITY CHECKS; IGNORE BELOW #####

# Group by "Participant.study.ID" and count the occurrences
id_counts <- as.data.frame( xxx %>%
                              group_by(Participant.study.ID) %>%
                              tally(name = "Count") %>%
                              filter(Count > 1))

# Display the counts of repeated characters
print(id_counts)



#




