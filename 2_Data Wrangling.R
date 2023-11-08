source("/Users/oshi/Desktop/TB/1 _decision tree.R")

# Our starting dataframe is the last table from part 1 - joined.data.valid.tb

#RECODE height
#change missing height2  to be equals height 1, if height 2 is not missing it remains the same, then pipe this to find average height

height_recoded = joined.data.valid.tb %>% mutate(height_2 = case_when(is.na(height_2) ~ height_1, !is.na(height_2) ~ height_2 )) %>% mutate(height = (height_1+height_2)/2) %>% relocate(height, .after = height_2)

##########################################################
####RECODE sex############ 
#original data: 1- male, 2 - female, switch to 1-male, 0-female
########################################################## 
sex.recoded = height_recoded %>% mutate(Sex = case_when(sex == 1 ~ 1,sex == 2 ~ 0 ,TRUE ~ NA_real_))  %>% relocate(Sex, .after =sex)

##########################################################
####RECODE reside column using Census criteria############ 
########################################################## 
#1 = Rural 2 = Small Town 3 = Large Town
#Rural
#1 = Groot Mier - 13, Riemvasmaak - 12, Rietfontein - 10, Askham - 9, Grootdrink - 8
#Small town
#2 = Kakamas -7, Keimoes CHC - 6, Keimoes PHC - 5
#Large town
#3 = Harry Surtie - 1, Progress - 2, Sarah Stauss  -3, Dorp  -11

clinic.recoded = height_recoded %>% mutate(Clinic_Type = case_when(loc_sampling_2 == 13  ~  "Rural" ,loc_sampling_2 == 12  ~  "Rural", loc_sampling_2 == 10  ~  "Rural",loc_sampling_2 == 9  ~  "Rural",loc_sampling_2 == 8  ~  "Rural",loc_sampling_2 == 7  ~  "Small town",loc_sampling_2 == 6  ~  "Small town",loc_sampling_2 == 5  ~  "Small town",loc_sampling_2 == 1  ~  "Large town",loc_sampling_2 == 2  ~  "Large town", loc_sampling_2 == 3  ~  "Large town", loc_sampling_2 == 11  ~  "Large town", TRUE ~ "other")) %>% relocate(Clinic_Type, .after = loc_sampling_2)


##########################################################
####RECODE education column USING codebook criteria############ 
########################################################## 

education.recoded = clinic.recoded %>% mutate(Years.of.education = case_when(qual == 1 ~ NA_real_, qual == 2 ~ 0, qual == 3 ~ 1, qual == 4 ~2, qual == 5 ~ 3, qual == 6~ 4, qual == 7 ~ 5, qual == 8 ~6, qual == 9 ~ 7, qual == 10 ~ 8, qual == 11 ~ 9, qual == 12 ~ 10, qual == 13 ~ 11, qual == 14 ~ 12, qual == 15 ~14, qual == 16 ~ 16 )) %>% relocate(Years.of.education, .after = qual)

#alternate if we group 0 years of education with primary
#education.recoded %>% mutate(Education2 = case_when(Years.of.education > 12 ~ "Tertiary", Years.of.education == 10 ~ "Secondary",Years.of.education == 11 ~ "Secondary",Years.of.education == 12 ~ "Secondary", TRUE ~ "Primary")) %>% relocate(Education2, .after = Years.of.education)

##########################################################
####RECODE ethnicity USING Justin's criteria############ 
########################################################## 

ethnicity.recoded = education.recoded  %>% 
  mutate(across(c(m_ethnicity,f_ethnicity),
                ~ case_when(. == "1"  ~ "colored",. == "6"  ~ "colored",. == "7"  ~ "colored" ,. == "1,7"  ~ "colored",
                            . == "9"  ~ "Bantu",. == "10"  ~ "Bantu",. == "11"  ~ "Bantu",. == "12"  ~ "Bantu",. == "14"  ~ "Bantu",. == "2"  ~ "KhoeSan",. == "4"  ~ "KhoeSan",. == "5"  ~ "KhoeSan",.== "2,5"  ~ "KhoeSan",.== "1,5"  ~ "KhoeSan",. == "8"  ~ "South Asian",. == "15"  ~ "South Asian",. == "3"  ~ "White",.== "13"  ~ "Other",.== "2,11"  ~ "KS-Bantu",.== "2,14"  ~ "KS-Bantu",.== "5,10"  ~ "KS-Bantu",. == "2,5,14"  ~ "KS-Bantu", . == "1,10"  ~ "KS-Bantu", TRUE ~ .)))

summary(ethnicity.recoded)

############### Now clean up alcohol smoking data ###############

#recode missing smoking and drinking sub categories if main category is zero else leave as it is

#if main category is zero change all sub categories to zero

raw.alc.smoke = ethnicity.recoded %>%  mutate(across(c(drnk_alcohol,smokes,week_alcohol,wkday_beer,wkday_wine,wkday_liquor,wkday_gngrbeer,weekend_alcohol,wkend_beer,wkend_wine,wkend_liquor,wkend_gngrbeer,cig_day,zol_day,pipe_day,herb_day), as.character)) %>%
  mutate(across(c(week_alcohol,wkday_beer,wkday_wine,wkday_liquor,wkday_gngrbeer,weekend_alcohol,wkend_beer,wkend_wine,wkend_liquor,wkend_gngrbeer), ~ case_when(drnk_alcohol == "0" ~ "0" , TRUE  ~ .))) %>%
  mutate(across(c(cig_day,zol_day,pipe_day,herb_day),~case_when(smokes == "0" ~ "0", TRUE  ~ .))) 

#create way to track blank drinking cells, if main is 1 and some sub categories
# are missing, make the others zero
alc.smoke.1 = raw.alc.smoke %>% mutate(across(c(week_alcohol,wkday_beer,wkday_wine,wkday_liquor,wkday_gngrbeer,weekend_alcohol,wkend_beer,wkend_wine,wkend_liquor,wkend_gngrbeer), as.numeric)) %>% 
  mutate(wkday_drink_blank = case_when(is.na(wkday_beer) & is.na(wkday_wine) & is.na(wkday_liquor) & is.na(wkday_gngrbeer) ~ "blank", TRUE ~ ">1")) %>%relocate(wkday_drink_blank, .after = wkday_gngrbeer)%>%
  mutate(wkday_beer = case_when(wkday_drink_blank == '>1' & is.na(wkday_beer)~ 0, TRUE ~ wkday_beer))%>%
  mutate(wkday_wine = case_when(wkday_drink_blank == '>1' & is.na(wkday_wine)~ 0, TRUE ~ wkday_wine))%>%
  mutate(wkday_liquor = case_when(wkday_drink_blank == '>1' & is.na(wkday_liquor)~ 0, TRUE ~ wkday_liquor))%>%
  mutate(wkday_gngrbeer = case_when(wkday_drink_blank == '>1' & is.na(wkday_gngrbeer)~ 0, TRUE ~ wkday_gngrbeer))

alc.smoke.2 = alc.smoke.1 %>% 
  mutate(wkend_drink_blank = case_when(is.na(wkend_beer) & is.na(wkend_wine) & is.na(wkend_liquor) & is.na(wkend_gngrbeer) ~ "blank", TRUE ~ ">1")) %>%relocate(wkend_drink_blank, .after = wkend_gngrbeer)%>%
  mutate(wkend_beer = case_when(wkend_drink_blank == '>1' & is.na(wkend_beer)~ 0, TRUE ~ wkend_beer))%>%
  mutate(wkend_wine = case_when(wkend_drink_blank == '>1' & is.na(wkend_wine)~ 0, TRUE ~ wkend_wine))%>%
  mutate(wkend_liquor = case_when(wkend_drink_blank == '>1' & is.na(wkend_liquor)~ 0, TRUE ~ wkend_liquor))%>%
  mutate(wkend_gngrbeer = case_when(wkend_drink_blank == '>1' & is.na(wkend_gngrbeer)~ 0, TRUE ~ wkend_gngrbeer))

alc.smoke.3 = alc.smoke.2 %>% mutate(across(c(cig_day:herb_day),as.numeric))%>%
  mutate(smoke_blank = case_when(is.na(cig_day) & is.na(zol_day) & is.na(pipe_day) & is.na(herb_day) ~ "blank", TRUE ~ ">1")) %>%relocate(smoke_blank, .after = herb_day)%>%
  mutate(cig_day = case_when(smoke_blank == '>1' & is.na(cig_day)~0, TRUE ~ cig_day)) %>%
  mutate(zol_day = case_when(smoke_blank == '>1' & is.na(zol_day)~0, TRUE ~ zol_day)) %>%
  mutate(pipe_day = case_when(smoke_blank == '>1' & is.na(pipe_day)~0, TRUE ~ pipe_day))%>%
  mutate(herb_day = case_when(smoke_blank == '>1' & is.na(herb_day)~0, TRUE ~ herb_day))

# transform alcohol variables to g per ethanol
alcohol.transformed = alc.smoke.3 %>% 
  mutate(wkday_beer = 0.05 * 1000 * 0.79 * wkday_beer)%>% 
  mutate(wkday_wine = 0.12 * 1000 * 0.79 * wkday_wine)%>% 
  mutate(wkday_liquor = 0.4 * 1000 * 0.79 * wkday_liquor)%>% 
  mutate(wkday_gngrbeer = 0.25 * 1000 * 0.79 * wkday_gngrbeer)%>% 
  mutate(wkend_beer = 0.05 * 1000 * 0.79 * wkend_beer)%>% 
  mutate(wkend_wine = 0.12 * 1000 * 0.79 * wkend_wine)%>% 
  mutate(wkend_liquor = 0.4 * 1000 * 0.79 * wkend_liquor)%>% 
  mutate(wkend_gngrbeer = 0.25 * 1000 * 0.79 * wkend_gngrbeer) %>% 
  rowwise() %>%
  mutate(wkday_alcohol_total = sum(c_across(wkday_beer:wkday_gngrbeer))) %>%
  relocate(wkday_alcohol_total,.after = wkday_gngrbeer) %>%
  mutate(wkend_alcohol_total = sum(c_across(wkend_beer:wkend_gngrbeer))) %>%
  relocate(wkend_alcohol_total,.after = wkend_gngrbeer) %>%
  mutate(total_weekly_alcohol = sum(wkday_alcohol_total, wkend_alcohol_total, na.rm = TRUE)) %>%
  mutate(log_total_weekly_alcohol = log(total_weekly_alcohol))

####3###
#recode birthplace
############
birthplace_key <- read.csv("/Users/oshi/Desktop/TB/birthplace_key_v2.csv") %>%  select (birthplace, size_categ) %>%mutate_at("birthplace", as.character) %>% distinct

#create birthplace_var which combines results from 3 birthplace vars to prevent missingness
#NOTE: "NorthernCapeTBCaseCo-Birthplace_DATA_LABELS_2023-11-08_2321.csv" is downloaded from redcap it is named "Birthplace" download with data labels
update_birthplace <- read.csv("/Users/oshi/Downloads/NorthernCapeTBCaseCo-Birthplace_DATA_LABELS_2023-11-08_2321.csv", header=T,sep = ";" ,na.strings=c(""," ","NA","N/A"))  %>%
  mutate(birthplace_var = case_when(
    !is.na(Birthplace) ~ Birthplace,
    !is.na(Place.of.Birth) & !(Place.of.Birth %in% c("Other", "other")) ~ Place.of.Birth,
    !is.na(Place.of.Birth) & (Place.of.Birth %in% c("Other", "other")) ~ Place.of.birth..if.not.listed.above,
    TRUE ~ NA_character_
  ))  %>% select (Record.ID, birthplace_var) %>% rename(birthplace = birthplace_var,record_id = Record.ID )%>% mutate_at("birthplace", as.character)

#use left join function to merge the birthplace table with the key
#now just select the record.id and the recoded variable
birthplace_recoded = left_join(update_birthplace, birthplace_key, by = "birthplace") %>% rename(birthplace_clean = birthplace)

# merge recoded birthplace with main dataset###
# in this case its alcohol transformed
birthplace.transformed =  left_join(alcohol.transformed, birthplace_recoded, by = "record_id") %>% relocate(birthplace_clean:size_categ, .after = "Clinic_Type")



#
 

#create new variable that converts town size(clinic sampling location) with this key: Large and small towns = town, rural =rural
town.transformed = birthplace.transformed %>% mutate(Residence = case_when(Clinic_Type == "Large town"~ 'Town',Clinic_Type =="Small town"~ 'Town' ,Clinic_Type =="Rural"~ 'Rural')) %>% relocate(Residence, .after = Clinic_Type) %>% mutate(size_categ =as.character(size_categ)) %>% mutate(birthplace_type = case_when(size_categ == "1" ~ 'Rural',size_categ =="2" ~ 'Town',size_categ =="3"~ 'Town', size_categ =="4"~ 'Town')) %>% relocate(birthplace_type, .after = size_categ)

#create new migration variable using key below
#1= birthplace_type(=1) & residence (=1) 
#2= birthplace_type(=2)&residence (=1),3 = birthplace_type (=1) & residence (=2) #4= birthplace_type(=2) & residence(=2)

#migration.transformed = town.transformed %>%
#  mutate(migration = case_when(birthplace_type == "Rural" & residence == "Rural"  ~ 'Rural-Rural', birthplace_type == "Rural" & residence == "Town"  ~ 'Rural-Town', birthplace_type == "Town" & residence == "Rural"  ~ 'Town-Rural',birthplace_type == "Town" & residence == "Town"  ~ 'Town-Town',TRUE ~ NA_character_)) %>% relocate(migration, .after = birthplace_type)

migration.transformed = town.transformed %>%
  mutate(migration = case_when(birthplace_type == "Rural" & Residence == "Rural"  ~ 'Rural-Rural', birthplace_type == "Rural" & Residence == "Town"  ~ 'Lived in a Town', birthplace_type == "Town" & Residence == "Rural"  ~ 'Lived in a Town',birthplace_type == "Town" & Residence == "Town"  ~ 'Lived in a Town',TRUE ~ NA_character_)) %>% relocate(migration, .after = birthplace_type) %>% rename(Birthplace = birthplace_type)

 
  
#rename dataset. and remove NCTEST (561)
  clean.full.data.pre.imputation = migration.transformed %>% filter(sample_id != "NCTest")
  

  #filter HIV TB comorbids and unknown HIV
 pre.imputation.hiv.TB_FILT = clean.full.data.pre.imputation %>%
   filter((hiv == "1" & TB_diagnosis == "control") | (hiv == "0")) %>%
   filter(!is.na(TB_diagnosis)) %>%
   filter((TB_diagnosis=="unknown")) 

 
