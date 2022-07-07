source("/Users/oshi/Desktop/TB/3_Imputation.R") #change to source of your imputation script
library(lmtest)
library(tidyverse)
library(effects)
library(performance)
library(huxtable) # knit regression outputs
library(jtools) #plt summaries of multiple models using plotsumms
library(ggstance)#plt summaries of multiple models using plotsumms
library(gtsummary)
library(ggplot2)
library(effects)
##library(cowplot)
library(forcats)
library(questionr) # odds ratio model converteer

#########################################
#Recoding factor ordering#
################################

imputed.dataset.1.filtered.a<- within(imputed.dataset.1.filtered.a, drnk_alcohol <- relevel(drnk_alcohol, ref = "Non-Drinker"))
imputed.dataset.1.filtered.a1<- within(imputed.dataset.1.filtered.a, diabetes <- relevel(diabetes, ref = "Non-Diabetic"))
imputed.dataset.1.filtered.a1$Clinic_Type<- fct_relevel(imputed.dataset.1.filtered.a1$Clinic_Type, "Rural", "Small town", "Large town")

imputed.dataset.1.filtered.a1$Clinic_Type<- factor(imputed.dataset.1.filtered.a1$Clinic_Type,levels = c("Rural", "Small town", "Large town"), ordered = "TRUE")
#imputed.dataset.1.filtered.a1$age2 <- (imputed.dataset.1.filtered.a1$age)^2

#rename factors
#imputed.dataset.1.filtered.a1$Clinic_Type<- recode_factor(imputed.dataset.1.filtered.a1$Clinic_Type,'Rural' ="Rural Area", 'Small town' = "Small Town", 'Large town' = "Large Town")

#imputed.dataset.1.filtered.a1$Residence<- fct_relevel(imputed.dataset.1.filtered.a1$residence,"Town", "Rural" )
#imputed.dataset.1.filtered.a1$Birthplace<- fct_relevel(imputed.dataset.1.filtered.a1$birthplace_type, "Town", "Rural")
young.peeps.model.data = imputed.dataset.1.filtered.a1 %>% filter(age < 55)
#imputed.dataset.migration$Clinic_Type <- fct_relevel(imputed.dataset.migration$Clinic_Type, "Rural", "Small town", "Large town")
 
#########################################
# models 4 hypotheses
################################

library(car)# for anova glm
##model 1 w/out covariates plausibly strongly related to survivor ship therefore a model likely to minimize collider bias
model_1a = glm(TB_diagnosis ~ Residence + sex + smokes + Years.of.education + drnk_alcohol + age ,imputed.dataset.1.filtered.a1, family = binomial)
odds.ratio(model_1a)

model_1b = glm(TB_diagnosis ~ Residence + sex + smokes + Years.of.education + drnk_alcohol + age ,young.peeps.model.data, family = binomial)
odds.ratio(model_1b)
################################################
#Make model predictions and add 2 data set (rounded to 2 decimal plces)
######################################################
imputed.dataset.1.filtered.a1$predprob_model_1a<-round(fitted(model_1a),2)
imputed.dataset.1.filtered.a1 <- imputed.dataset.1.filtered.a1 %>% relocate(predprob_model_1a, .after = "TB_diagnosis")
classification.table.m1a<-table(imputed.dataset.1.filtered.a1$TB_diagnosis,imputed.dataset.1.filtered.a1$predprob_model_1a > 0.5)
classification.table.m1a
#head(data,n=10) 
#anova
Anova(model_1a, type=c("II"), test.statistic=c("LR"))

# visualizing model 1a results
plot(allEffects(model_1a))
plot(predictorEffects(model_1a, ~ Residence + sex + smokes + Years.of.education),
     
     axes=list(
       x=list(rotate=0,
              Residence = list(lab = "Residence"),
              sex= list(lab = "Sex"),
              smokes = list(lab = "Smoking"),
              Years.of.education = list(lab = "Years of Education")
       ),
      y=list(transform=exp, lab="Odds of Active TB", type="link")),
     main = " "
)
title(main ="Common Risk Factors")


summ(model_1a, exp = TRUE)


model_2a = glm(TB_diagnosis ~ Residence + sex + smokes + cohort2*Years.of.education   ,imputed.dataset.1.filtered.a1, family = binomial)
odds.ratio(model_2a)
Anova(model_2a, type=c("II"), test.statistic=c("LR"))
modl= glm(TB_diagnosis ~ Residence + sex + smokes + age2*Years.of.education   ,imputed.dataset.1.filtered.a1, family = binomial)


#leae the results as the analys, talk anbout survivor na;uysis and survivorship

plot(predictorEffects(model_2a, ~ Years.of.education),
     axes=list(
       x=list(rotate=0, Years.of.education = list(lab = "Years of Education")),
       y=list(transform=exp, lab="Odds of Active TB", type="link")),
     main = " "
)
anova(model_1a, model_2a)
## New models with Migration ##
model_3a = glm(TB_diagnosis ~ Birthplace*Residence + sex + smokes + Years.of.education ,imputed.dataset.1.filtered.a1, family = binomial)
odds.ratio(model_3a)
#anova
Anova(model_3a, type=c("II"), test.statistic=c("LR"))
plot(predictorEffects(model_3a, ~ Residence),
     axes=list(
       x=list(rotate=0,  Residence = list(lab = "Current Residence")),
       y=list(transform=exp, lab="Odds of Active TB", type="link")),
     main = " "
)
model_3b = glm(TB_diagnosis ~ migration + sex + smokes + Years.of.education ,imputed.dataset.1.filtered.a1, family = binomial)
odds.ratio(model_3b)
#anova
Anova(model_3b, type=c("II"), test.statistic=c("LR"))
# visualizing model 3b results
plot(predictorEffects(model_3b, ~ migration),
     axes=list(
       x=list(rotate=0,  Birthplace = list(lab = " Migration")),
       y=list(transform=exp, lab="Odds of Active TB", type="link")),
     main = " "
)




#Plot effect one by 1 or in a grid MAIN effects
#sex effect
SEX.EFFECT = plot(effect("sex",B),main="Sex", 
                  ylab="Predicted Probability of Active TB", xlab = "" )
#smoke effect
SMOKE.EFFECT = plot(effect("smokes",B),main="Smoking", 
                    ylab="Predicted Probability of Active TB", xlab = "")
#town size effect
POP.SIZE.EFFECT = plot(effect("Clinic_Type",B),main="Town Size", 
                       ylab="Predicted Probability of Active TB", xlab = "")
POP.SIZE.EFFECT2 = plot(effect("Clinic_Type",B),main="Town Size", 
                        ylab="Predicted Probability of Active TB", xlab = "")
plot_grid(SEX.EFFECT,SMOKE.EFFECT,POP.SIZE.EFFECT, labels = "AUTO", nrow = 1) 





  

#check_collinearity(fixed_effect_model)
#check_model(fixed_effect_model)
 #education by age effect

 birth.by.town = plot(effect("residence*birthplace_type",C),main="birth.by.town", 
                  ylab="Predicted Probability of Active TB", xlab = "Residence") 


plot(effect("Clinic_Type:birthplace_type",C1),main="birth.by.town", 
     ylab="Predicted Probability of Active TB", xlab = "Birthplace") 

plot_summs(A,B, coefs = c("sex" = "sex-Male", "smokes"  = "Smoker", "diabetes" = "Diabetic" , "drnk_alcohol" = "Drinker","Years.of.education"= "Years of Education", "age" , "Years.of.education:age" = "Years.of.education*age","birthplace_type" = ),model.names =c("A", "B"))


#######################
#likelihood ratio test

#lrtest(A ,B)
#lrtest(C,D)

# INTERACTIONS effects plots

#education by age effect
ed.by.age = plot(effect("Years.of.education:age",B),main="Years of Education by Age", 
     ylab="Predicted Probability of Active TB", xlab = "Years of Education") 

plot_grid(ed.by.age, labels = "AUTO", nrow = 2) 


#export ancestry glm with odds ratios to csv file 

##
############################################
##  IGNORE BELOW THIS: DO  NOT RUN ##  
##########################################


#ARCHIVE OROGNAL MODELS#

# change below to Mark's idea:
##  Original Models for hypothesis 1 and 2
#model_1 = glm(TB_diagnosis ~ Clinic_Type + sex + smokes + diabetes+ drnk_alcohol+ Years.of.education +age  ,imputed.dataset.1.filtered.a1, family = binomial)
#odds.ratio(model_1)
#anova
#Anova(model_1, type=c("II"), test.statistic=c("LR"))

model_2 = glm(TB_diagnosis ~ Clinic_Type + sex + smokes + diabetes+ drnk_alcohol+ cohort2*Years.of.education   ,imputed.dataset.1.filtered.a1, family = binomial)
odds.ratio(model_2)
Anova(model_2, type=c("II"), test.statistic=c("LR"))
B = glm(TB_diagnosis ~ Clinic_Type + sex + smokes + diabetes+ drnk_alcohol+ Years.of.education*age  ,imputed.dataset.1.filtered.a1, family = binomial)




#alternate effects and interaction  plot#
#create effect plots ggpredict
#model.effect = ggpredict(fixed_effect_model2,terms = c("Years.of.education","smokes"))
#model.effect.df = data_frame(model.effect)
#plot(model.effect, xlim=c(0,15))
#plot(model.effect, facet = TRUE)

##age by Years.of.education interaction
#age.Years.of.education.effect = ggeffect(fixed_effect_model2,terms = c("Years.of.education","age"),default.levels=10)
#plot(age.Years.of.education.effect) +
#      scale_x_continuous(limits = c(0,14)) +xlab("Years of Education") +ylab("Predicted Probability of Active TB") + geom_rug()

#glm model
ancestry.model.1 = glm(TB_diagnosis ~ KhoeSan + sex , data = raw_merge , family = binomial)
summary(ancestry.model.1)

ancestry.model.2 = glm( TB_diagnosis ~ Clinic_Type + sex +KhoeSan +Years.of.education+smokes +diabetes+age + drnk_alcohol , data = raw_merge , family = binomial)
summary(ancestry.model.2)

ancestry.model.3 = glm( TB_diagnosis ~ Clinic_Type + sex +KhoeSan +smokes +diabetes + drnk_alcohol +age*Years.of.education , data = raw_merge , family = binomial)
summary(ancestry.model.3)
odds.ratio(ancestry.model.3)
plot(allEffects(ancestry.model.3))



ancestry.model.4 = glm( TB_diagnosis ~ Clinic_Type + sex +KhoeSan+smokes +diabetes + drnk_alcohol +age+Years.of.education , data = raw_merge , family = binomial)
summary(ancestry.model.4)
odds.ratio(ancestry.model.4)
plot(allEffects(ancestry.model.4))


#export ancestry glm with odds ratios to csv file 
export_summs(ancestry.model.3, exp =TRUE, scale = TRUE,error_format = "[{conf.low}, {conf.high}]", to.file = "xlsx",file.name = "ancestry_model3_11_01.xlsx",digits = 3)

odds.ratio(fixed_effect_model)
odds.ratio(fixed_effect_model_no_int)
odds.ratio(ancestry.model.1)
odds.ratio(ancestry.model.2)
odds.ratio(ancestry.model.3)


#ggplot(epi.model.data,aes( fill = sex)) +
geom_bar(aes(smokes)) +scale_x_discrete(breaks = c("0", "1"),labels = c(" No", "Yes"))+ scale_fill_discrete(breaks = c("2", "1"),labels = c("Female", "Male"))


#random effects will change the variance but nit the exoected value



#plot_summs(fixed_effect_model,exp =TRUE, scale = TRUE,error_format = "[{conf.low}, {conf.high}]")

#regression table fixed effects
#huxreg(fixed_effect_model,exp =TRUE)



#export_summs(C, exp =TRUE, scale = FALSE,error_format = "[{conf.low}, {conf.high}]", to.file = "xlsx",file.name = "C.xlsx",digits = 3)
#export_summs(D, exp =TRUE, scale = FALSE,error_format = "[{conf.low}, {conf.high}]", to.file = "xlsx",file.name = "D.xlsx",digits = 3)

#############

old.peeps.model.data = imputed.dataset.1.filtered.a %>% filter(age > 45)

young.peeps.model = glm(TB_diagnosis ~ Clinic_Type + sex +smokes+diabetes+ drnk_alcohol+ Years.of.education + age  ,young.peeps.model.data, family = binomial)
old.peeps.model = glm(TB_diagnosis ~ Clinic_Type + sex +smokes+diabetes+ drnk_alcohol+ Years.of.education + age,old.peeps.model.data, family = binomial)

summary(young.peeps.model)
odds.ratio(young.peeps.model)
plot(allEffects(young.peeps.model))

summary(old.peeps.model)
odds.ratio(old.peeps.model)
plot(allEffects(old.peeps.model))

plot_summs(young.peeps.model,old.peeps.model,model.names =c("3", "4"),exp = TRUE,coefs = c("sex - Male" = "sexMale", "Smoker"  = "smokesSmoker", "diabetes" = "diabetesNonDiabetic" , "Drinker" = "drnk_alcoholDrinker","Years of Education"= "Years.of.education", "Rural" = "Clinic_TypeRural" ,"Small Town" = "Clinic_TypeSmall town", "Age"  = "age"))
####################

#C1 = glm(TB_diagnosis ~ Clinic_Type*birthplace_type + sex + smokes + diabetes+ drnk_alcohol+ Years.of.education + age  ,imputed.dataset.1.filtered.a1, family = binomial)

#D = glm(TB_diagnosis ~ migration + sex + smokes + diabetes+ drnk_alcohol+ Years.of.education*age  ,imputed.dataset.1.filtered.a1, family = binomial)

model4 = glm(TB_diagnosis ~ migration + sex + smokes + diabetes+ drnk_alcohol+ Years.of.education+age  ,imputed.dataset.1.filtered.a1, family = binomial)


model5 = glm(TB_diagnosis ~ migration + sex + smokes + diabetes+ drnk_alcohol+ Years.of.education*age  ,imputed.dataset.1.filtered.a1, family = binomial)

############################################
## rural only models ##  
##########################################


rural = imputed.dataset.1.filtered.a1 %>% filter(residence == "Rural")
rural.B2 = glm(TB_diagnosis ~ sex + smokes + diabetes+ drnk_alcohol+ Years.of.education*age  ,rural, family = binomial) 
odds.ratio(rural.B2)
plot(allEffects(rural.B2))
#w/bplace
rural.C0 = glm(TB_diagnosis ~ birthplace_type + sex + smokes + diabetes+ drnk_alcohol+ Years.of.education*age  ,rural, family = binomial)
odds.ratio(rural.C0)
plot(allEffects(rural.C0))
############################################
## TOWN only models ##  
##########################################
town = imputed.dataset.1.filtered.a1 %>% filter(residence == "Town")
town.B2 = glm(TB_diagnosis ~ sex + smokes + diabetes+ drnk_alcohol+ Years.of.education*age  ,town, family = binomial) 
odds.ratio(town.B2)
plot(allEffects(town.B2))

#w/bplace
town.C0 = glm(TB_diagnosis ~ birthplace_type + sex + smokes + diabetes+ drnk_alcohol+ Years.of.education*age  ,town, family = binomial)
odds.ratio(town.C0)
plot(allEffects(town.C0))


#rename coefficients
plot_summs(rural.B2, town.B2,rural.C0, town.C0, coefs = c("sex-Male" = "sexMale", "Smoker" = "smokesSmoker", "Diabetic" = "diabetesDiabetic", "Drinker"="drnk_alcoholDrinker","Age"="age", "Years of Education" = "Years.of.education", "Years.of.education:age" = "Years.of.education:age", "Birthplace" = "birthplace_type") , model.names =c("rural.B2", "town.B2","rural.C0", "town.C0"),exp = TRUE)
