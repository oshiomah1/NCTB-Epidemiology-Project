#Random forest model for NCTB case control Dataset
#https://stats.stackexchange.com/questions/145602/justification-for-feature-selection-by-removing-predictors-with-near-zero-varian
install.packages("tree")
install.packages("rfsrc")

#Load packages
library(randomForest)
library(randomForestSRC)
library(tree)
library(ISLR2)
library(ggRandomForests)

#model_1a = glm(TB_diagnosis ~ Residence + sex + smokes + Years.of.education ,imputed.dataset.1.filtered.a1, family = binomial)
#summary(model_1a)
#model_1a = glm(TB_diagnosis ~ Residence + sex + smokes + Years.of.education + drnk_alcohol + age ,imputed.dataset.1.filtered.a1, family = binomial)
#select variables and remove missing data
RF_dataset = imputed.dataset.1.filtered.a1 %>% select(Residence,sex,smokes,Years.of.education,drnk_alcohol, age, TB_diagnosis) %>% rename(
  "Sex" = sex,
  "Smoker" = smokes,
  "Years.Of.Education" = Years.of.education,
  "Drinks.Alcohol" = drnk_alcohol,
  "Age" = age,
  "TB.Status" = TB_diagnosis,
  #"Diabetes" = diabetes
)
#RF_dataset2 = RF_dataset[complete.cases(RF_dataset),] #use this if there's missing data

#do a random forest model (Initial Pass)
NCTB_RF <- randomForest(TB.Status ~ . , data=RF_dataset)
NCTB_RF
# mean(NCTB_RF$rsq) Doesnt work for binary outcomes
varImpPlot(NCTB_RF)

#Try a different number of variables at each split
#use the package randomForestSRC for tuning mtry and nodesize, and other nice features

#Tuned Pass at random forest
FullModel_tune = tune(TB.Status ~ ., importance=TRUE, na.action=c("na.omit"), data=RF_dataset)
# optimize mtry and nodesize
FullModel_tune$optimal[["mtry"]]
FullModel_tune$optimal[["nodesize"]]
# include optimized mtry and nodesize values in tuned model
FullModel_withTuning = rfsrc(TB.Status ~ ., importance="permute", na.action=c("na.omit"), mtry = FullModel_tune$optimal[["mtry"]], nodesize = FullModel_tune$optimal[["nodesize"]], data=RF_dataset)
FullModel_withTuning
vimp(FullModel_withTuning, importance = "anti")$importance
#visualize results
plot(FullModel_withTuning)

# rfsrc_iris <- rfsrc(Species ~ ., data = iris)
# data(rfsrc_iris, package="ggRandomForests")
gg_dta <- gg_vimp(FullModel_withTuning , xvar="Sex") 
plot(gg_dta)





#visualize with gini coefficient
FullModel_withTuning.1 <- randomForest(TB.Status ~ . , mtry =5,nodesize =8, data=RF_dataset)
FullModel_withTuning.1
varImpPlot(FullModel_withTuning.1)
##################################


#Introduce training and testing datasets 
#Split data into training and testing data
NumPoints = nrow(RF_dataset)
Training = NumPoints * 0.7
TrainingInt = round(Training)
TrainingPoints = sample(1:NumPoints, TrainingInt, replace = FALSE)
NCTB.train = RF_dataset[TrainingPoints,]
NCTB.test = RF_dataset[-TrainingPoints,]

#Train RF on the training data
TunedModel_RF_train <- randomForest(TB.Status ~ . , mtry =5,nodesize =8, data=RF_dataset)
TunedModel_RF_train

#Test the trained model on the testing data
RF_test_predictions <- predict(TunedModel_RF_train, NCTB.test)
plot(RF_test_predictions, NCTB.test$TB.Status)
abline(0,1)
#mean((RF_test_predictions-NCTB.test$TB.Status)^2)

Prediction and Calculate Performance Metrics

pred1=predict(rf,type = "prob")
library(ROCR)
perf = prediction(pred1[,2], mydata$Creditability)
# 1. Area under curve
auc = performance(perf, "auc")
auc
# 2. True Positive and Negative Rate
pred3 = performance(perf, "tpr","fpr")
# 3. Plot the ROC curve
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
