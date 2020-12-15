# library(readxl)
# df <- read_excel("Activities_Predict_Litigation.xlsx")
# View(Activities_Predict_Litigation)


## Run a logistic regression model to see how well activities can predict litigation 

require(ISLR)
library(tidyverse)
require(gridExtra)
library(Amelia)
library(kableExtra)
library(caret)
library(DMwR)
library(scales)
library(purrr)
library(RColorBrewer)
library(ROCR)
library(corrplot)
library(digest)
library(openssl)



train <- read.csv("C:/Users/carnout/Documents/train_data.csv")

test <- read.csv("https://raw.githubusercontent.com/crarnouts/Data-698-Thesis/main/test_data.csv")

household_attributes <- read.csv("https://raw.githubusercontent.com/crarnouts/Data-698-Thesis/main/household_attributes_data2.csv")

insured_age <- read.csv("https://raw.githubusercontent.com/crarnouts/Data-698-Thesis/main/insured_age_data.csv")

losslocation <- read.csv("https://raw.githubusercontent.com/crarnouts/Data-698-Thesis/main/losslocation_data.csv")

billing_attributes <- read.csv("https://raw.githubusercontent.com/crarnouts/Data-698-Thesis/main/billing_attributes_data.csv")

ProviderMeanEncodingTest <- read.csv("https://raw.githubusercontent.com/crarnouts/Data-698-Thesis/main/ProviderMeanEncodingTest_data.csv")

ProviderMeanEncodingTrain <- read.csv("https://raw.githubusercontent.com/crarnouts/Data-698-Thesis/main/ProviderMeanEncodingTrain_data.csv")




billing_attributes <- billing_attributes %>% select(ClaimNumber,Bill_Ct,Total_Bill_Ct,DistinctServiceDateCount,ServiceGroup)
household_attributes <- household_attributes %>% select(ClaimNumber,HHStandardizedRelativePremium,HHStandardizedRelativeLossRatio
                                                        ,HHStandardizedRelativeLossCount,HHStandardizedRelativeLossPaid,HouseholdLongevity
                                                        ,ActivePolicies,CancelledPolicies,YearlyTotalPremium)


train <- merge(train,losslocation)
test <- merge(test,losslocation)


 train <- merge(train,ProviderMeanEncodingTrain, all.x = TRUE)
 test <- merge(test,ProviderMeanEncodingTest, all.x = TRUE)
 
train <- merge(train,billing_attributes, all.x = TRUE)
test <- merge(test,billing_attributes, all.x = TRUE)

train <- merge(train,household_attributes, all.x = TRUE)
test <- merge(test,household_attributes, all.x = TRUE)

train <- merge(train,insured_age, all.x = TRUE)
test <- merge(test,insured_age, all.x = TRUE)

### Encoding for Claims that don't have billing attributes
  
train$Bill_Ct[is.na(train$Bill_Ct)] <- 0
train$Total_Bill_Ct[is.na(train$Total_Bill_Ct)]<-0
train$DistinctServiceDateCount[is.na(train$DistinctServiceDateCount)]<-0
train$ServiceGroup[is.na(train$ServiceGroup)]<- "No Medical Review in Firsts 60 Days"


test$Bill_Ct[is.na(test$Bill_Ct)] <- 0
test$Total_Bill_Ct[is.na(test$Total_Bill_Ct)]<-0
test$DistinctServiceDateCount[is.na(test$DistinctServiceDateCount)]<-0
test$ServiceGroup[is.na(test$ServiceGroup)]<- "No Medical Review in Firsts 60 Days"
 

# Using the MICE package to impute for any NULL Values
 library(mice)
 
 imputed_Data <- mice(train,method = 'cart')
 imputed_Data2 <- mice(test,method = 'cart')
 
 
 train <- complete(imputed_Data,2)
 

 test <- complete(imputed_Data2,2)
 
 # mean imputation
 for(i in 1:ncol(train)) {
   train[ , i][is.na(train[ , i])] <- mean(train[ , i], na.rm = TRUE)
 }
 
 # mean imputation
 for(i in 1:ncol(test)) {
   test[ , i][is.na(test[ , i])] <- mean(test[ , i], na.rm = TRUE)
 }

 

### Lets try out the ol random forest ##
#################### BRING IN THE RANDOM FOREST FUNCTION #############################

source("https://raw.githubusercontent.com/crarnouts/Random_Forest_Function/master/RandomForestNulls_testing.R")
source("https://raw.githubusercontent.com/crarnouts/Corey-s_Scripts_For_Reference/master/Density_diff_2.R")
######################################################################################

 # Renaming Process Variables so that are more intuitive
 train <- train %>% 
  dplyr::rename(
    ProcessLitRate = AVGRate,
    CredProcessLitRate = AVGCredRate 
  )

test <- test %>% 
  dplyr::rename(
    ProcessLitRate = AVGRate,
    CredProcessLitRate = AVGCredRate 
  )


## create training dataset that will be used fot the 60 day model
train2 <- train %>% dplyr::select(lat,long,ProcessLitRate,litigationind,CredibleProviderLawsuitAverage,Bill_Ct 
                                  ,Total_Bill_Ct,DistinctServiceDateCount,ServiceGroup
                                  ,HHStandardizedRelativePremium,HHStandardizedRelativeLossRatio
                                  ,HHStandardizedRelativeLossCount,HHStandardizedRelativeLossPaid,HouseholdLongevity
                                  ,ActivePolicies,CancelledPolicies,ActivePolicies,CancelledPolicies,InsuredAge)

## create training dataset that will be used for the day 1 model
train3 <- train %>% dplyr::select(lat,long,litigationind
                                  ,HHStandardizedRelativePremium,HHStandardizedRelativeLossRatio
                                  ,HHStandardizedRelativeLossCount,HHStandardizedRelativeLossPaid,HouseholdLongevity
                                  ,ActivePolicies,CancelledPolicies,ActivePolicies,CancelledPolicies,InsuredAge)


## A look at the insured age variable #####
library(ggplot)
library(ggthemes)
ggplot(train2,aes(InsuredAge))+geom_density(aes(color=as.factor(litigationind),fill=as.factor(litigationind),alpha=0.1))+
  labs(x="Insured Age",y="Density",title="Insured Age Density for Litigated and non-Litigated Claims")+
  theme(legend.position = "top")+theme_economist()





## Create a Logistic Regression model to establish a baseline model

model1 <- glm(litigationind ~ ProcessLitRate+CredibleProviderLawsuitAverage
              +DistinctServiceDateCount+lat+long+Total_Bill_Ct+ServiceGroup, 
              family = binomial(link = "logit"), 
              train)
summary(model1)

test$model1 <- predict.glm(model1, test,"response")

cor(test$model1,test$litigationind)

## Day One logistic regression model

model2 <- glm(litigationind ~ ., 
              family = binomial(link = "logit"), 
              train3)
summary(model2)

test$model2 <- predict.glm(model2, test,"response")

cor(test$model2,test$litigationind)



## Make predictions on the test dataset using the Random forest function that I created
test <- RF_with_Nulls(train2,test,"litigationind",.5,4,100,.003,10,300)


## Create a Factor version of the target variable

test$litigationindfact <- as.factor(test$litigationind)

#################### Gradient Boosting Model ##########################################

# create the gradient boosting machine with caret package and utilize the cross validation
library(gbm)

set.seed(42)
gbm_model <- train(litigationind  ~ ., data = train2, method="gbm", verbose = FALSE,
                   trControl = trainControl("cv", number = 5))



gbm_model

# Make predictions using the gbm_model on the test dataset 
test$gbm_predictions <- predict(gbm_model, test)

# Day one model with GBM 

set.seed(42)
gbm_model_day_one <- train(litigationind  ~ ., data = train3, method="gbm", verbose = FALSE,
                   trControl = trainControl("cv", number = 5))



gbm_model_day_one

# Make predictions using the gbm_model on the test dataset 
test$gbm_model_day_one_predictions <- predict(gbm_model_day_one, test)

###############################################################################################
########### Random Forest Function from Ranger ################################################





rf_model <- train(litigationind  ~ ., data = train2, method = "ranger", 
                  scale = TRUE,
                  trControl = trainControl("cv", number = 3))
test$rf_predictions <- predict(rf_model, test)





##############################################################################

library(pROC)
roc(test$litigationindfact, test$model1, plot = TRUE, legacy.axes = TRUE, print.auc = TRUE)


roc(test$litigationindfact, test$prediction_overall, plot = TRUE, legacy.axes = TRUE, print.auc = TRUE)


roc(test$litigationindfact, test$gbm_predictions, plot = TRUE, legacy.axes = TRUE, print.auc = TRUE)

roc(test$litigationindfact, test$gbm_model_day_one_predictions, plot = TRUE, legacy.axes = TRUE, print.auc = TRUE)

roc(test$litigationindfact, test$rf_predictions, plot = TRUE, legacy.axes = TRUE, print.auc = TRUE)

# roc(test$litigationindfact, test$model2, plot = TRUE, legacy.axes = TRUE, print.auc = TRUE)

################################## Assemble a table with the best model #################



nintyfifth_percentile <- mean(test$litigationind[test$gbm_predictions> quantile(test$gbm_predictions, .95)])/mean(test$litigationind)

nintypercentile <-mean(test$litigationind[test$gbm_predictions> quantile(test$gbm_predictions, .90)])/mean(test$litigationind)

eightypercentile <-mean(test$litigationind[test$gbm_predictions> quantile(test$gbm_predictions, .80)])/mean(test$litigationind)

seventypercentile <-mean(test$litigationind[test$gbm_predictions> quantile(test$gbm_predictions, .70)])/mean(test$litigationind)

sixtypercentile <-mean(test$litigationind[test$gbm_predictions> quantile(test$gbm_predictions, .60)])/mean(test$litigationind)
fiftypercentile <-mean(test$litigationind[test$gbm_predictions> quantile(test$gbm_predictions, .50)])/mean(test$litigationind)

belowfiftypercentile <-mean(test$litigationind[test$gbm_predictions< quantile(test$gbm_predictions, .50)])/mean(test$litigationind)

belowfortypercentile <-mean(test$litigationind[test$gbm_predictions< quantile(test$gbm_predictions, .40)])/mean(test$litigationind)

belowthirtypercentile <-mean(test$litigationind[test$gbm_predictions< quantile(test$gbm_predictions, .30)])/mean(test$litigationind)

belowtwentypercentile <-mean(test$litigationind[test$gbm_predictions< quantile(test$gbm_predictions, .20)])/mean(test$litigationind)

belowtenthpercentile <-mean(test$litigationind[test$gbm_predictions< quantile(test$gbm_predictions, .10)])/mean(test$litigationind)



litigationFound95 <- .05* nintyfifth_percentile
litigationFound90<- .1 * nintypercentile
litigationFound80 <- .2*eightypercentile
litigationFound70 <- .3* seventypercentile
litigationFound60 <- .4*sixtypercentile
litigationFound50<- .5*fiftypercentile
litigationFoundbelow50<- .5*belowfiftypercentile
litigationFoundbelow40<- .4*belowfortypercentile
litigationFoundbelow30<- .3*belowthirtypercentile
litigationFoundbelow20<- .2*belowtwentypercentile
litigationFoundbelow10<- .1*belowtenthpercentile

library(data.table)

dt <- data.table(x = c(".95", ".90",".8",".7",".6",".5","below .5","below .4","below .3","below .2","below .1"), y = c(nintyfifth_percentile,nintypercentile,eightypercentile,seventypercentile,sixtypercentile,fiftypercentile,belowfiftypercentile,belowfortypercentile,belowthirtypercentile,belowtwentypercentile,belowtenthpercentile),
                 z =c(litigationFound95,litigationFound90,litigationFound80,litigationFound70,litigationFound60,
                      litigationFound50,litigationFoundbelow50,litigationFoundbelow40,litigationFoundbelow30,litigationFoundbelow20,litigationFoundbelow10))

dt$Predictive_Percentiles <- dt$x
dt$EfficiencyMultiplier <- dt$y
dt$litigationFound <- dt$z
dt$x <- NULL
dt$y <- NULL
dt$z <- NULL

dt$EfficiencyMultiplier <- format(round(dt$EfficiencyMultiplier, 2), nsmall = 2)
dt$litigationFound <- format(round(dt$litigationFound, 2), nsmall = 2)


kable(dt) %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(1:6, bold = T, color = "white", background = "#D7261E")

#################### DAY ONE MODEL ##############################################################


nintyfifth_percentile <- mean(test$litigationind[test$gbm_model_day_one_predictions> quantile(test$gbm_model_day_one_predictions, .95)])/mean(test$litigationind)

nintypercentile <-mean(test$litigationind[test$gbm_model_day_one_predictions> quantile(test$gbm_model_day_one_predictions, .90)])/mean(test$litigationind)

eightypercentile <-mean(test$litigationind[test$gbm_model_day_one_predictions> quantile(test$gbm_model_day_one_predictions, .80)])/mean(test$litigationind)

seventypercentile <-mean(test$litigationind[test$gbm_model_day_one_predictions> quantile(test$gbm_model_day_one_predictions, .70)])/mean(test$litigationind)

sixtypercentile <-mean(test$litigationind[test$gbm_model_day_one_predictions> quantile(test$gbm_model_day_one_predictions, .60)])/mean(test$litigationind)
fiftypercentile <-mean(test$litigationind[test$gbm_model_day_one_predictions> quantile(test$gbm_model_day_one_predictions, .50)])/mean(test$litigationind)

belowfiftypercentile <-mean(test$litigationind[test$gbm_model_day_one_predictions< quantile(test$gbm_model_day_one_predictions, .50)])/mean(test$litigationind)

belowfortypercentile <-mean(test$litigationind[test$gbm_model_day_one_predictions< quantile(test$gbm_model_day_one_predictions, .40)])/mean(test$litigationind)

belowthirtypercentile <-mean(test$litigationind[test$gbm_model_day_one_predictions< quantile(test$gbm_model_day_one_predictions, .30)])/mean(test$litigationind)

belowtwentypercentile <-mean(test$litigationind[test$gbm_model_day_one_predictions< quantile(test$gbm_model_day_one_predictions, .20)])/mean(test$litigationind)

belowtenthpercentile <-mean(test$litigationind[test$gbm_model_day_one_predictions< quantile(test$gbm_model_day_one_predictions, .10)])/mean(test$litigationind)



litigationFound95 <- .05* nintyfifth_percentile
litigationFound90<- .1 * nintypercentile
litigationFound80 <- .2*eightypercentile
litigationFound70 <- .3* seventypercentile
litigationFound60 <- .4*sixtypercentile
litigationFound50<- .5*fiftypercentile
litigationFoundbelow50<- .5*belowfiftypercentile
litigationFoundbelow40<- .4*belowfortypercentile
litigationFoundbelow30<- .3*belowthirtypercentile
litigationFoundbelow20<- .2*belowtwentypercentile
litigationFoundbelow10<- .1*belowtenthpercentile

library(data.table)

dt <- data.table(x = c(".95", ".90",".8",".7",".6",".5","below .5","below .4","below .3","below .2","below .1"), y = c(nintyfifth_percentile,nintypercentile,eightypercentile,seventypercentile,sixtypercentile,fiftypercentile,belowfiftypercentile,belowfortypercentile,belowthirtypercentile,belowtwentypercentile,belowtenthpercentile),
                 z =c(litigationFound95,litigationFound90,litigationFound80,litigationFound70,litigationFound60,
                      litigationFound50,litigationFoundbelow50,litigationFoundbelow40,litigationFoundbelow30,litigationFoundbelow20,litigationFoundbelow10))

dt$Predictive_Percentiles <- dt$x
dt$EfficiencyMultiplier <- dt$y
dt$litigationFound <- dt$z
dt$x <- NULL
dt$y <- NULL
dt$z <- NULL

dt$EfficiencyMultiplier <- format(round(dt$EfficiencyMultiplier, 2), nsmall = 2)
dt$litigationFound <- format(round(dt$litigationFound, 2), nsmall = 2)


kable(dt) %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(1:6, bold = T, color = "white", background = "#D7261E")


#####################################################################################################
############## Variable Importance 
varimportance <- varImp(gbm_model)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) 

colnames(varimportance) <- c("Feature","Feature_Importance")

varimportance <- varimportance[order(-varimportance$Feature_Importance),]

kable(varimportance)%>%
  kable_styling("striped", full_width = F)

