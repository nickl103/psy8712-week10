# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
library(stats)

#Data Import and Cleaning
gss_tbl <- read_sav("../data/GSS2016.sav") %>% #importing data into R
  mutate_all(~ifelse(.==0, NA, .)) %>% #mutating non-answers to NA%>%
  filter(!is.na(MOSTHRS)) %>% #filtering nas from MOSTHRS
  rename(`work hours` = MOSTHRS) %>% #renaming MOSTHRS to work hours
  select(-HRS1, -HRS2) %>% #removing HRS1 and HRS2 from data
  select(-where(~mean(is.na(.))>.75)) %>% #removing any variables with less than 75% missingness
  mutate_all(as.numeric) #converting to as numeric



#Visualization
ggplot(gss_tbl, aes(x=`work hours`)) +
geom_histogram() +
labs(x= "Work Hours", y= "Frequency", title= "Frequency of Work Hours Histogram") #visualizing data

#Analysis
set.seed(1234) #setting seed for reproducibility 
rows <- sample(nrow(gss_tbl)) #randomly ordering dataset per data camp
gss_shuffle <- gss_tbl[rows, ] #same as above comment
gss_split <- round(nrow(gss_shuffle) * 0.75) #creating split data per data camp
gss_train <- gss_shuffle[1:gss_split, ] #creating train data per data camp
gss_test <- gss_shuffle[(gss_split + 1):nrow(gss_shuffle), ] #creating test data per data camp/ this is also the holdout data

###Pre-setting things needed for models
index_folds <- createFolds(gss_train$`work hours`, 10) #creating folds for indexout. 

myControl <- trainControl(
  method= "cv",
  indexOut= index_folds,
  number= 10, # 10-fold cv, 
  verboseIter = TRUE) #setting these now so I don't have to keep retyping it for each model based on requested number of cv folds and data camp


###OLS model
Ols_Model <- train(`work hours` ~ ., #predicting work hours from all variables in dataset
              data = gss_train, #using split train data for 10-fold CV
              method = "lm", #lm for OLS model
              metric = "Rsquared", #will need later for publication section
              preProcess = "medianImpute", #median Impute to deal with missing values per assignment instructions
              na.action = na.pass, #so model will go passed nas and actually run otherwise it won't work 
              trControl = myControl #used so I didn't have to retype every time. 
)

OLS_Model_test <- predict(Ols_Model, gss_test, na.action= na.pass) #testing OLS model on test data, so the holdout CV

###Elastic Net model

myGrid <- expand.grid(alpha =0:1, lambda =seq(0.0001, 0.1, length = 10)) #setting gridsearch for hyperparameters, used numbers from data camp

EN_Model <- train(`work hours` ~ ., 
              data = gss_train, 
              tuneGrid = myGrid,
              method = "glmnet",#used glmnet to run the elastic net model
              metric= "Rsquared", # need later
              preProcess = "medianImpute", #median Impute per assignment 
              na.action = na.pass, #need otherwise it won't work
              trControl = myControl) #same as above

EN_Model_test <- predict(EN_Model, gss_test, na.action= na.pass) #testing EN model on test data, holdout CV

###Random Forest Model

RF_grid <- expand.grid(mtry= 510 , splitrule= 'variance' , min.node.size= 5 ) #number for mtry chosen through trial and error, used these parameters based on the guide from slides
RF_Model <- train(`work hours` ~ ., 
                data= gss_train, 
                tuneGrid= RF_grid,
                metric= "RSquared",
                method= "ranger", #random forest model 
                preProcess= "medianImpute",
                na.action= na.pass,
                trControl=myControl)
RF_Model_test <- predict(RF_Model, gss_test, na.action= na.pass) #testing RF model on test data, holdout CV

####eXtreme Gradient Boosting
XGB_grid <- expand.grid(nrounds= 50, alpha =1, lambda =.1, eta=.1) #used chatgpt to figure out the grid options for XGB

XGB_Model <- train(`work hours` ~ .,
                   data= gss_train,
                   tuneGrid= XGB_grid,
                   method= "xgbLinear", #to do the xgb model
                   preProcess="medianImpute",
                   na.action= na.pass,
                   trControl= myControl
                   )
XGB_Model_test <- predict(XGB_Model, gss_test, na.action= na.pass)

