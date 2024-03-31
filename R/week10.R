# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)

#Data Import and Cleaning
gss_tbl <- read_sav("../data/GSS2016.sav") %>% #importing data into R
  tibble() %>% #converting to a tibble
  filter(!is.na(MOSTHRS)) %>% #filtering nas from MOSTHRS
  mutate(across(everything(~ifelse(.==0, NA, .)))) %>%
  rename(`work hours` = MOSTHRS) %>% #renaming MOSTHRS to work hours
  select(-HRS1, -HRS2) %>% #removing HRS1 and HRS2 from data
  select_if(~mean(is.na(.))<.75) %>% #removing any variables with less than 75% missingness
  mutate_all(as.numeric) #converting to as numeric

#Visualization
ggplot(gss_tbl, aes(x=`work hours`)) +
geom_histogram() +
labs(x= "Work Hours", y= "Frequency", title= "Frequency of Work Hours Histogram") #visualizing data

#Analysis
set.seed(84) #setting seed for reproducibility 
rows <- sample(nrow(gss_tbl)) #randomly ordering dataset per data camp
gss_shuffle <- gss_tbl[rows, ] #same as above comment
gss_split <- round(nrow(gss_shuffle) * 0.75) #creating split data
gss_train <- gss_shuffle[1:gss_split, ] #creating train data
gss_test <- gss_shuffle[(gss_split + 1):nrow(gss_shuffle), ] #creating test data 


