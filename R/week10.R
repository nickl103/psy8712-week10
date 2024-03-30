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
  rename('work hours' = MOSTHRS) %>% #renaming MOSTHRS to work hours
  select(-HRS1, -HRS2) %>% #removing HRS1 and HRS2 from data
  select_if(~mean(is.na(.))<.75) %>% #removing any variables with less than 75% missingness
  mutate_all(as.numeric) #converting to as numeric

