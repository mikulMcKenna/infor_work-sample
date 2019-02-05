# Org: Infor
# Role: Behavioral Analyst
# Project: Work Sample

# Step 1: Load packages ---------
library("tidyverse")
library("readxl")
library("lubridate")
library("janitor")
library("skimr")


# Step 2: read in and reformat necessary data ---------
raw <- read_excel("01_raw-data/Sales Rep Data.xlsx",
                  sheet = "Sales Rep Data") %>% 
  clean_names() %>%
  mutate(study_date = excel_numeric_to_date(study_date),
         hire_date = excel_numeric_to_date(hire_date),
         age_clean = 2018 - birthday,
         ethnicity_clean = case_when(ethnicity == "American Indian or Alaskan Native" ~ "American Indian or Alaskan Native",
                                     ethnicity == "Black/African American" ~ "Black or African American",
                                     ethnicity == "Asian" ~ "Asian",
                                     ethnicity == "White" ~ "White",
                                     ethnicity == "Two or More Races" ~ "Two or More Races",
                                     ethnicity == "Hispanic" ~ "Hispanic",
                                     ethnicity == "Other" ~ "Ethnicity Unknown",
                                     ethnicity == "Do not wish to self-identify" ~ "Ethnicity Unknown",
                                     is.na(ethnicity) ~ "Ethnicity Unknown"))


# Step 3: data cleaning ---------

# Checking for duplicate cases
dupes <- get_dupes(raw, sales_rep_name)
## 2 duplicate cases
### Scherer, Jacob: pretty certain 1 is a dupe, only diff is birth year (1 year off), removing both
### Kane, Jamie: hiring date, fit index, and recc are same, but diff perf, gender, and year, removing both just to be safe

clean <-
  raw %>% 
  filter(sales_rep_name != "Scherer, Jacob",
         sales_rep_name != "Kane, Jamie")

# Checking for complete lack of performance criteria
perf_filter <-
  clean %>%
  select(oct_17_closed:jul_18_closed) %>% 
  replace(is.na(.), 0) %>%
  mutate(sum = rowSums(abs(.))) %>% 
  filter(sum < 1)
## No employees with NO perf data

# Checking perf criteria for issues
clean %>% 
  select(sales_rep_name, oct_17_closed:jul_18_closed) %>% 
  skim()
## Some cases have -perf, others have decimal points

perf_issue <- 
  clean %>% 
  select(oct_17_closed:jul_18_closed) %>% 
  mutate(oct_17_neg = if_else(oct_17_closed < 0,1,0),
         nov_17_neg = if_else(nov_17_closed < 0,1,0),
         dec_17_neg = if_else(dec_17_closed < 0,1,0),
         jan_18_neg = if_else(jan_18_closed < 0,1,0),
         feb_18_neg = if_else(feb_18_closed < 0,1,0),
         mar_18_neg = if_else(mar_18_closed < 0,1,0),
         apr_18_neg = if_else(apr_18_closed < 0,1,0),
         may_18_neg = if_else(may_18_closed < 0,1,0),
         jun_18_neg = if_else(jun_18_closed < 0,1,0),
         jul_18_neg = if_else(jul_18_closed < 0,1,0))
  

test <- perf_issue %>%
  select(oct_17_neg:jul_18_neg) %>% 
  replace(is.na(.), 0) %>%
  mutate(sum = rowSums(.[1:10]))

table(test$sum)
## 5 candidates with a neg score (1 each)

## Setting any -perf values to NA (rather than removing) for the moment
clean_noPerf <- 
  clean %>% 
  mutate_all(funs(replace(., .<0, NA))) 

skim(clean_noPerf)

## Checking for missing assessment data
clean_noPerf %>% 
  count(is.na(fit_index))

clean_noPerf_noAssm <- clean_noPerf %>% 
  add_count(is.na(fit_index)) %>% 
  filter(n != 1)
