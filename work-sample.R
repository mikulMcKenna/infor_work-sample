# Org: Infor
# Role: Behavioral Analyst
# Project: Work Sample

# Step 1: Load packages
library("tidyverse")
library("readxl")
library("lubridate")
library("janitor")


# Step 2: read in data
raw <- read_excel("01_raw-data/Sales Rep Data.xlsx",
                  sheet = "Sales Rep Data") %>% 
  clean_names()


# Step 3: data cleaning
clean <- 
  raw %>%
  mutate(study_date = excel_numeric_to_date(study_date),
         hire_date = excel_numeric_to_date(hire_date),
         age = 2018 - birthday,
         ethnicity_clean = case_when(ethnicity == "American Indian or Alaskan Native" ~ "American Indian or Alaskan Native",
                                     ethnicity == "Black/African American" ~ "Black or African American",
                                     ethnicity == "Asian" ~ "Asian",
                                     ethnicity == "White" ~ "White",
                                     ethnicity == "Two or More Races" ~ "Two or More Races",
                                     ethnicity == "Hispanic" ~ "Hispanic",
                                     ethnicity == "Other" ~ "Ethnicity Unknown",
                                     ethnicity == "Do not wish to self-identify" ~ "Ethnicity Unknown",
                                     is.na(ethnicity) ~ "Ethnicity Unknown"))
                        