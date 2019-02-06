# Org: Infor
# Role: Behavioral Analyst
# Project: Work Sample

# Step 1: load packages ---------
library("tidyverse")
library("readxl")
library("lubridate")
library("janitor")
library("skimr")
library("psych")
library("ggcorrplot")


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
# 1 candidate missing assessment data: Applegate, Joseph

clean_noPerf_noAssm <- clean_noPerf %>% 
  add_count(is.na(fit_index)) %>% 
  filter(n != 1) %>% 
  select(-is.na(fit_index), -n)

data <- clean_noPerf_noAssm

rm(clean, clean_noPerf, clean_noPerf_noAssm, dupes, t)

# Step 4: data munging --------

data_eda <- 
  data %>% 
  mutate(hire_date_short = ym(hire_date, label = TRUE),
         hire_year = year(hire_date))
table(data_eda$hire_month, data_eda$hire_year)  

data_eda %>% 
  ggplot(aes(x = days_employed)) +
  geom_histogram()

# Removing first two months of perf for candidates hired within available perf data
data_eda <- data_eda %>% 
  mutate(oct_17_closed = ifelse(hire_year == 2017 & hire_month == "Aug", NA, oct_17_closed),
         oct_17_closed = ifelse(hire_year == 2017 & hire_month == "Sep", NA, oct_17_closed),
         nov_17_closed = ifelse(hire_year == 2017 & hire_month == "Sep", NA, nov_17_closed),
         nov_17_closed = ifelse(hire_year == 2017 & hire_month == "Oct", NA, nov_17_closed),
         dec_17_closed = ifelse(hire_year == 2017 & hire_month == "Oct", NA, dec_17_closed),
         dec_17_closed = ifelse(hire_year == 2017 & hire_month == "Nov", NA, dec_17_closed),
         jan_18_closed = ifelse(hire_year == 2017 & hire_month == "Nov", NA, jan_18_closed),
         jan_18_closed = ifelse(hire_year == 2017 & hire_month == "Dec", NA, jan_18_closed),
         feb_18_closed = ifelse(hire_year == 2017 & hire_month == "Dec", NA, feb_18_closed),
         feb_18_closed = ifelse(hire_year == 2018 & hire_month == "Jan", NA, feb_18_closed),
         mar_18_closed = ifelse(hire_year == 2018 & hire_month == "Jan", NA, mar_18_closed),
         mar_18_closed = ifelse(hire_year == 2018 & hire_month == "Feb", NA, mar_18_closed),
         apr_18_closed = ifelse(hire_year == 2018 & hire_month == "Feb", NA, apr_18_closed),
         apr_18_closed = ifelse(hire_year == 2018 & hire_month == "Mar", NA, apr_18_closed),
         may_18_closed = ifelse(hire_year == 2018 & hire_month == "Mar", NA, may_18_closed),
         may_18_closed = ifelse(hire_year == 2018 & hire_month == "Apr", NA, may_18_closed),
         jun_18_closed = ifelse(hire_year == 2018 & hire_month == "Apr", NA, jun_18_closed))
         # jun_18_closed = ifelse(hire_year == 2018 & hire_month == "May", NA, jun_18_closed),
         # jul_18_closed = ifelse(hire_year == 2018 & hire_month == "May", NA, jul_18_closed),
         # jul_18_closed = ifelse(hire_year == 2018 & hire_month == "Jun", NA, jul_18_closed),

# Step 5: Data file summary -----------

# Back to original dataset for summary slide
mean(raw$days_employed)
range(raw$fit_index, na.rm = TRUE)
table(raw$recommendation_category)

# Step 6: Descriptives -----------

# Demographics
## Ethnicity
data_eda %>% 
  mutate(ethnicity_clean = ifelse(is.na(ethnicity_clean), "Ethnicity Unknown", ethnicity_clean)) %>% 
  group_by(ethnicity_clean) %>% 
  count() %>% 
  ggplot(aes(reorder(ethnicity_clean, nn), nn)) +
  geom_col(fill = "#D51F27", width = .75) +
  geom_text(aes(label=nn), hjust=-.35, size = 3) +
  coord_flip() + 
  scale_y_continuous(expand = c(0,0), limits = c(0,175)) +
  theme_classic() +
  labs(x = "", y = "") + 
  theme(panel.background = element_rect(fill = "#F2F2F2"),
        plot.background = element_rect(fill = "#F2F2F2", color = "#F2F2F2")) +
  NULL

## Gender ## NEED TO FIGURE OUT
data_eda %>% 
  # mutate(gender = ifelse(is.na(ethnicity_clean), "Ethnicity Unknown", ethnicity_clean)) %>% 
  group_by(gender) %>% 
  summarise (n = n()) %>%
  mutate(perc = n / sum(n)) %>% 
  ggplot(aes(x = gender, y = perc, fill = gender)) +
  geom_bar(stat = "identity", position = stacked) +
  geom_text(aes(label=n), vjust=-.35) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1), labels = scales::percent) +
  theme_classic() +
  labs(x = "", y = "") + 
  theme(panel.background = element_rect(fill = "#F2F2F2"),
        plot.background = element_rect(fill = "#F2F2F2", color = "#F2F2F2")) +
  coord_flip()

## Age distribution
data_eda %>% 
  ggplot(aes(age_clean)) +
  theme_classic() +
  geom_histogram(bins = 20, color = "white", fill = "#D51F27") +
  scale_x_continuous(breaks = seq(0,70,10), limits = c(0,70)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "", y = NULL) + 
  theme(panel.background = element_rect(fill = "#F2F2F2"),
        plot.background = element_rect(fill = "#F2F2F2", color = "#F2F2F2"))

# Assessment

## Fit Index
data_eda %>% 
  ggplot(aes(fit_index)) +
  theme_classic() +
  geom_histogram(bins = 20, color = "white", fill = "#D51F27") +
  geom_vline(xintercept = mean(data_eda$fit_index), color = "black", size = 1, alpha = .55) +
  scale_x_continuous(breaks = seq(0,110,10), limits = c(0,100)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Fit Index", y = NULL) + 
  theme(panel.background = element_rect(fill = "#F2F2F2"),
        plot.background = element_rect(fill = "#F2F2F2", color = "#F2F2F2"))

mean(data_eda$fit_index)


## Recommendation
data_eda %>% 
  group_by(recommendation_category) %>% 
  summarize(rec_mean = mean(fit_index),
            n = n()) %>% 
  ggplot(aes(recommendation_category, rec_mean)) +
  theme_classic() +
  geom_point(col="#D51F27", size=5) +   # Draw points
  geom_segment(aes(x=recommendation_category, 
                   xend=recommendation_category, 
                   y=min(0), 
                   yend=max(100)), 
               linetype="dashed", 
               size=0.1) +  
  geom_text(aes(label = n), vjust = -1, size = 5) +
  scale_x_discrete(limits = c("REC", "RQ", "RR", "NR")) +
  scale_y_continuous(limits = c(0,100)) +
  labs(y = "Fit Index", x = "") +
  theme(panel.background = element_rect(fill = "#F2F2F2"),
        plot.background = element_rect(fill = "#F2F2F2", color = "#F2F2F2")) +
  coord_flip()


# Performance

# Correlation

  data_eda %>% 
  select(oct_17_closed:jul_18_closed) %>% 
  corr.test(., use = "pairwise")
t <- as_tibble(t[["r"]])
n <- as_tibble(t[["n"]])
write_csv(t, "corr_matr_perf.csv")
# ggcorrplot(t, hc.order = TRUE, type = "upper", lab = TRUE,
#            outline.col = "white",
#            ggtheme = ggplot2::theme_classic,
#            colors = c("gray", "white", "#D51F27")) +
#   theme(panel.background = element_rect(fill = "#F2F2F2"),
#         plot.background = element_rect(fill = "#F2F2F2", color = "#F2F2F2"))

# Plot
perf_count <- data_eda %>% 
  select(oct_17_closed:jul_18_closed) %>% 
  mutate(oct_17_tally = if_else(oct_17_closed > 0,1,0),
         nov_17_tally = if_else(nov_17_closed > 0,1,0),
         dec_17_tally = if_else(dec_17_closed > 0,1,0),
         jan_18_tally = if_else(jan_18_closed > 0,1,0),
         feb_18_tally = if_else(feb_18_closed > 0,1,0),
         mar_18_tally = if_else(mar_18_closed > 0,1,0),
         apr_18_tally = if_else(apr_18_closed > 0,1,0),
         may_18_tally = if_else(may_18_closed > 0,1,0),
         jun_18_tally = if_else(jun_18_closed > 0,1,0),
         jul_18_tally = if_else(jul_18_closed > 0,1,0)) %>% 
  select(oct_17_tally:jul_18_tally)



t <- tibble(colSums(perf_count, na.rm = TRUE),
               month = c("Oct 2017",
                         "Nov 2017",
                         "Dec 2017",
                         "Jan 2018",
                         "Feb 2018",
                         "Mar 2018",
                         "Apr 2018",
                         "May 2018",
                         "Jun 2018",
                         "Jul 2018")) %>% 
  rownames_to_column() 

rm(corr, data, n, perf_count, raw, rcor, rn, t)

# Step 7: Analyses -------------

# Performance Modeling

# First investigating how an overall avg variable works
## Overall Avg is straight average all available perf indicator months (first 2 still removed)
analys_data <-
data_eda %>%  
  select(oct_17_closed:jul_18_closed) 
data_eda$perf_overall_avg <- rowMeans(analys_data, na.rm = TRUE)

# Not awful; r = .19
data_eda %>% 
  select(perf_overall_avg, fit_index) %>% 
  corr.test(., use = "pairwise")

  

