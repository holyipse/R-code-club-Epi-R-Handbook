# Lesson 4: Chapter 15 in The Epidemiologist R Handbook

#### 15.1 Preparation ####
# p_load doesn't show all the (unnecessary) warnings that I get if I use "library"
pacman::p_load(tidyverse, tigris)

# where is this project located?
here::here()

# import data
co.pop <- readRDS(here::here("Data", "pop_new.rds"))

# how is the data structured
head(co.pop)

#### 15.2 Deduplication ####

# this will tabulate how many counties are in each state, by year
co.pop %>% 
  janitor::tabyl(STATE, YEAR)

# how is this data structured
head(tigris::fips_codes)

# remove county data so that data set contains duplicate records
states <- tigris::fips_codes %>%
  select(state, state_code, state_name)

# use "get_dupes" from the janitor package to see how many counties per state
states %>%
  filter(row_number() %in% c(1:10)) %>%
  janitor::get_dupes()

# or specify columns to exclude from "get_dupes"
tigris::fips_codes %>%
  filter(row_number() %in% c(1:10)) %>%
  janitor::get_dupes(-c(county_code, county))

# if we want a reference table that shows names associated with each state FIPS code
states %>%
  distinct()

# or base r alternative
states[!duplicated(states),]

#### 15.3 Slicing ####

# or use "slice" function with "group_by"
tigris::fips_codes %>%
  group_by(state, state_code, state_name) %>%
  slice(1)

# I wanted to drop the count_code and county variables
states %>%
  group_by(state, state_code, state_name) %>%
  slice(1)

# changing the ordering of the grouping, changes the sorting
states %>%
  group_by(state_code, state_name, state) %>%
  slice(1)

# use "get_dupes" to see how many counties are associated with each state
states %>%
  janitor::get_dupes() %>%
  group_by(state_code, state_name, state) %>%
  slice(1)

# use "get_dupes" to see how many counties are associated with each state
states %>%
  janitor::get_dupes() %>%
  group_by(state_code, state_name, state) %>%
  slice_head()

# what if I want to see the highest county FIPS code for each state
tigris::fips_codes %>%
  group_by(state_code, state_name, state) %>%
  slice_tail()

#### 15.4 Roll-up values ####

# roll-up values
pop_rolled <- co.pop %>%
  group_by(STATE, COUNTY, FIPSCODE, AREANAME) %>%
  arrange(YEAR, .by_group=TRUE) %>%
  summarise(across(POP, ~paste0(., collapse = "; ")))
head(pop_rolled)

# adding the suffix "roll" to the new column
pop_rolled <- co.pop %>%
  group_by(STATE, COUNTY, FIPSCODE, AREANAME) %>%
  arrange(YEAR, .by_group=TRUE) %>%
  summarise(across(POP, list(roll = ~paste0(., collapse = "; "))))

#### 15.3 Row completeness

# import data
linelist <- readRDS(here::here("Data", "linelist_cleaned.rds")) %>%
  select(case_id, date_hospitalisation, gender, age_years, hospital, fever, chills, cough, aches, vomit)

# how is the data structured
head(linelist)

# using rowSums to see how many symptoms
sx_cols = c("fever", "chills", "cough", "aches", "vomit")
linelist %>%
  filter(row_number() %in% c(1:10)) %>%
  mutate(sx_complete = rowSums(!is.na(.[,sx_cols]))/length(sx_cols))

# store variables in a new data set called sx
sx <- linelist %>%
  mutate(sx_complete = rowSums(!is.na(.[,sx_cols]))/length(sx_cols))

# are any of the symptoms for an individual partially complete?
table(sx$sx_complete)
