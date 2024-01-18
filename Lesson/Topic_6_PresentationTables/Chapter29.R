# p_load doesn't show all the (unnecessary) warnings that I get if I use "library"
pacman::p_load(tidyverse)

# import data
linelist <- readRDS(here::here("Data", "linelist_cleaned.rds"))

# variables in analysis
linelist %>%
  janitor::tabyl(hospital, outcome)

# number of records with hospital="Missing" and missing outcome
linelist %>%
  select(hospital, outcome, ct_blood) %>%
  mutate(hosp = ifelse(hospital != "Missing", hospital, NA),
         both_values = ifelse(is.na(hosp) | is.na(outcome),NA, "yes")) %>%
  janitor::tabyl(both_values, outcome)

# copied code from chapter 29 to create table for presentation
table <- linelist %>% 
  
  # Get summary values per hospital-outcome group
  ###############################################
group_by(hospital, outcome) %>%                      # Group data
  summarise(                                           # Create new summary columns of indicators of interest
    N = n(),                                            # Number of rows per hospital-outcome group     
    ct_value = median(ct_blood, na.rm=T)) %>%           # median CT value per group
  
  # add totals
  ############
bind_rows(                                           # Bind the previous table with this mini-table of totals
  linelist %>% 
    filter(!is.na(outcome) & hospital != "Missing") %>%
    group_by(outcome) %>%                            # Grouped only by outcome, not by hospital    
    summarise(
      N = n(),                                       # Number of rows for whole dataset     
      ct_value = median(ct_blood, na.rm=T))) %>%     # Median CT for whole dataset
  
  # Pivot wider and format
  ########################
mutate(hospital = replace_na(hospital, "Total")) %>% 
  pivot_wider(                                         # Pivot from long to wide
    values_from = c(ct_value, N),                       # new values are from ct and count columns
    names_from = outcome) %>%                           # new column names are from outcomes
  mutate(                                              # Add new columns
    N_Known = N_Death + N_Recover,                               # number with known outcome
    Pct_Death = scales::percent(N_Death / N_Known, 0.1),         # percent cases who died (to 1 decimal)
    Pct_Recover = scales::percent(N_Recover / N_Known, 0.1)) %>% # percent who recovered (to 1 decimal)
  select(                                              # Re-order columns
    hospital, N_Known,                                   # Intro columns
    N_Recover, Pct_Recover, ct_value_Recover,            # Recovered columns
    N_Death, Pct_Death, ct_value_Death)  %>%             # Death columns
  arrange(N_Known)                                    # Arrange rows from lowest to highest (Total row at bottom)

table  # print

my_table <- flextable(table)
save_as_docx("my table" = my_table, path="file.docx")


# reference this web page https://ardata-fr.github.io/flextable-book/index.html