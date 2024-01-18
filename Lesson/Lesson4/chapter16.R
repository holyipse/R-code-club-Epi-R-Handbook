# Lesson 4: Chapter 16 in The Epidemiologist R Handbook

#### 16.1 Preparation ####

pacman::p_load(tidyverse, janitor, tigris)

#### Review Chapter 15 on de-duplication ####

# remove county data so that data set contains duplicate records
states <- tigris::fips_codes %>%
  select(state, state_code, state_name)

# use "get_dupes" to see how many counties are associated with each state
states %>%
  janitor::get_dupes() %>%
  group_by(state_code, state_name, state) %>%
  slice_head()

# answer to question about warning in console
states %>%
  janitor::get_dupes(state_code, state_name, state) %>%
  group_by(state_code, state_name, state) %>%
  slice_head()

#### 16.2 for loops ####

# list with one data set for each year
NETSS.list <- vector(mode = "list", length=2)

# read data sets into a list using a for loop
for (i in c(1:2)) {
  y <- 2019+i
  NETSS.list[[i]] <- readRDS(here::here("Data", paste0("NETSS",y,".rds"))) %>%
    mutate(Disease = case_when(EVENT == 10274 ~ "Chlamydia",
                               EVENT == 10280 ~ "Gonorrhea"))
}

# printing from within a for loop
for (i in c(1:2)) print(dim(NETSS.list[[i]]))

# plotting from within a for loop
for (i in c(1:2)) {
  y <- 2019 + i
  plot_cases <- ggplot(NETSS.list[[i]]) +
    geom_col(aes(Disease,CASES)) +
    labs(title = paste("NNDSS cases in", y))
  print(plot_cases)
}

# use "reduce" function with argument "bind_rows" to convert list to data frame
NETSS <- reduce(NETSS.list, bind_rows)

#### 16.3 purrr and lists ####

# review from import chapter
list.files(here::here("Data"), full.names=TRUE)

# TRUE/FALSE if file name contains "NETSS"
i <- list.files(here::here("Data"), full.names=TRUE) %>%
  str_detect("NETSS")

# a vector with the file names that contain "NETSS"
file_names <- list.files(here::here("Data"), full.names=TRUE)[i]

# an alternative to using for loop to read files
combined <- file_names %>%
  map(.f = ~readRDS(.))

# convert list to data frame
combined <- file_names %>%
  map(.f = ~readRDS(.)) %>%
  bind_rows

# alternatively, use map_dfr to create a data frame
combined <- file_names %>%
  map_dfr(.f = ~readRDS(.))


# combined <- list.files(here::here("Data"), full.names=TRUE) %>%
#   map_if(.f = ~readRDS(.),
#         .p = str_detect(., pattern = "NETSS")) %>%
#   compact() 

# alternatively, use keep to select files to read
combined <- list.files(here::here("Data"), full.names=TRUE) %>%
  keep(.p = str_detect(., pattern = "NETSS")) %>%
  map_dfr(.f = ~readRDS(.))

# split into multiple data frames that are stored in a list
NETSS_split <- combined %>%
  group_by(EVENT) %>%
  group_split()

# use map function to print dimensions of the 2 data frames in the list
NETSS_split %>%
  map(.f = ~dim(.))

# use lapply to print dimmensions of the 2 data frames in the list
lapply(NETSS_split, dim)

# use map function to print a subset of each data frame
NETSS_split %>%
  map(.f = ~head(.))

# use lapply to print a subset of each data frame
lapply(NETSS_split, head)

# write a function to use with "map"
make_table <- function(item){
  item %>%
    group_by(YEAR, EVENT) %>%
    summarise(CASES = sum(CASES, na.rm=TRUE))
}

# call the function for each element in the list
NETSS_split %>%
  map(.f = ~make_table(.))

# use lapply to make a table for each disease
lapply(NETSS_split, make_table) 

# create a data frame from the output of the function
NETSS_split %>%
  map(.f = ~make_table(.)) %>%
  bind_rows

# create a data frame from the output of the function
NETSS_split %>%
  map_dfr(.f = ~make_table(.))

# create a data frame form the output of the function
lapply(NETSS_split, make_table) %>%
  bind_rows()
