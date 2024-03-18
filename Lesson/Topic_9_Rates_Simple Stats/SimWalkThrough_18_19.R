library(tidyverse)
library(PHEindicatormethods)
library(rstatix)
library(glue)
  
###############################################
############## Random Data Gen ################
###############################################
# Set seed for reproducibility
set.seed(123)

# Define demographic types and their rates for each clinic
all_demographics <- c("Type1", "Type2", "Type3")
all_clinics <- 1:4

# rates can be pulled from their position
clinic_rates <- matrix(c(
  0.1, 0.2, 0.3,  # Clinic 1
  0.2, 0.3, 0.4,  # Clinic 2
  0.3, 0.4, 0.5,  # Clinic 3
  0.4, 0.5, 0.6   # Clinic 4
), nrow = 4, byrow = TRUE)

# Number of samples to generate
n_samples <- 1000

generate_random_outcome <- function(clinics=all_clinics,
                                    demographics=all_demographics,
                                    rates=clinic_rates){
  clinic_idx <- sample(length(clinics),size=1)
  demographic_idx <- sample(length(demographics),size=1)
  
  current_clinic <- clinics[clinic_idx]
  current_demographic <- demographics[demographic_idx]
  
  rate <- clinic_rates[clinic_idx,demographic_idx]
  
  current_time_to_visit <- 1/rate * rnorm(1,mean=3,sd=1)
  current_outcome <-rbinom(n=1, size = 1, prob = rate)
  
  data.frame(clinic=current_clinic,
             demographic=current_demographic,
             outcome=current_outcome,
             time_to_visit=current_time_to_visit)
  
}

# simulating data points and creating a dataframe from it
clinics_sample <- bind_rows(replicate(n_samples, generate_random_outcome(),simplify=F))
########################################

# group by and summarise by counting total succeses, total visits, and success rates
clinic_successes<-clinics_sample %>%
  group_by(clinic,demographic) %>%
  summarise(visits=n(),successes=sum(outcome),
            success_rate=successes/visits,
            time_to_visit=mean(time_to_visit))

# check the result
# note the unequal number of visits per category
print(clinic_successes)

# add a colomn to change the standardized visit count for all rows to 100 visits
# because there is an inbalance in the number of visits by clinic and demographic
clinic_successes <- clinic_successes %>%
  mutate(standardized_visits=100)


# this becomes the multiplier for the rate:
# changes the interpretation to successes per 100 visits
per_n_visits <- 100

# unstandardized clinic success rates
clinic_unstandardized <- clinic_successes %>%
  group_by(clinic) %>%
  summarise(x=sum(successes),n=sum(visits), rate=x/n*per_n_visits) 

# standardized clinic success rates
# creates a new dataframe with confidence intervals
clinic_standardized <-   phe_dsr(data=clinic_successes %>% group_by(clinic),
                                 x=successes,
                                 n=visits,
                                 stdpop = standardized_visits,
                                 stdpoptype = "field",
                                 type = "full",
                                 confidence = 0.95,
                                 multiplier = per_n_visits
                                 )
print(clinic_standardized)

# filter and adjust the column names in case you want more descriptive names
clinic_standardized_prepped <- clinic_standardized %>%
  select(clinic,
         `Total Count`=total_count,
         `Total Population`=total_pop,
         `Standardized Rate`=value,
         `95% Lower Confidence Limit`=lowercl,
         `95% Upper Confidence Limit`=uppercl)

# join on clinic, it will find common columns if you don't specify
clinic_rates_and_standardization<-full_join(clinic_unstandardized,clinic_standardized_prepped)
print(clinic_rates_and_standardization)

#### Indirect Standardization ####

# Here are the following references that we'll use separately
# in different indirectly standardized rates
reference_demographic <- clinic_successes %>%
  filter(demographic == 'Type3') %>%
  select(clinic,
         reference_demographic=demographic,
         reference_successes=successes,
         reference_visits=visits)

reference_clinic <- clinic_successes %>%
  filter(clinic == 4) %>%
  select(reference_clinic=clinic,
         demographic,
         reference_successes=successes,
         reference_visits=visits)


### calculate_ISRate
# start out by assuming similar #'s of visits per demographic as clinic 4
# and calculate observed/expected rates for clinic 4 within each level
# expected - what we would expect the successes to be if the clinic had
# equal demographics a clinic 4
# observed - how many successes we actually observed
clinic_successes %>%
  # add reference_successes by clinic 4 demographic
  left_join(reference_clinic,by='demographic') %>% 
    group_by(clinic,demographic) %>%
  PHEindicatormethods::calculate_ISRate(
    x = successes,                 # column with observed number of successes
    n = visits,             # column with non-standard visits for each demographic per clinic
    refpoptype='field',         # specify that the reference demographic comes from the data
    x_ref = reference_successes,  # reference number of successes for each demographic per clinic
    n_ref = reference_visits,   # reference visits for each demographic per clinic
    multiplier=1
    )
### calculate_ISRatio
# sometimes we'd want to calculate and compare rates as ratios of
# the expected rates:   (successes/observed)   / (ref_successes/ref_observed)
# value in this case is the rate ratio instead of the rate
is_ratio <- clinic_successes %>%
  left_join(reference_clinic,by='demographic') %>%
  group_by(clinic,demographic) %>%
  PHEindicatormethods::calculate_ISRatio(
    x = successes,                 # column with observed number of successes
    n = visits,             # column with non-standard visits for each stratum
    refpoptype='field',         # specify that the reference demographic comes from the data
    x_ref = reference_successes,  # reference number of successes for each stratum
    n_ref = reference_visits   # reference visits for each stratum
    )

###### Quick Chi Squared Goodness of Fit Test #########
# assumptions: 1) random sample, 2)  independence of samples,
# 3) all observed frequencies > 5  4) the data contains frequencies 
# note: generally, violations = inaccurate results
# results of statistics depend on: 1) degrees of  freedom, and test statistic (calculated above)
# formula: for every row (i)
# sum(((observed - expected)^2) / expected)

##### By Hand #####

## testing significance
# steps:
# calculate test statistic
# calculate degrees of freedom
# use a chart or programming language to check whether signifannt

# selecting relevant columns 

clinic_demographic_successes<-is_ratio %>%
  select(clinic,demographic, observed,expected,value) %>%
  ungroup()

#### step 1 ####
#method 1:
# transforms observed and expected counts to wide format


# calculate observed rates, to wide format
# moving observed counts in 3x4  grid
observed<-clinic_demographic_successes %>%
  select(clinic,demographic,observed) %>%
  pivot_wider(names_from=clinic,
              values_from=observed) %>%
  select(-demographic) # remove the uneeded column

print(observed) # make sure all cells over 5 observations

# calculate expected assuming similar population sizes as clinic 4
# moving expected counts in 3x4  grid
expected<-clinic_demographic_successes %>%
  select(clinic,demographic,expected) %>%
  pivot_wider(names_from=clinic, values_from=expected)  %>%
  select(-demographic) # remove the uneeded column

print(expected)

# uses subtracts observed counts in each cell from expected counts in each cell
cell_diferences <- observed - expected
# remember, this is comparing to clinic 4 and seeing whether rates differ
# from what is expected based on clinic 4 rates
print(cell_diferences) 
# the actual difference calculation, divide by 2 after squaring differences
# then sum
chi_squared_test_statistic <- sum((cell_diferences)^2 / expected)

#### quicker method 2 ####

# keeps data in same format for chi square test statistic calculation
chi_squared_test_statistic<-clinic_demographic_successes %>%
  mutate(difference=(observed-expected)^2/expected) %>%
  pull(difference) %>% sum()

### degrees of freedom ###
# the amount of leway the data has to vary
# the higher the sample size, the more we're likely to say that there is a significant result
# assuming there actually is a significant difference in the population

# formula: number of rows/levels for var 1 minus 1 times the number of columns/levels for var 2 minus 1 
# quick way of calculating number of rows and columns if not pivoted

### same result ###
#n_col = ncol(observed)
#n_row = nrow(observed)

n_row = n_distinct(clinic_demographic_successes$demographic)
n_col = n_distinct(clinic_demographic_successes$clinic)
degrees_freedom <-  (n_row - 1)*(n_col - 1)

# H0 - the null hypothesis states that there is no relationship between clinic and demographic
# p gives the probability of observing the result assuming that there's no difference
# between observed and expected counts:
p_value <- 1 - pchisq(chi_squared_test_statistic, degrees_freedom)

# we either reject it if the likelihood of seeing the observed difference is less than 5% (p value)
# when assuming that the null hypothesis true  or we fail to reject otherwise:
decision <- if(p_value < .05) 'reject' else 'fail to reject'

# the result is shown below:
writeLines(glue('p value: {p_value}, chi_sq: {chi_squared_test_statistic}, decision on H0: {decision}'))
