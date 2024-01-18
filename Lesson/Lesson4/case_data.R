# create data sets for example
pacman::p_load(tidyverse)

frozen <- 
  "//cdc/project/NCHHSTP_DSTD_SurvReports/surv21_Share/Tables/Frozen/R/"


county2020 <- readRDS(paste0(frozen,"netss2020.rds")) %>% 
  select(YEAR, STATE, EVENT, COUNTY) %>%
  filter(STATE <= 56, EVENT %in% c(10274, 10280)) %>%
  group_by(YEAR, EVENT, STATE, COUNTY) %>%
  summarise(CASES=n())

county2021 <- readRDS(paste0(frozen,"netss2021.rds")) %>% 
  select(YEAR, STATE, EVENT, COUNTY) %>%
  filter(STATE <= 56, EVENT %in% c(10274, 10280), STATE != 24) %>%
  group_by(YEAR, EVENT, STATE, COUNTY) %>%
  summarise(CASES=n())


saveRDS(county2020, file=here::here("Data","NETSS2020.rds"))
saveRDS(county2021, file=here::here("Data","NETSS2021.rds"))
