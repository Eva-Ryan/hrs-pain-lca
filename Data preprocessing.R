# Code to load and prepare 2016 HRS pain data
#------------------------------------------------------------------------------

# load packages
library(tidyverse)
library(haven)

# get location of project directory
directory <- getwd()

# load data from 2016 RAND fatfile
rand16 <- read_dta(paste0(directory, "/RAND data/h16f2c.dta"))

# prepare data
rand16 <- rand16 %>%
  # select relevant variables
  select(hhid, pn, pc104, pc105, pc106, pc287, pc288, pc146) %>%
  # create unique id hhidpn and remove household and person numbers
  mutate(hhidpn = paste0(hhid, pn)) %>%
  select(-hhid, -pn)

# give variables more descriptive names
names(rand16) <- c("troubledWithPain", "painUsualSeverity",
                   "painPreventActivity", "painMeds", "painOpioids",
                   "backPain", "hhidpn")


