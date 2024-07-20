# Code to load and prepare 2016 HRS pain data
#------------------------------------------------------------------------------

# load packages
library(tidyverse)
library(haven)

# get location of project directory
directory <- getwd()


#--------------------------------
# LOAD DATA AND MERGE

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

# load data from RAND tracker file
tracker <- read_dta(paste0(directory, "/Tracker file data/trk2020tr_r.dta"))

# prepare data
tracker <- tracker %>%
  # select just the relevant variables (id, main status, sociodemographics,
  # first interview and death month/year)
  select(HHID, PN, PRESCODE, PWGTR, PALIVE, GENDER, BIRTHYR, HISPANIC, RACE,
         DEGREE, EXDEATHMO, EXDEATHYR, FIRSTIW, FWHY0WGT) %>%
  # create unique id hhidpn and remove household and person numbers
  mutate(hhidpn = paste0(HHID, PN)) %>%
  select(-HHID, -PN)

# rename
names(tracker) <- c("prescode", "sample_wgt", "palive", "gender", "birthYear",
                    "hispanic", "race", "degree", "monthOfDeath", "yearOfDeath",
                    "yrOf1stIntvw", "fwhy0wgt", "hhidpn")

# merge tracker data into 2016 fatfile data
df <- left_join(rand16,
                tracker,
                by = "hhidpn")


#--------------------------------
# DATA CLEANING

# the "prescode" variable records what kind of interview was conducted in 2016
# check what values are recorded
summary(as.factor(df$prescode)) # just 1001 = complete and 1005 = partial

# recode so 1 = complete interview and 0 = partial interview
df <- df %>% mutate(prescode = car::recode(prescode,
                                           "1001 = 1;
                                           1005 = 0"))

# the "palive" variable records vital status at the 2016 wave
summary(as.factor(df$palive)) # all alive

# check gender variable
summary(as.factor(df$gender)) # no missing data

# check birth year variable
summary(as.factor(df$birthYear)) # no missing data

# create age variable
df <- df %>%
  mutate(age = 2016 - birthYear)

# check race variables
summary(as.factor(df$hispanic))
summary(as.factor(df$race))

# first create a variable where 1 = Hispanic, 0 = non-Hispanic
df <- df %>%
  # pool type-unknown, Mexican, and other Hispanic (0-2)
  mutate(hispTemp = car::recode(hispanic,
                                 "0 = NA;
                                 1 = 1;
                                 2 = 1;
                                 3 = 1;
                                 5 = 0"))

# create new race variable
df <- df %>%
  mutate(race4Cats = car::recode(race,
                                 "0 = NA;
                                 1 = 1;
                                 2 = 2;
                                 7 = 4"))

# add Hispanic
df$race4Cats[df$hispTemp == 1] <- 3

# check education variable
summary(as.factor(df$degree))

# recode "unknown degree" to NA
df <- df %>%
  mutate(degree = car::recode(degree, "9 = NA"))

# create new education category variable
df <- df %>%
  mutate(edu4Cats = car::recode(degree,
                                "0 = 1;
                                1 = 2;
                                2 = 2;
                                3 = 2;
                                4 = 3;
                                5 = 4;
                                6 = 4"))

# remove redundant variables
df <- df %>%
  select(-hispanic, -race, -hispTemp, -degree)

# select just those aged 51+ in 2016 who do not have zero weights
df <- df %>%
  filter(age >= 51) %>%
  filter(sample_wgt != 0)


#--------------------------------
# EXPORT FINAL CLEANED DATASET

# export data as a csv file
write_csv(df, paste0(directory, "/HRS_2016_pain.csv"))
