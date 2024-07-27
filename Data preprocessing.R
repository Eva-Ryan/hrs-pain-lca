# Code to load and prepare 2016 HRS pain data
#------------------------------------------------------------------------------

# load packages
library(tidyverse)
library(haven)

#--------------------------------
# LOAD DATA AND MERGE

# get location of project directory
directory <- getwd()


######################
# 1. 2016 RAND FATFILE

# load data from 2016 RAND fatfile
rand16 <- read_dta(paste0(directory, "/RAND data/h16f2c.dta"))

# prepare data
rand16 <- rand16 %>%
  # select relevant variables
  # select(hhid, pn, pc104, pc105, pc106, pc287, pc288, pc146) %>%
  select("hhid", "pn",
         pc104, pc105, pc106, pc287, pc288, pc146, pc018, pc020, pc019, pc023,
         pc024, pc028, pc010, pc030, pc036, pc045, pc053, pc070,
         pc139, pc141, pc142, pb063, pj005m1, pq415, pi841, pi834) %>%
  # create unique id hhidpn and remove household and person numbers
  mutate(hhidpn = paste0(hhid, pn)) %>%
  select(-hhid, -pn)

# give variables more descriptive names
#names(rand16) <- c("troubledWithPain", "painUsualSeverity",
#                   "painPreventActivity", "painMeds", "painOpioids",
#                   "backPain", "hhidpn")

names(rand16) <- c("troubledWithPain", "painUsualSeverity", "painPreventActivity",
                   "painMeds", "painOpioids", "backPain", "cancerEver",
                   "cancerTrtmtLast2Yrs", "cancerSeenDocLast2Yrs",
                   "cancerBetterSameWorse", "cancerNewSinceLastWave",
                   "cancerYearMostRecent", "diabetes", "lungDis", "hrtCond",
                   "angina", "stroke", "arthritis",
                   "weight", "htFeet", "htInches", "maritalStatus", "jobStatus",
                   "foodInsecurity", "measuredWgt", "measuredHgt",
                   "hhidpn")

### Data for BMI

# create function to extract height/weight data for 2002 wave onwards as
#  Wave 2002 - 2018 used the same naming conventions
bmi_data_fun <- function(file_path_name, alphabet, wave){
  # Inputs: file_path_name = path to file to RAND file to be loaded in
  #         alphabet = letter that the pain variable names start with
  #         wave = wave year (numeric)
  # Output: df = reduced dataframe containing just ID and pain variables for the
  #              wave, with wave specific suffixes

  # read in RAND data
  df_full <- read_dta(file_path_name)

  # create vector of variable names
  var_names <- c("weight", "htFeet", "htInches")

  # prepare data
  df <- df_full %>%
    # select relevant variables (NOTE: in same order as names in var_names)
    select(hhid, pn,
           paste0(alphabet, "c139"), paste0(alphabet, "c141"),
           paste0(alphabet, "c142")) %>%
    # create unique id hhidpn and remove household and person numbers
    mutate(hhidpn = paste0(hhid, pn)) %>%
    select(-hhid, -pn)

  # make variable names more descriptive and add wave year (except for id)
  names(df)[-ncol(df)] <- paste0(var_names, wave)

  return(df)
}

# 2018 - Wave 14
w2018 <- bmi_data_fun(paste0(directory, "/RAND data/h18f2b.dta"), "q", 2018)
# 2014 - Wave 12
w2014 <- bmi_data_fun(paste0(directory, "/RAND data/h14f2b.dta"), "o", 2014)
# 2012 - Wave 11
w2012 <- bmi_data_fun(paste0(directory, "/RAND data/h12f3a.dta"), "n", 2012)
# 2010 - Wave 10
w2010 <- bmi_data_fun(paste0(directory, "/RAND data/hd10f6a.dta"), "m", 2010)
# 2008 - Wave 9
w2008 <- bmi_data_fun(paste0(directory, "/RAND data/h08f3a.dta"), "l", 2008)
# 2006 - Wave 8
w2006 <- bmi_data_fun(paste0(directory, "/RAND data/h06f4a.dta"), "k", 2006)
# 2004 - Wave 7
w2004 <- bmi_data_fun(paste0(directory, "/RAND data/h04f1c.dta"), "j", 2004)
# 2002 - Wave 6
w2002 <- bmi_data_fun(paste0(directory, "/RAND data/h02f2c.dta"), "h", 2002)

# load in 1992-2000 waves separately as they all use different naming
# conventions for height and weight data
w2000 <- read_dta(paste0(directory, "/RAND data/h00f1d.dta")) %>%
  select(hhid, pn, g1425, g1428, g1429) %>%
  mutate(hhidpn = paste0(hhid, pn)) %>%
  select(-hhid, -pn)

w1998 <- read_dta(paste0(directory, "/RAND data/hd98f2c.dta")) %>%
  select(hhid, pn, f1291, f1295, f1296) %>%
  mutate(hhidpn = paste0(hhid, pn)) %>%
  select(-hhid, -pn)

w1996 <- read_dta(paste0(directory, "/RAND data/h96f4a.dta")) %>%
  select(hhid, pn, e954, e958, e959) %>%
  mutate(hhidpn = paste0(hhid, pn)) %>%
  select(-hhid, -pn)

w1995 <- read_dta(paste0(directory, "/RAND data/ad95f2b.dta")) %>%
  select(hhid, pn, d954, d958, d959) %>%
  mutate(hhidpn = paste0(hhid, pn)) %>%
  select(-hhid, -pn)

w1994 <- read_dta(paste0(directory, "/RAND data/h94f1a.dta")) %>%
  select(hhid, pn, w462, w463, w464) %>%
  mutate(hhidpn = paste0(hhid, pn)) %>%
  select(-hhid, -pn)

w1993 <- read_dta(paste0(directory, "/RAND data/ad93f2a.dta")) %>%
  select(hhid, pn, b304, b306) %>% # height only measured in inches
  mutate(hhidpn = paste0(hhid, pn)) %>%
  select(-hhid, -pn)

w1992 <- read_dta(paste0(directory, "/RAND data/hd92f1b.dta")) %>%
  select(hhid, pn, v515, v517, v518) %>%
  mutate(hhidpn = paste0(hhid, pn)) %>%
  select(-hhid, -pn)

# align with naming convention
var_names <- c("weight", "htFeet", "htInches")
names(w2000)[-ncol(w2000)] <- paste0(var_names, 2000)
names(w1998)[-ncol(w1998)] <- paste0(var_names, 1998)
names(w1996)[-ncol(w1996)] <- paste0(var_names, 1996)
names(w1995)[-ncol(w1995)] <- paste0(var_names, 1995)
names(w1994)[-ncol(w1994)] <- paste0(var_names, 1994)
names(w1993)[-ncol(w1993)] <- c("weight1993", "htInches1993")
names(w1992)[-ncol(w1992)] <- paste0(var_names, 1992)

#---------------------------------
# Merge all datasets together

# add heights and weights for all years to the 2016 dataset
rand16 <- left_join(rand16,
                    w1992,
                    by = "hhidpn")
rand16 <- left_join(rand16,
                    w1993,
                    by = "hhidpn")
rand16 <- left_join(rand16,
                    w1994,
                    by = "hhidpn")
rand16 <- left_join(rand16,
                    w1995,
                    by = "hhidpn")
rand16 <- left_join(rand16,
                    w1996,
                    by = "hhidpn")
rand16 <- left_join(rand16,
                    w1998,
                    by = "hhidpn")
rand16 <- left_join(rand16,
                    w2000,
                    by = "hhidpn")
rand16 <- left_join(rand16,
                    w2002,
                    by = "hhidpn")
rand16 <- left_join(rand16,
                    w2004,
                    by = "hhidpn")
rand16 <- left_join(rand16,
                    w2006,
                    by = "hhidpn")
rand16 <- left_join(rand16,
                    w2008,
                    by = "hhidpn")
rand16 <- left_join(rand16,
                    w2010,
                    by = "hhidpn")
rand16 <- left_join(rand16,
                    w2012,
                    by = "hhidpn")
rand16 <- left_join(rand16,
                    w2014,
                    by = "hhidpn")
rand16 <- left_join(rand16,
                    w2018,
                    by = "hhidpn")


######################
# 2. TRACKER FILE

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


######################
# RAND LONGITUDINAL FILE

# read in combined Longitudinal RAND file (1992 - 2020)
long_rand <- read_dta(paste0(directory, "/RAND data/randhrs1992_2020v1.dta"))

# prepare data
long_rand <- long_rand %>%
  # select just the relevant variables
  select(hhid, pn, h13atotb,	h13hhres, h13child, r13higov, r13prpcnt,
         ravetrn, r13cesd, r13smokev, r13smoken) %>%
  # create unique id hhidpn and remove household and person numbers
  mutate(hhidpn = paste0(hhid, pn)) %>%
  select(-hhid, -pn)

# give variables more descriptive names
names(long_rand) <- c("wealth", "householdSize", "numChildren",
                      "govHealthIns", "private", "veteranStatus",
                      "randCESD", "randSmokeEver", "randSmokeNow",
                      "hhidpn")


######################
# CENSUS REGION FILE

# read in the HRS Census region file
region <- read_dta(paste0(directory, "/Census region file data/built/stata/HRSXREGION18.dta"))

# prepare data
region <- region %>%
  # select just the relevant variables
  select(region16, beale2003_16, hhid, pn) %>%
  # create unique id hhidpn and remove household and person numbers
  mutate(hhidpn = paste0(hhid, pn)) %>%
  select(-hhid, -pn)

# give variables more descriptive names
names(region) <- c("region", "urbanicity", "hhidpn")


#######################
# MERGE ALL VARIABLES

# merge tracker data into 2016 fatfile data
df1 <- left_join(rand16,
                 tracker,
                 by = "hhidpn")
# merge in data from RAND Longitudinal file
df2 <- left_join(df1,
                 long_rand,
                 by = "hhidpn")
# merge in data from Census region file
df <- left_join(df2,
                region,
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

# check race variable
summary(as.factor(df$race4Cats))

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
  filter(age >= 51) %>% # 20912 - 19929 = 983 removed
  filter(sample_wgt != 0) # 19929 - 19623 = 306 removed

# check pain variables
summary(as.factor(df$troubledWithPain))
summary(as.factor(df$painUsualSeverity))
summary(as.factor(df$painPreventActivity))
summary(as.factor(df$painMeds))
summary(as.factor(df$painOpioids))
summary(as.factor(df$backPain))

# recode pain values
df <- df %>%
  # 1 = yes, 2 = no, otherwise NA
  mutate(troubledWithPain = car::recode(troubledWithPain,
                                        "1 = 1;
                                        5 = 2;
                                        8 = NA;
                                        9 = NA")) %>%
  # 2 = mild, 3 = moderate, 4 = severe, otherwise NA
  mutate(painUsualSeverity = car::recode(painUsualSeverity,
                                         "1 = 2;
                                         2 = 3;
                                         3 = 4;
                                         8 = NA;
                                         9 = NA")) %>%
  # 1 = yes, 2 = no, otherwise NA
  mutate(painPreventActivity = car::recode(painPreventActivity,
                                           "1 = 1;
                                           5 = 2;
                                           8 = NA;
                                           9 = NA")) %>%
  # 1 = yes, 2 = no, otherwise NA
  mutate(painMeds = car::recode(painMeds,
                                "1 = 1;
                                5 = 2;
                                8 = NA;
                                9 = NA")) %>%
  # 1 = yes, 2 = no, otherwise NA
  mutate(painOpioids = car::recode(painOpioids,
                                   "1 = 1;
                                   5 = 2;
                                   7 = NA;
                                   8 = NA;
                                   9 = NA")) %>%
  # 1 = yes, 2 = no, otherwise NA
  mutate(backPain = car::recode(backPain,
                                "1 = 1;
                                5 = 2;
                                8 = NA;
                                9 = NA"))

# make new pain level variable with 1 = no pain, 2 = mild pain,
# 3 = moderate pain, 4 = severe pain
df <- df %>%
  mutate(painLevel = ifelse(troubledWithPain == 2,
                            1,
                            painUsualSeverity))

# check new variable
summary(as.factor(df$painLevel))

# recode pain prevents activity variable to be 2 (no) instead of NA for
# participants who reported no pain
df <- df %>%
  mutate(painDisability = ifelse(troubledWithPain == 2,
                                 2,
                                 painPreventActivity))

# check the new variable
summary(as.factor(df$painDisability))

# create wealth quartile categorical variable
df <- df %>%
  mutate(wealthQuarts = cut(wealth,
                            breaks = (quantile(wealth,
                                               c(0, 0.25, 0.5, 0.75, 1),
                                               na.rm = TRUE)),
                            labels = c("Q1", "Q2", "Q3", "Q4")))
summary(as.factor(df$wealthQuarts))

# create vector of factor level names for census region
census_region <- c("New England", "Mid-Atlantic", "East North Central",
                   "West North Central", "South Atlantic", "East South Central",
                   "West South Central", "Mountain", "Pacific")

# create vector of factor level names for 4 category region variable
region4 <- c("Northeast", "Mid-west", "South", "West")


# tidy up census region variable
df <- df %>%
  mutate(region = factor(
    ifelse(region >= 1 & region <= 9, region, NA),
    levels = c(1:9), labels = census_region)) %>%
  # recode to a 4 category variable
  mutate(region4Cats = factor(car::recode(region,
                                          "'New England' = 1;
                                              'Mid-Atlantic' = 1;
                                              'East North Central' = 2;
                                              'West North Central' = 2;
                                              'South Atlantic' = 3;
                                              'East South Central' = 3;
                                              'West South Central' = 3;
                                              'Mountain' = 4;
                                              'Pacific' = 4"),
                              levels = c(1:4), labels = region4))
summary(as.factor(df$region4Cats))

# create vector of factor level names for urbanicity
urban_labs <- c("Urban", "Suburban", "Ex-urban/rural")

df <- df %>%
  mutate(urbanicity = factor(
    ifelse(urbanicity >= 1 & urbanicity <= 3, urbanicity, NA),
    levels = c(1,2,3), labels = urban_labs))
summary(as.factor(df$urbanicity))

# create vector of marital status categories
marital_status <- c("Married", "Separated/Divorced", "Widowed", "Never married",
                    "Other")

# recode marital status variable
df <- df %>%
  mutate(maritalStatus = ifelse(maritalStatus >= 1 &
                                  maritalStatus <= 7,
                                maritalStatus, NA)) %>%
  mutate(maritalStatus = car::recode(maritalStatus,
                                     "1 = 1; 2 = 2; 3 = 2; 4 = 2; 5 = 3;
                                         6 = 4; 7 = 5"))
summary(as.factor(df$maritalStatus))

# household size - no cleaning needed
summary(as.factor(df$householdSize))

# number of children - no cleaning needed
summary(as.factor(df$numChildren))

# create vector of current employment categories
employment_status <- c("Employed", "Unemployed", "Retired", "Not in labour force")

# recode to a 4 category variable:
# Employed = working now
# Unemployed = unemployed and looking for work / temporarily laid off, on sick
# or other leave / disabled / homemaker / other
# Retired = retired
df <- df %>%
  mutate(jobStatus = ifelse(jobStatus >= 1 & jobStatus <= 8,
                            jobStatus, NA)) %>%
  mutate(jobStatus4Cats = factor(car::recode(jobStatus,
                                             "1 = 1;
                                                 2 = 2; 3 = 2; 7 = 2;
                                                 5 = 3;
                                                 4 = 4; 6 = 4"),
                                 levels = c(1:4), labels = employment_status))
summary(as.factor(df$jobStatus4Cats))

# Financial insecurity - food insecurity

# recode "in the last two years, have you always had enough money to buy food?"
# to 1 = yes, 0 = no
df <- df %>%
  mutate(foodInsecurity = ifelse(foodInsecurity >= 1 &
                                   foodInsecurity <= 5,
                                 foodInsecurity, NA)) %>%
  mutate(foodInsecurity = factor(car::recode(foodInsecurity,
                                             "1 = 1;
                                                 5 = 0"),
                                 levels = c(0,1), labels = c("no", "yes")))
summary(as.factor(df$foodInsecurity))

# Create a flag showing if someone was in active cancer treatment (or palliative
# care) at time of current wave
df <- df %>%
  mutate(cancerEver = ifelse(cancerEver >= 1 & cancerEver <= 5,
                             cancerEver, NA)) %>%
  mutate(cancerEver = car::recode(cancerEver,
                                  "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(cancerEver = factor(cancerEver, levels = c(0,1),
                             labels = c("no", "yes")))
summary(as.factor(df$cancerEver))

# Cancer same/better/worse

# labels for cancer status categories
cancer_labs <- c("Better", "Same", "Worse")

# change any values other than 1/2/3 to NA and convert to factor
df <- df %>%
  mutate(cancerBetterSameWorse = factor(
    ifelse(cancerBetterSameWorse >= 1 & cancerBetterSameWorse <= 3,
           cancerBetterSameWorse, NA), levels = c(1,2,3),
    labels = cancer_labs))
summary(as.factor(df$cancerBetterSameWorse))

# Cancer treatment last two years
# recode anything other than 1 (Yes) and 5 (No) to NA and convert to factor
df <- df %>%
  mutate(cancerTrtmtLast2Yrs = factor(car::recode(cancerTrtmtLast2Yrs,
                                                  "1 = 1;
                                                    5 = 0;
                                                       8 = 0"),
                                      levels = c(0,1),
                                      labels = c("no","yes")))
summary(as.factor(df$cancerTrtmtLast2Yrs))

# Cancer new since last wave
df <- df %>%
  mutate(cancerNewSinceLastWave = factor(
    ifelse(cancerNewSinceLastWave == 1 | cancerNewSinceLastWave == 5,
           cancerNewSinceLastWave, NA),
    levels = c(1,5),
    labels = c("yes","no")))
summary(as.factor(df$cancerNewSinceLastWave))

# create active cancer variable - 0 by default, change to 1 if evidence of
# active cancer based on responses to cancerTrmtLast2Yrs, canceBetterSameWorse,
# and cancerNewSinceLastWave
df$cancerActive <- 0
df$cancerActive[which(df$cancerTrtmtLast2Yrs == "yes")] <- 1
df$cancerActive[which(df$cancerBetterSameWorse == "Worse")] <- 1
df$cancerActive[which(df$cancerNewSinceLastWave == "yes")] <- 1
df$cancerActive[which(is.na(df$cancerEver) == TRUE &
                        is.na(df$cancerTrtmtLast2Yrs) == TRUE &
                        is.na(df$cancerBetterSameWorse) == TRUE &
                        is.na(df$cancerNewSinceLastWave) == TRUE)] <- NA
summary(as.factor(df$cancerActive))

# Diabetes
df <- df %>%
  mutate(diabetes = ifelse(diabetes >= 1 & diabetes <= 5,
                           diabetes, NA)) %>%
  mutate(diabetes = car::recode(diabetes,
                                "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(diabetes = factor(diabetes, levels = c(0,1),
                           labels = c("no", "yes")))
summary(as.factor(df$diabetes))

# Lung disease
df <- df %>%
  mutate(lungDis = ifelse(lungDis >= 1 & lungDis <= 5,
                          lungDis, NA)) %>%
  mutate(lungDis = car::recode(lungDis,
                               "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(lungDis = factor(lungDis, levels = c(0,1),
                          labels = c("no", "yes")))
summary(as.factor(df$lungDis))

# Heart condition
df <- df %>%
  mutate(hrtCond = ifelse(hrtCond >= 1 & hrtCond <= 5,
                          hrtCond, NA)) %>%
  mutate(hrtCond = car::recode(hrtCond,
                               "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(hrtCond = factor(hrtCond, levels = c(0,1),
                          labels = c("no", "yes")))
summary(as.factor(df$hrtCond))

# Angina
# set to 0 if response to screener question about heart conditions is also 0
df$angina[which(df$hrtCond == "no")] <- 0
df <- df %>%
  mutate(angina = ifelse(angina >= 0 & angina <= 5,
                         angina, NA)) %>%
  mutate(angina = car::recode(angina,
                              "0 = 0;
                                   5 = 0")) %>%
  mutate(angina = factor(angina, levels = c(0,1),
                         labels = c("no", "yes")))
summary(as.factor(df$angina))

# Stroke
df <- df %>%
  mutate(stroke = ifelse(stroke >= 1 & stroke <= 5,
                         stroke, NA)) %>%
  mutate(stroke = car::recode(stroke,
                              "5 = 0;
                                  4 = 0;
                                  3 = 1;
                                  2 = 0")) %>%
  mutate(stroke = factor(stroke, levels = c(0,1),
                         labels = c("no", "yes")))
summary(as.factor(df$stroke))

# Arthritis
df <- df %>%
  mutate(arthritis = ifelse(arthritis >= 1 & arthritis <= 5,
                            arthritis, NA)) %>%
  mutate(arthritis = car::recode(arthritis,
                                 "5 = 0;
                                  4 = 0;
                                  3 = 1")) %>%
  mutate(arthritis = factor(arthritis, levels = c(0,1),
                            labels = c("no", "yes")))
summary(as.factor(df$arthritis))

# BMI

# the heightXXXX and weightXXXX variables are self-reported measures, while the
# measuredHgtXXXX and measuredWgtXXXX variables are actual measurements
# recorded by the interviewer.

# Proposed strategy:
#   - start with self-reported heights/weights in 2016
#   - if possible, fill missing height/weight values with 2016 measured values
#   - if values are still missing, propagate forward and then backwards across
#     all waves (not just nearby ones)

# set impossible values to NA
df <- df %>%
  mutate(weight1992 = ifelse(weight1992 >= 997, NA, weight1992)) %>%
  mutate(htFeet1992 = ifelse(htFeet1992 >= 8, NA, htFeet1992)) %>%
  mutate(htInches1992 = ifelse(htInches1992 > 12, NA, htInches1992)) %>%
  mutate(weight1993 = ifelse(weight1993 >= 997, NA, weight1993)) %>%
  mutate(htInches1993 = ifelse(htInches1993 > 96, NA, htInches1993)) %>%
  mutate(weight1994 = ifelse(weight1994 >= 997, NA, weight1994)) %>%
  mutate(htFeet1994 = ifelse(htFeet1994 >= 8, NA, htFeet1994)) %>%
  mutate(htInches1994 = ifelse(htInches1994 > 12, NA, htInches1994)) %>%
  mutate(weight1995 = ifelse(weight1995 >= 997, NA, weight1995)) %>%
  mutate(htFeet1995 = ifelse(htFeet1995 >= 8, NA, htFeet1995)) %>%
  mutate(htInches1995 = ifelse(htInches1995 > 12, NA, htInches1995)) %>%
  mutate(weight1996 = ifelse(weight1996 >= 997, NA, weight1996)) %>%
  mutate(htFeet1996 = ifelse(htFeet1996 >= 8, NA, htFeet1996)) %>%
  mutate(htInches1996 = ifelse(htInches1996 > 12, NA, htInches1996)) %>%
  mutate(weight1998 = ifelse(weight1998 >= 997, NA, weight1998)) %>%
  mutate(htFeet1998 = ifelse(htFeet1998 >= 8, NA, htFeet1998)) %>%
  mutate(htInches1998 = ifelse(htInches1998 > 12, NA, htInches1998)) %>%
  mutate(weight2000 = ifelse(weight2000 >= 997, NA, weight2000)) %>%
  mutate(htFeet2000 = ifelse(htFeet2000 >= 8, NA, htFeet2000)) %>%
  mutate(htInches2000 = ifelse(htInches2000 > 12, NA, htInches2000)) %>%
  mutate(weight2002 = ifelse(weight2002 >= 997, NA, weight2002)) %>%
  mutate(htFeet2002 = ifelse(htFeet2002 >= 8, NA, htFeet2002)) %>%
  mutate(htInches2002 = ifelse(htInches2002 > 12, NA, htInches2002)) %>%
  mutate(weight2004 = ifelse(weight2004 >= 997, NA, weight2004)) %>%
  mutate(htFeet2004 = ifelse(htFeet2004 >= 8, NA, htFeet2004)) %>%
  mutate(htInches2004 = ifelse(htInches2004 > 12, NA, htInches2004)) %>%
  mutate(weight2006 = ifelse(weight2006 >= 997, NA, weight2006)) %>%
  mutate(htFeet2006 = ifelse(htFeet2006 >= 8, NA, htFeet2006)) %>%
  mutate(htInches2006 = ifelse(htInches2006 > 12, NA, htInches2006)) %>%
  mutate(weight2008 = ifelse(weight2008 >= 997, NA, weight2008)) %>%
  mutate(htFeet2008 = ifelse(htFeet2008 >= 8, NA, htFeet2008)) %>%
  mutate(htInches2008 = ifelse(htInches2008 > 12, NA, htInches2008)) %>%
  mutate(weight2010 = ifelse(weight2010 >= 997, NA, weight2010)) %>%
  mutate(htFeet2010 = ifelse(htFeet2010 >= 8, NA, htFeet2010)) %>%
  mutate(htInches2010 = ifelse(htInches2010 > 12, NA, htInches2010)) %>%
  mutate(weight2012 = ifelse(weight2012 >= 997, NA, weight2012)) %>%
  mutate(htFeet2012 = ifelse(htFeet2012 >= 8, NA, htFeet2012)) %>%
  mutate(htInches2012 = ifelse(htInches2012 > 12, NA, htInches2012)) %>%
  mutate(weight2014 = ifelse(weight2014 >= 997, NA, weight2014)) %>%
  mutate(htFeet2014 = ifelse(htFeet2014 >= 8, NA, htFeet2014)) %>%
  mutate(htInches2014 = ifelse(htInches2014 > 12, NA, htInches2014)) %>%
  mutate(weight = ifelse(weight >= 997, NA, weight)) %>%
  mutate(htFeet = ifelse(htFeet >= 8, NA, htFeet)) %>%
  mutate(htInches = ifelse(htInches > 12, NA, htInches)) %>%
  mutate(weight2018 = ifelse(weight2018 >= 997 | weight2018 < 0, NA, weight2018)) %>%
  mutate(htFeet2018 = ifelse(htFeet2018 >= 8, NA, htFeet2018)) %>%
  mutate(htInches2018 = ifelse(htInches2018 > 12, NA, htInches2018))

# calculate height in inches
df <- df %>%
  mutate(height1992 = ((12 * htFeet1992) + htInches1992)) %>%
  mutate(height1993 = htInches1993) %>% # recordd in inches only in 1993
  mutate(height1994 = ((12 * htFeet1994) + htInches1994)) %>%
  mutate(height1995 = ((12 * htFeet1995) + htInches1995)) %>%
  mutate(height1996 = ((12 * htFeet1996) + htInches1996)) %>%
  mutate(height1998 = ((12 * htFeet1998) + htInches1998)) %>%
  mutate(height2000 = ((12 * htFeet2000) + htInches2000)) %>%
  mutate(height2002 = ((12 * htFeet2002) + htInches2002)) %>%
  mutate(height2004 = ((12 * htFeet2004) + htInches2004)) %>%
  mutate(height2006 = ((12 * htFeet2006) + htInches2006)) %>%
  mutate(height2008 = ((12 * htFeet2008) + htInches2008)) %>%
  mutate(height2010 = ((12 * htFeet2010) + htInches2010)) %>%
  mutate(height2012 = ((12 * htFeet2012) + htInches2012)) %>%
  mutate(height2014 = ((12 * htFeet2014) + htInches2014)) %>%
  mutate(height = ((12 * htFeet) + htInches)) %>%
  mutate(height2018 = ((12 * htFeet2018) + htInches2018))

# try fill in missing values with measured values
df$height[which(is.na(df$height) == TRUE)] <- df$measuredHgt[which(is.na(df$height) == TRUE)]
df$weight[which(is.na(df$weight) == TRUE)] <- df$measuredWgt[which(is.na(df$weight) == TRUE)]

# Propagate height forward first, then backwards.
df$height1993[which(is.na(df$height1993) == TRUE)] <- df$height1992[which(is.na(df$height1993) == TRUE)]
df$height1994[which(is.na(df$height1994) == TRUE)] <- df$height1993[which(is.na(df$height1994) == TRUE)]
df$height1995[which(is.na(df$height1995) == TRUE)] <- df$height1994[which(is.na(df$height1995) == TRUE)]
df$height1996[which(is.na(df$height1996) == TRUE)] <- df$height1995[which(is.na(df$height1996) == TRUE)]
df$height1998[which(is.na(df$height1998) == TRUE)] <- df$height1996[which(is.na(df$height1998) == TRUE)]
df$height2000[which(is.na(df$height2000) == TRUE)] <- df$height1998[which(is.na(df$height2000) == TRUE)]
df$height2002[which(is.na(df$height2002) == TRUE)] <- df$height2000[which(is.na(df$height2002) == TRUE)]
df$height2004[which(is.na(df$height2004) == TRUE)] <- df$height2002[which(is.na(df$height2004) == TRUE)]
df$height2006[which(is.na(df$height2006) == TRUE)] <- df$height2004[which(is.na(df$height2006) == TRUE)]
df$height2008[which(is.na(df$height2008) == TRUE)] <- df$height2006[which(is.na(df$height2008) == TRUE)]
df$height2010[which(is.na(df$height2010) == TRUE)] <- df$height2008[which(is.na(df$height2010) == TRUE)]
df$height2012[which(is.na(df$height2012) == TRUE)] <- df$height2010[which(is.na(df$height2012) == TRUE)]
df$height2014[which(is.na(df$height2014) == TRUE)] <- df$height2012[which(is.na(df$height2014) == TRUE)]
df$height[which(is.na(df$height) == TRUE)] <- df$height2014[which(is.na(df$height) == TRUE)]
# propagate backwards
df$height[which(is.na(df$height) == TRUE)] <- df$height2018[which(is.na(df$height) == TRUE)]

# decided not to  propagate weight forwards/backwards are more likely to
# fluctuate compared to height

# create vector of BMI category labels
bmi_cats <- c("Underweight (<18.5)", "Normal weight (<25)",
              "Overweight (<30),", "Obese 1 (<35)", "Obese 2 (<40)",
              "Obese 3 (40+)")

# calculate BMI and convert to factor - Use conversion factor (of 703) since
# original formula expects kgs and meters.
df <- df %>%
  mutate(bmi = (( weight / (height ^ 2) ) * 703)) %>%
  mutate(bmi6Cats = factor(
    cut(bmi, breaks = c(0, 18.5, 25, 30, 35, 40, Inf)),
    labels = bmi_cats))

summary(as.factor(df$bmi6Cats))

# check CESD scores
summary(as.factor(df$randCESD))

# Smoking

# Current smoker
df <- df %>%
  mutate(randSmokeNow = factor(randSmokeNow,
                               levels = c(0,1), labels = c("no","yes")))
summary(as.factor(df$randSmokeNow))

# ever smoked
df <- df %>%
  mutate(randSmokeEver = factor(randSmokeEver,
                                levels = c(0,1), labels = c("no","yes")))
summary(as.factor(df$randSmokeEver))

# create vector of labels for smoker status variable
smoke_status <- c("Never smoker", "Former smoker", "Current smoker")

# create smoker status variables
df$smokeStatus <- NA
df$smokeStatus[which(df$randSmokeEver == "no")] <- 0
df$smokeStatus[which(df$randSmokeEver == "yes" &
                       df$randSmokeNow == "no")] <- 1
df$smokeStatus[which(df$randSmokeEver == "yes" &
                       df$randSmokeNow == "yes")] <- 2

# recode to factor variables
df <- df %>%
  mutate(smokeStatus = factor(smokeStatus, levels = c(0,1,2),
                              labels = smoke_status))
summary(as.factor(df$smokeStatus))

# Health insurance type

# goverment insurance plan variable is coded 0 = no, 1 = yes
# private health insurance variable is a count of how many private insurance
# plans the participant has - so a value >= 1 indicates private health
# insurance
df$insurance <- NA
df$insurance[which(df$govHealthIns == 0 & df$private == 0)] <- 0 # no insurance
df$insurance[which(df$private > 0)] <- 1 # some private insurance
df$insurance[which(df$govHealthIns == 1 & df$private == 0)] <- 2 # gov insurance only

# create vector of labels for insurance status variable
ins_status <- c("Uninsured", "Any private insurance", "Public insurance only")

# convert to factor variable
df <- df %>%
  mutate(insurance = factor(insurance, levels = c(0,1,2),
                            labels = ins_status))
summary(as.factor(df$insurance))


# select just relevant variables
df_final <- df %>%
  select(hhidpn, troubledWithPain, painUsualSeverity, painPreventActivity,
         painMeds, painLevel, painDisability, painOpioids, backPain, diabetes,
         lungDis, hrtCond, angina, stroke, arthritis, maritalStatus, jobStatus,
         foodInsecurity, prescode, sample_wgt, palive, gender, birthYear,
         monthOfDeath, yearOfDeath, yrOf1stIntvw, fwhy0wgt, householdSize,
         numChildren, veteranStatus,  randCESD, region, urbanicity, age,
         race4Cats, edu4Cats, wealthQuarts, region4Cats, jobStatus4Cats,
         cancerActive, bmi6Cats, smokeStatus, insurance)

#--------------------------------
# EXPORT FINAL CLEANED DATASET

# export data as a csv file
write_csv(df_final, paste0(directory, "/HRS_2016_pain.csv"))
