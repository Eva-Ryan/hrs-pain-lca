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
names(long_rand) <- c("wealth2016", "householdSize2016", "numChildren2016",
                      "govHealthIns2016", "private2016", "veteranStatus",
                      "randCESD2016", "randSmokeEver2016", "randSmokeNow2016",
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
names(region) <- c("region2016", "urbanicity2016", "hhidpn")


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
  mutate(wealthQuarts2016 = cut(wealth2016,
                                breaks = (quantile(wealth2016,
                                                   c(0, 0.25, 0.5, 0.75, 1),
                                                   na.rm = TRUE))))

# create vector of factor level names for census region
census_region <- c("New England", "Mid-Atlantic", "East North Central",
                   "West North Central", "South Atlantic", "East South Central",
                   "West South Central", "Mountain", "Pacific")

# create vector of factor level names for 4 category region variable
region4 <- c("Northeast", "Mid-west", "South", "West")

# tidy up census region variable
df <- df %>%
  mutate(region2016 = factor(
    ifelse(region2016 >= 1 & region2016 <= 9, region2016, NA),
    levels = c(1:9), labels = census_region)) %>%
  # recode to a 4 category variable
  mutate(region4Cats2016 = factor(car::recode(region2016,
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

# create vector of factor level names for urbanicity
urbanicity <- c("Urban", "Suburban", "Ex-urban/rural")

df <- df %>%
  mutate(urbanicity2016 = factor(
    ifelse(urbanicity2016 >= 1 & urbanicity2016 <= 3, urbanicity2016, NA),
    levels = c(1,2,3), labels = urbanicity))

# create vector of marital status categories
marital_status <- c("Married", "Separated/Divorced", "Widowed", "Never married",
                    "Other")

# recode marital status variable
df <- df %>%
  mutate(maritalStatus2016 = ifelse(maritalStatus2016 >= 1 &
                                      maritalStatus2016 <= 7,
                                    maritalStatus2016, NA)) %>%
  mutate(maritalStatus2016 = car::recode(maritalStatus2016,
                                         "1 = 1; 2 = 2; 3 = 2; 4 = 2; 5 = 3;
                                         6 = 4; 7 = 5"))

# household size - no cleaning needed
summary(as.factor(df$householdSize2016))

# number of children - no cleaning needed
summary(as.factor(df$numChildren2016))

# create vector of current employment categories
employment_status <- c("Employed", "Unemployed", "Retired", "Not in labour force")

# recode to a 4 category variable:
# Employed = working now
# Unemployed = unemployed and looking for work / temporarily laid off, on sick
# or other leave / disabled / homemaker / other
# Retired = retired
df <- df %>%
  mutate(jobStatus2016 = ifelse(jobStatus2016 >= 1 & jobStatus2016 <= 8,
                                jobStatus2016, NA)) %>%
  mutate(jobStatus4Cats2016 = factor(car::recode(jobStatus2016,
                                                 "1 = 1;
                                                 2 = 2; 3 = 2; 7 = 2;
                                                 5 = 3;
                                                 4 = 4; 6 = 4"),
                                     levels = c(1:4), labels = employment_status))

# Financial insecurity - food insecurity

# recode "in the last two years, have you always had enough money to buy food?"
# to 1 = yes, 0 = no
df <- df %>%
  mutate(foodInsecurity2016 = ifelse(foodInsecurity2016 >= 1 &
                                       foodInsecurity2016 <= 5,
                                     foodInsecurity2016, NA)) %>%
  mutate(foodInsecurity2016 = factor(car::recode(foodInsecurity2016,
                                                 "1 = 1;
                                                 5 = 0"),
                                     levels = c(0,1), labels = c("no", "yes")))

# Create a flag showing if someone was in active cancer treatment (or palliative
# care) at time of current wave
df <- df %>%
  mutate(cancerEver2016 = ifelse(cancerEver2016 >= 1 & cancerEver2016 <= 5,
                                 cancerEver2016, NA)) %>%
  mutate(cancerEver2016 = car::recode(cancerEver2016,
                                      "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(cancerEver2016 = factor(cancerEver2016, levels = c(0,1),
                                 labels = c("no", "yes")))

# Cancer same/better/worse

# labels for cancer status categories
cancer_labs <- c("Better", "Same", "Worse")

# change any values other than 1/2/3 to NA and convert to factor
df <- df %>%
  mutate(cancerBetterSameWorse2016 = factor(
    ifelse(cancerBetterSameWorse2016 >= 1 & cancerBetterSameWorse2016 <= 3,
           cancerBetterSameWorse2016, NA), levels = c(1,2,3),
    labels = cancer_labs))

# Cancer treatment last two years (NOTE: no responses in 2016)
# recode anything other than 1 (Yes) and 5 (No) to NA and convert to factor
df <- df %>%
  mutate(cancerTrtmtLast2YrsP2016 = factor(car::recode(cancerTrtmtLast2YrsP2016,
                                                       "1 = 1;
                                                    5 = 0;
                                                       8 = 0"),
                                           levels = c(0,1),
                                           labels = c("no","yes")))

# Cancer new since last wave
df <- df %>%
  mutate(cancerNewSinceLastWaveP2016 = factor(
    ifelse(cancerNewSinceLastWaveP2016 == 1 | cancerNewSinceLastWaveP2016 == 5,
           cancerNewSinceLastWaveP2016, NA),
    levels = c(1,5),
    labels = c("yes","no")))

# create active cancer variable - 0 by default, change to 1 if evidence of
# active cancer based on responses to cancerTrmtLast2Yrs, canceBetterSameWorse,
# and cancerNewSinceLastWave
df$cancerActiveP2016 <- 0
df$cancerActiveP2016[which(df$cancerTrtmtLast2YrsP2016 == "yes")] <- 1
df$cancerActiveP2016[which(df$cancerBetterSameWorse2016 == "Worse")] <- 1
df$cancerActiveP2016[which(df$cancerNewSinceLastWaveP2016 == "yes")] <- 1
df$cancerActiveP2016[which(is.na(df$cancerEverP2016) == TRUE &
                             is.na(df$cancerTrtmtLast2YrsP2016) == TRUE &
                             is.na(df$cancerBetterSameWorse2016) == TRUE &
                             is.na(df$cancerNewSinceLastWaveP2016) == TRUE)] <- NA

# Diabetes
df <- df %>%
  mutate(diabetesP2016 = ifelse(diabetesP2016 >= 1 & diabetesP2016 <= 5,
                                diabetesP2016, NA)) %>%
  mutate(diabetesP2016 = car::recode(diabetesP2016,
                                     "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(diabetesP2016 = factor(diabetesP2016, levels = c(0,1),
                                labels = c("no", "yes")))

# Lung disease
df <- df %>%
  mutate(lungDisP2016 = ifelse(lungDisP2016 >= 1 & lungDisP2016 <= 5,
                               lungDisP2016, NA)) %>%
  mutate(lungDisP2016 = car::recode(lungDisP2016,
                                    "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(lungDisP2016 = factor(lungDisP2016, levels = c(0,1),
                               labels = c("no", "yes")))

# Heart condition
df <- df %>%
  mutate(hrtCondP2016 = ifelse(hrtCondP2016 >= 1 & hrtCondP2016 <= 5,
                               hrtCondP2016, NA)) %>%
  mutate(hrtCondP2016 = car::recode(hrtCondP2016,
                                    "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(hrtCondP2016 = factor(hrtCondP2016, levels = c(0,1),
                               labels = c("no", "yes")))

# Angina
# set to 0 if response to screener question about heart conditions is also 0
df$anginaP2016[which(df$hrtCondP2016 == "no")] <- 0
df <- df %>%
  mutate(anginaP2016 = ifelse(anginaP2016 >= 0 & anginaP2016 <= 5,
                              anginaP2016, NA)) %>%
  mutate(anginaP2016 = car::recode(anginaP2016,
                                   "0 = 0;
                                   5 = 0")) %>%
  mutate(anginaP2016 = factor(anginaP2016, levels = c(0,1),
                              labels = c("no", "yes")))

# Stroke
mutate(strokeP2016 = ifelse(strokeP2016 >= 1 & strokeP2016 <= 5,
                            strokeP2016, NA)) %>%
  mutate(strokeP2016 = car::recode(strokeP2016,
                                   "5 = 0;
                                  4 = 0;
                                  3 = 1;
                                  2 = 0")) %>%
  mutate(strokeP2016 = factor(strokeP2016, levels = c(0,1),
                              labels = c("no", "yes")))

# Arthritis
df <- df %>%
  mutate(arthritisP2016 = ifelse(arthritisP2016 >= 1 & arthritisP2016 <= 5,
                                 arthritisP2016, NA)) %>%
  mutate(arthritisP2016 = car::recode(arthritisP2016,
                                      "5 = 0;
                                  4 = 0;
                                  3 = 1")) %>%
  mutate(arthritisP2016 = factor(arthritisP2016, levels = c(0,1),
                                 labels = c("no", "yes")))

# BMI
# set impossible values to NA
df <- df %>%
  mutate(weight2016 = ifelse(weight2016 >= 997, NA, weight2016)) %>%
  mutate(htFeet2016 = ifelse(htFeet2016 >= 8, NA, htFeet2016)) %>%
  mutate(htInches2016 = ifelse(htInches2016 > 12, NA, htInches2016))

# calculate height in inches
df <- df %>%
  mutate(height2016 = ((12 * htFeet2016) + htInches2016))

# make sure no unreasonable measurements first - set max weight at 400 pounds and
# max height at < 8 Ft (<96). Set min weight to 60 pounds and min height to 3 Ft
# (36 inches)
df <- df %>%
  mutate(measuredWgt2016 = ifelse(measuredWgt2016 > 60 & measuredWgt2016 <= 400,
                                  measuredWgt2016, NA)) %>%
  mutate(measuredHgt2016 = ifelse(measuredHgt2016 > 36 & measuredHgt2016 <= 96,
                                  measuredHgt2016, NA))

# try fill in missing values with measured values
df$height2016[which(is.na(df$height2016) == TRUE &
                      df$deadP2016 == 0)] <- df$measuredHgt2016[which(is.na(df$height2016) == TRUE &
                                                                        df$deadP2016 == 0)]
df$weight2016[which(is.na(df$weight2016) == TRUE &
                      df$deadP2016 == 0)] <- df$measuredWgt2016[which(is.na(df$weight2016) == TRUE &
                                                                        df$deadP2016 == 0)]

# create vector of BMI category labels
bmi_cats <- c("Underweight (<18.5)", "Normal weight (<25)",
              "Overweight (<30),", "Obese 1 (<35)", "Obese 2 (<40)",
              "Obese 3 (40+)")

# calculate BMI and convert to factor - Use conversion factor (of 703) since
# original formula expects kgs and meters.
df <- df %>%
  mutate(bmi2016 = (( weight2016 / (height2016 ^ 2) ) * 703)) %>%
  mutate(bmi6Cats2016 = factor(
    cut(bmi2016, breaks = c(0, 18.5, 25, 30, 35, 40, Inf)),
    labels = bmi_cats))

# check CESD scores
summary(as.factor(df$randCESD2016))

# Smoking

# Current smoker
df <- df %>%
  mutate(randSmokeNow2016 = factor(randSmokeNow2016,
                                   levels = c(0,1), labels = c("no","yes")))

# ever smoked
df <- df %>%
  mutate(randSmokeEver2016 = factor(randSmokeEver2016,
                                    levels = c(0,1), labels = c("no","yes")))

# create vector of labels for smoker status variable
smoke_status <- c("Never smoker", "Former smoker", "Current smoker")

# create smoker status variables
df$smokeStatus2016 <- NA
df$smokeStatus2016[which(df$randSmokeEver2016 == "no")] <- 0
df$smokeStatus2016[which(df$randSmokeEver2016 == "yes" &
                           df$randSmokeNow2016 == "no")] <- 1
df$smokeStatus2016[which(df$randSmokeEver2016 == "yes" &
                           df$randSmokeNow2016 == "yes")] <- 2

# recode to factor variables
df <- df %>%
  mutate(smokeStatus2016 = factor(smokeStatus2016, levels = c(0,1,2),
                                  labels = smoke_status))

# Health insurance type

# goverment insurance plan variable is coded 0 = no, 1 = yes
# private health insurance variable is a count of how many private insurance
# plans the participant has - so a value >= 1 indicates private health
# insurance
df$insurance2016 <- NA
df$insurance2016[which(df$govHealthIns2016 == 0 & df$private2016 == 0)] <- 0 # no insurance
df$insurance2016[which(df$private2016 > 0)] <- 1 # some private insurance
df$insurance2016[which(df$govHealthIns2016 == 1 & df$private2016 == 0)] <- 2 # gov insurance only

# create vector of labels for insurance status variable
ins_status <- c("Uninsured", "Any private insurance", "Public insurance only")

# convert to factor variable
df <- df %>%
  mutate(insurance2016 = factor(insurance2016, levels = c(0,1,2),
                                labels = ins_status))


#--------------------------------
# EXPORT FINAL CLEANED DATASET

# export data as a csv file
write_csv(df, paste0(directory, "/HRS_2016_pain.csv"))
