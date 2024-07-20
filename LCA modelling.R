# code to fit a pain LCA model using 2016 HRS data
#------------------------------------------------------------------------------

# load packages
library(tidyverse)
library(poLCA)
library(tictoc)
library(beepr)

#--------------------------------
# PREPARE DATA

# load data
df <- read_csv(paste0("HRS_2016_pain.csv"))

# convert variables to correct type
df <- df %>%
  mutate(gender = factor(gender, levels = c(1,2),
                         labels = c("male", "female"))) %>%
  mutate(race4Cats = factor(race4Cats, levels = c(1, 2, 3, 4),
                            labels = c("White (non-Hispanic)",
                                       "Black (non-Hispanic)",
                                       "Hispanic",
                                       "Other (non-Hispanic)"))) %>%
  mutate(edu4Cats = ordered(edu4Cats, levels = c(1,2,3,4),
                            labels = c("No degree", "High school degree",
                                       "4-year college degree",
                                       "Graduate degree"))) %>%
  mutate(painLevel = ordered(painLevel, levels = c(1,2,3,4),
                             labels = c("no", "mild", "moderate",
                                        "severe"))) %>%
  mutate(painDisability = factor(painDisability, levels = c(1,2),
                                 labels = c("yes", "no"))) %>%
  mutate(painMeds = factor(painMeds, levels = c(1,2),
                           labels = c("yes", "no"))) %>%
  mutate(painOpioids = factor(painOpioids, levels = c(1,2),
                              labels = c("yes", "no"))) %>%
  mutate(backPain = factor(backPain, levels = c(1,2),
                           labels = c("yes", "no")))

# data summary
summary(df)

# check % missing data for each pain variable
na_count <- df %>% dplyr::select(painLevel, painDisability, painMeds, painOpioids,
                                 backPain) %>%
  sapply(function(x) sum(is.na(x)))
na_percent <- paste0(round((na_count/nrow(df))*100, 1), "%")

data.frame(NA_count = na_count,
           NA_percent = na_percent)
# Output:
#                NA_count NA_percent
# painLevel           120       0.6%
# painDisability      126       0.6%
# painMeds             36       0.2%
# painOpioids         346       1.8%
# backPain             46       0.2%

# most missing is 1.8% for opioid use variable

# create new complete cases dataset with those who are missing pain data removed
df_cc <- df %>%
  filter(is.na(painLevel) == FALSE & is.na(painDisability) == FALSE &
           is.na(painMeds) == FALSE & is.na(painOpioids) == FALSE &
           is.na(backPain) == FALSE) # 19623 - 19048 = 575 participants removed


#--------------------------------
# FIT LCA MODELS

# calculate models with 1-6 classes with NA values retained

tic()

set.seed(6049)
m1 <- poLCA(cbind(painLevel, painDisability, painMeds, painOpioids, backPain)~1,
            data = df, nclass = 1, maxiter = 1000, graphs = TRUE, tol = 1e-10,
            na.rm = FALSE, nrep = 100, verbose = FALSE, calc.se = TRUE)

set.seed(6049)
m2 <- poLCA(cbind(painLevel, painDisability, painMeds, painOpioids, backPain)~1,
            data = df, nclass = 2, maxiter = 1000, graphs = TRUE, tol = 1e-10,
            na.rm = FALSE, nrep = 100, verbose = FALSE, calc.se = TRUE)

set.seed(6049)
m3 <- poLCA(cbind(painLevel, painDisability, painMeds, painOpioids, backPain)~1,
            data = df, nclass = 3, maxiter = 1000, graphs = TRUE, tol = 1e-10,
            na.rm = FALSE, nrep = 100, verbose = FALSE, calc.se = TRUE)

set.seed(6049)
m4 <- poLCA(cbind(painLevel, painDisability, painMeds, painOpioids, backPain)~1,
            data = df, nclass = 4, maxiter = 1000, graphs = TRUE, tol = 1e-10,
            na.rm = FALSE, nrep = 100, verbose = FALSE, calc.se = TRUE)

set.seed(6049)
m5 <- poLCA(cbind(painLevel, painDisability, painMeds, painOpioids, backPain)~1,
            data = df, nclass = 5, maxiter = 1000, graphs = TRUE, tol = 1e-10,
            na.rm = FALSE, nrep = 100, verbose = FALSE, calc.se = TRUE)

set.seed(6049)
m6 <- poLCA(cbind(painLevel, painDisability, painMeds, painOpioids, backPain)~1,
            data = df, nclass = 6, maxiter = 1000, graphs = TRUE, tol = 1e-10,
            na.rm = FALSE, nrep = 100, verbose = FALSE, calc.se = TRUE)

toc()
beep()
