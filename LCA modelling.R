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

# create vectors of fit statistics
AICs <- c(m1$aic, m2$aic, m3$aic, m4$aic, m5$aic, m6$aic)
BICs <- c(m1$bic, m2$bic, m3$bic, m4$bic, m5$bic, m6$bic)

#------------------------
# calculate NFI and NNFI

# calculate degrees of freedom for each model using the formula df = nz − P − 1,
# where nz = # of cells in contingency table and P = number of parameters
# estimated for the model
nz <- 4*2*2*2*2
degfree <- c(nz - m1$npar - 1,
             nz - m2$npar - 1,
             nz - m3$npar - 1,
             nz - m4$npar - 1,
             nz - m5$npar - 1,
             nz - m6$npar - 1)

NFIs <- c(NA,
          (m1$Gsq - m2$Gsq)/m1$Gsq,
          (m1$Gsq - m3$Gsq)/m1$Gsq,
          (m1$Gsq - m4$Gsq)/m1$Gsq,
          (m1$Gsq - m5$Gsq)/m1$Gsq,
          (m1$Gsq - m6$Gsq)/m1$Gsq)

NNFIs <- c(NA,
           ((m1$Gsq/degfree[1]) - (m2$Gsq/degfree[2]))/((m1$Gsq/degfree[1]) - 1),
           ((m1$Gsq/degfree[1]) - (m3$Gsq/degfree[3]))/((m1$Gsq/degfree[1]) - 1),
           ((m1$Gsq/degfree[1]) - (m4$Gsq/degfree[4]))/((m1$Gsq/degfree[1]) - 1),
           ((m1$Gsq/degfree[1]) - (m5$Gsq/degfree[5]))/((m1$Gsq/degfree[1]) - 1),
           ((m1$Gsq/degfree[1]) - (m6$Gsq/degfree[6]))/((m1$Gsq/degfree[1]) - 1))

# merge all into one dataframe
indices <- cbind(1:6, AICs, BICs, NFIs, NNFIs) %>% as.data.frame()
names(indices) <- c("classes", "aic", "bic", "nfi", "nnfi")

#---------------------
# plot the fit indices

# AIC
aic_plot <- ggplot(indices, aes(x = classes, y = aic)) +
  geom_line(linewidth = 1, color = "red") +
  geom_point(shape = 21, size = 2, fill = "red") +
  theme_light() +
  xlab("Number of classes") + ylab("AIC") +
  ggtitle("AIC plot") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(1:6))
aic_plot

# BIC
bic_plot <- ggplot(indices, aes(x = classes, y = bic)) +
  geom_line(linewidth = 1, color = "red") +
  geom_point(shape = 21, size = 2, fill = "red") +
  theme_light() +
  xlab("Number of classes") + ylab("BIC") +
  ggtitle("BIC plot") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(1:6))
bic_plot

# NFI
nfi_plot <- ggplot(indices[-1,], aes(x = classes, y = nfi)) +
  geom_line(linewidth = 1, color = "red") +
  geom_point(shape = 21, size = 2, fill = "red") +
  theme_light() +
  xlab("Number of classes") + ylab("NFI") +
  ggtitle("NFI plot") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(2:6))
nfi_plot

# NFI
nnfi_plot <- ggplot(indices[-1,], aes(x = classes, y = nnfi)) +
  geom_line(linewidth = 1, color = "red") +
  geom_point(shape = 21, size = 2, fill = "red") +
  theme_light() +
  xlab("Number of classes") + ylab("NNFI") +
  ggtitle("NNFI plot") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(2:6))
nnfi_plot
