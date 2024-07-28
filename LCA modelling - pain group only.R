# Code to fit a pain LCA model using 2016 HRS data (just the participants who
# reported pain for Chapter 6 of my PhD thesis.

# Author: Eva Ryan
# Date: July 2024
#------------------------------------------------------------------------------

# load packages
library(tidyverse)
library(poLCA)
library(tictoc)
library(beepr)
library(gridExtra)
library(grid)
library(ggpubr)
#devtools::install_github("daob/poLCA.extras")
library(poLCA.extras)
library(tableone)

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
  mutate(edu4Cats =  factor(edu4Cats, levels = c(1,2,3,4),
                            labels = c("No degree", "High school degree",
                                       "4-year college degree",
                                       "Graduate degree"))) %>%
  mutate(painUsualSeverity =  factor(painUsualSeverity, levels = c(2,3,4),
                                     labels = c("mild", "moderate",
                                                "severe"))) %>%
  mutate(painDisability = factor(painDisability, levels = c(1,2),
                                 labels = c("yes", "no"))) %>%
  mutate(painMeds = factor(painMeds, levels = c(1,2),
                           labels = c("yes", "no"))) %>%
  mutate(painOpioids = factor(painOpioids, levels = c(1,2),
                              labels = c("yes", "no"))) %>%
  mutate(backPain = factor(backPain, levels = c(1,2),
                           labels = c("yes", "no"))) %>%
  mutate(diabetes = factor(diabetes, levels = c("no", "yes"),
                           labels = c("no", "yes"))) %>%
  mutate(lungDis = factor(lungDis, levels = c("no", "yes"),
                          labels = c("no", "yes"))) %>%
  mutate(hrtCond = factor(hrtCond, levels = c("no", "yes"),
                          labels = c("no", "yes"))) %>%
  mutate(angina = factor(angina, levels = c("no", "yes"),
                         labels = c("no", "yes"))) %>%
  mutate(stroke = factor(stroke, levels = c("no", "yes"),
                         labels = c("no", "yes"))) %>%
  mutate(arthritis = factor(arthritis, levels = c("no", "yes"),
                            labels = c("no", "yes"))) %>%
  mutate(foodSecurity = factor(foodSecurity, levels = c("no", "yes"),
                               labels = c("no", "yes"))) %>%
  mutate(cancerActive = factor(cancerActive, levels = c(0, 1),
                               labels = c("no", "yes"))) %>%
  mutate(maritalStatus = factor(maritalStatus, levels = c(1,2,3,4,5),
                                labels = c("Married", "Separated/Divorced",
                                           "Widowed", "Never married",
                                           "Other"))) %>%
  mutate(veteranStatus = factor(veteranStatus)) %>%
  mutate(region4Cats = factor(region4Cats)) %>%
  mutate(urbanicity = factor(urbanicity)) %>%
  mutate(jobStatus4Cats = factor(jobStatus4Cats)) %>%
  mutate(bmi6Cats = factor(bmi6Cats)) %>%
  mutate(smokeStatus = factor(smokeStatus)) %>%
  mutate(insurance = factor(insurance)) %>%
  mutate(wealthQuarts = factor(wealthQuarts))

# data summary
summary(df)

# filter to just those with pain
df <- df %>%
  filter(troubledWithPain == 1)

# check % missing data for each pain variable
na_count <- df %>% dplyr::select(painUsualSeverity, painDisability, painMeds, painOpioids,
                                 backPain) %>%
  sapply(function(x) sum(is.na(x)))
na_percent <- paste0(round((na_count/nrow(df))*100, 1), "%")

data.frame(NA_count = na_count,
           NA_percent = na_percent)
#                   NA_count NA_percent
#painUsualSeverity       52       0.6%
#painDisability          58       0.7%
#painMeds                11       0.1%
#painOpioids            257       3.2%
#backPain                14       0.2%

# most missing is 3.2% for opioid use variable

# create new complete cases dataset with those who are missing pain data removed
df_cc <- df %>%
  filter(is.na(painUsualSeverity) == FALSE & is.na(painDisability) == FALSE &
           is.na(painMeds) == FALSE & is.na(painOpioids) == FALSE &
           is.na(backPain) == FALSE) # 8085 - 7712 = 373 participants removed
#                                     (4.61%)


#--------------------------------
# FIT LCA MODELS

# specify indicator variables
f1 <- as.formula(cbind(painUsualSeverity, painDisability, painMeds, painOpioids, backPain)~1)

# calculate models with 1-6 classes with NA values retained

tic()

set.seed(6049)
m1 <- poLCA(f1,
            data = df_cc, nclass = 1, maxiter = 5000, graphs = FALSE, tol = 1e-10,
            na.rm = TRUE, nrep = 100, verbose = TRUE, calc.se = TRUE)

set.seed(6049)
m2 <- poLCA(f1,
            data = df_cc, nclass = 2, maxiter = 5000, graphs = FALSE, tol = 1e-10,
            na.rm = TRUE, nrep = 100, verbose = TRUE, calc.se = TRUE)

set.seed(6049)
m3 <- poLCA(f1,
            data = df_cc, nclass = 3, maxiter = 5000, graphs = FALSE, tol = 1e-10,
            na.rm = TRUE, nrep = 100, verbose = TRUE, calc.se = TRUE)

set.seed(6049)
m4 <- poLCA(f1,
            data = df_cc, nclass = 4, maxiter = 5000, graphs = FALSE, tol = 1e-10,
            na.rm = TRUE, nrep = 100, verbose = TRUE, calc.se = TRUE)

set.seed(6049)
m5 <- poLCA(f1,
            data = df_cc, nclass = 5, maxiter = 5000, graphs = FALSE, tol = 1e-10,
            na.rm = TRUE, nrep = 100, verbose = TRUE, calc.se = TRUE)

set.seed(6049)
m6 <- poLCA(f1,
            data = df_cc, nclass = 6, maxiter = 5000, graphs = FALSE, tol = 1e-10,
            na.rm = TRUE, nrep = 100, verbose = TRUE, calc.se = TRUE)

toc()
beep()

# create vectors of fit statistics
#AICs <- c(m1$aic, m2$aic, m3$aic, m4$aic, m5$aic, m6$aic)
#BICs <- c(m1$bic, m2$bic, m3$bic, m4$bic, m5$bic, m6$bic)

# NOTE: poLCA calculates the AIC and BIC based on the log-likelihood (LL) using
# the formulae AIC = -2log(LL) + 2P and BIC = -2log(LL) + log(N)P

# calculate AICs and BICs manually using formulas from Collins & Lanza (2010)
# instead as these versions are also used in Chapter 5 of my thesis
AICs <- c(m1$Gsq + 2*m1$npar, m2$Gsq + 2*m2$npar, m3$Gsq + 2*m3$npar,
          m4$Gsq + 2*m4$npar, m5$Gsq + 2*m5$npar, m6$Gsq + 2*m6$npar)
BICs <- c(m1$Gsq + log(nrow(df_cc))*m1$npar, m2$Gsq + log(nrow(df_cc))*m2$npar,
          m3$Gsq + log(nrow(df_cc))*m3$npar, m4$Gsq + log(nrow(df_cc))*m4$npar,
          m5$Gsq + log(nrow(df_cc))*m5$npar, m6$Gsq + log(nrow(df_cc))*m6$npar)


# calculate NFI and NNFI

# extract degrees of freedom from fitted models
degfree <- c(m1$resid.df, m2$resid.df, m3$resid.df, m4$resid.df, m5$resid.df,
             m6$resid.df)

# calculate NFI
NFIs <- c(NA,
          (m1$Gsq - m2$Gsq)/m1$Gsq,
          (m1$Gsq - m3$Gsq)/m1$Gsq,
          (m1$Gsq - m4$Gsq)/m1$Gsq,
          (m1$Gsq - m5$Gsq)/m1$Gsq,
          (m1$Gsq - m6$Gsq)/m1$Gsq)

# calculate NNFI
NNFIs <- c(NA,
           ((m1$Gsq/degfree[1]) - (m2$Gsq/degfree[2]))/((m1$Gsq/degfree[1]) - 1),
           ((m1$Gsq/degfree[1]) - (m3$Gsq/degfree[3]))/((m1$Gsq/degfree[1]) - 1),
           ((m1$Gsq/degfree[1]) - (m4$Gsq/degfree[4]))/((m1$Gsq/degfree[1]) - 1),
           ((m1$Gsq/degfree[1]) - (m5$Gsq/degfree[5]))/((m1$Gsq/degfree[1]) - 1),
           ((m1$Gsq/degfree[1]) - (m6$Gsq/degfree[6]))/((m1$Gsq/degfree[1]) - 1))

# merge all into one dataframe
indices <- cbind(1:6, AICs, BICs, NFIs, NNFIs) %>% as.data.frame()
names(indices) <- c("classes", "aic", "bic", "nfi", "nnfi")

#--------------------------------
# PLOT THE FIT INDICES

# AIC
aic_plot <- ggplot(indices, aes(x = classes, y = aic)) +
  geom_line(linewidth = 1, color = "red") +
  geom_point(shape = 21, size = 2, fill = "red") +
  theme_light() +
  xlab("Number of classes") + ylab("AIC") +
  ggtitle("")  +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  scale_x_continuous(breaks = c(1:6))
aic_plot

# BIC
bic_plot <- ggplot(indices, aes(x = classes, y = bic)) +
  geom_line(linewidth = 1, color = "red") +
  geom_point(shape = 21, size = 2, fill = "red") +
  theme_light() +
  xlab("Number of classes") + ylab("BIC") +
  ggtitle("") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  scale_x_continuous(breaks = c(1:6))
bic_plot

# NFI
nfi_plot <- ggplot(indices[-1,], aes(x = classes, y = nfi)) +
  geom_line(linewidth = 1, color = "red") +
  geom_point(shape = 21, size = 2, fill = "red") +
  theme_light() +
  xlab("Number of classes") + ylab("NFI") +
  ggtitle("")  +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  scale_x_continuous(breaks = c(2:6))
nfi_plot

# NFI
nnfi_plot <- ggplot(indices[-1,], aes(x = classes, y = nnfi)) +
  geom_line(linewidth = 1, color = "red") +
  geom_point(shape = 21, size = 2, fill = "red") +
  theme_light() +
  xlab("Number of classes") + ylab("NNFI") +
  ggtitle("")  +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  scale_x_continuous(breaks = c(2:6))
nnfi_plot

# put the plots together
## combine all plots together into one figure
plot_grid <- grid.arrange(
  aic_plot, bic_plot, nfi_plot, nnfi_plot,
  ncol = 2, nrow = 2,
  top = text_grob("Fit Criteria Plots",
                  size = 15))

# save the combined plot
ggsave(
  filename = paste0(getwd(), "/Results - poLCA/All_plots_all_indicators_pain_only.png"),
  plot = plot_grid,
  units = "in",
  width = 10,
  height = 10,
  dpi = 1000
)

# save each plot individually
ggsave(
  filename = paste0(getwd(), "/Plots/AIC_all_indicators_pain_only.png"),
  plot = aic_plot,
  units = "in",
  width = 5,
  height = 4,
  dpi = 1000
)

ggsave(
  filename = paste0(getwd(), "/Plots/BIC_all_indicators_pain_only.png"),
  plot = bic_plot,
  units = "in",
  width = 5,
  height = 4,
  dpi = 1000
)

ggsave(
  filename = paste0(getwd(), "/Plots/NFI_all_indicators_pain_only.png"),
  plot = nfi_plot,
  units = "in",
  width = 5,
  height = 4,
  dpi = 1000
)

ggsave(
  filename = paste0(getwd(), "/Plots/NNFI_all_indicators_pain_only.png"),
  plot = nnfi_plot,
  units = "in",
  width = 5,
  height = 4,
  dpi = 1000
)

# check bivariate residuals and calculate p-values using bootstrapping
# (functions from Daniel Oberski's package poLCA.extras on GitHub)
# NOTE: cannot have missing data values
tic()
bvr_m2 <- bvr(m2)
bvr_m3 <- bvr(m3)
bvr_m4 <- bvr(m4)
bvr_m5 <- bvr(m5)
bvr_m6 <- bvr(m6)
bvr_boot_m2 <- bootstrap_bvr_pvals(formula = f1, fit_polca = m2, data = df_cc, R = 500)
bvr_boot_m3 <- bootstrap_bvr_pvals(formula = f1, fit_polca = m3, data = df_cc, R = 500)
bvr_boot_m4 <- bootstrap_bvr_pvals(formula = f1, fit_polca = m4, data = df_cc, R = 500)
bvr_boot_m5 <- bootstrap_bvr_pvals(formula = f1, fit_polca = m5, data = df_cc, R = 500)
bvr_boot_m6 <- bootstrap_bvr_pvals(formula = f1, fit_polca = m6, data = df_cc, R = 500)
toc()

# print results
bvr_m2; bvr_m3; bvr_m4; bvr_m5; bvr_m6
bvr_boot_m2; bvr_boot_m3; bvr_boot_m4; bvr_boot_m5; bvr_boot_m6


#--------------------------------
# PLOT THE LCA MODEL PARAMETERS

# create function to extract model parameters from LCA model objects
parameters.fun <- function(model, nclass, cats){
  # Inputs: model = poLCA object
  #         nclass = number of classes in the fitted LCA model
  #         cats = item categories to plot response probabilities for

  # create dataframe to store class membership probabilities and item response
  # probabilities for each class
  para.df <- as.data.frame(matrix(NA, nrow = nclass*length(cats), ncol = 4))
  names(para.df) <- c("Class", "classP", "item", "itemResponseP")

  # add class number and class membership probability for each class
  para.df$Class <- rep(1:nclass, each = length(cats))
  para.df$classP <- rep(round(model$P,3), each = length(cats))
  para.df$item <- rep(cats, times = nclass)

  # cycle through classes adding item response probabilities
  for(i in 1:nclass){
    # row index to start on
    ind <- length(cats)*(i-1) + 1
    # add item response probabilities for each item
    para.df$itemResponseP[ind:(ind+2)] <- round(model$probs$painUsualSeverity[i,1:3],3)
    para.df$itemResponseP[ind+3] <- round(model$probs$painDisability[i, 1],3)
    para.df$itemResponseP[ind+4] <- round(model$probs$painMeds[i, 1],3)
    para.df$itemResponseP[ind+5] <- round(model$probs$painOpioids[i, 1],3)
    para.df$itemResponseP[ind+6] <- round(model$probs$backPain[i, 1],3)
  }

  # make class and class membership prob. variable for plot legend
  para.df <- para.df %>%
    mutate(`Class (prob.)` = paste0(Class, " (", classP, ")")) %>%
    mutate(`Class (prob.)` = as.factor(`Class (prob.)`))

  # return the df of parameter values
  return(para.df)
}

# specify categories
categories <- c("Mild pain", "Moderate pain", "Severe pain",
                "Disabling pain", "Took meds (OTC)", "Took opioids",
                "Back pain"
)

# apply to the various candidate models
m2_parameters <- parameters.fun(m2, 2, categories)
m3_parameters <- parameters.fun(m3, 3, categories)
m4_parameters <- parameters.fun(m4, 4, categories)
m5_parameters <- parameters.fun(m5, 5, categories)
m6_parameters <- parameters.fun(m6, 6, categories)

# view
#m2_parameters
#m3_parameters
#m4_parameters
#m5_parameters
#m6_parameters

# create plots for each model

# 2 class model
plot_2classes <- m2_parameters %>%
  ggplot(aes(x = item, y = itemResponseP)) +
  geom_line(aes(color = `Class (prob.)`, group = `Class (prob.)`)) +
  geom_point(aes(shape = `Class (prob.)`, color = `Class (prob.)`,
                 group = `Class (prob.)`)) +
  theme_light() +
  #xlab("Item responses") + ylab("Item response probabilities") +
  xlab("") + ylab("") +
  ggtitle("2-class LCA model") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

# 3 class model
plot_3classes <- m3_parameters %>%
  ggplot(aes(x = item, y = itemResponseP)) +
  geom_line(aes(color = `Class (prob.)`, group = `Class (prob.)`)) +
  geom_point(aes(shape = `Class (prob.)`, color = `Class (prob.)`,
                 group = `Class (prob.)`)) +
  theme_light() +
  #xlab("Item responses") + ylab("Item response probabilities") +
  xlab("") + ylab("") +
  ggtitle("3-class LCA model") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

# 4 class model
plot_4classes <- m4_parameters %>%
  ggplot(aes(x = item, y = itemResponseP)) +
  geom_line(aes(color = `Class (prob.)`, group = `Class (prob.)`)) +
  geom_point(aes(shape = `Class (prob.)`, color = `Class (prob.)`,
                 group = `Class (prob.)`)) +
  theme_light() +
  #xlab("Item responses") + ylab("Item response probabilities") +
  xlab("") + ylab("") +
  ggtitle("4-class LCA model") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

# 5 class model
plot_5classes <- m5_parameters %>%
  ggplot(aes(x = item, y = itemResponseP)) +
  geom_line(aes(color = `Class (prob.)`, group = `Class (prob.)`)) +
  geom_point(aes(shape = `Class (prob.)`, color = `Class (prob.)`,
                 group = `Class (prob.)`)) +
  theme_light() +
  #xlab("Item responses") + ylab("Item response probabilities") +
  xlab("") + ylab("") +
  ggtitle("5-class LCA model") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

# 6 class model
plot_6classes <- m6_parameters %>%
  ggplot(aes(x = item, y = itemResponseP)) +
  geom_line(aes(color = `Class (prob.)`, group = `Class (prob.)`)) +
  geom_point(aes(shape = `Class (prob.)`, color = `Class (prob.)`,
                 group = `Class (prob.)`)) +
  theme_light() +
  #xlab("Item responses") + ylab("Item response probabilities") +
  xlab("") + ylab("") +
  ggtitle("6-class LCA model") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

# put the plots together
## combine all plots together into one figure
parameters_plot_grid <- grid.arrange(
  plot_2classes, plot_3classes, plot_4classes, plot_5classes, plot_6classes,
  ncol = 1, nrow = 5,
  top = text_grob("Conditional item probability plots for candidate models",
                  size = 15),
  bottom = textGrob("Items"),
  left = textGrob("Item response probabilities", rot=90))


# save the combined plot
ggsave(
  filename = paste0(getwd(), "/Plots/item_probability_plots_all_indicators_pain_only.png"),
  plot = parameters_plot_grid,
  units = "in",
  width = 12,
  height = 15,
  dpi = 1000
)

# save just the final 3-class solution plot
# first, add axis labels
plot_3classes <- plot_3classes + xlab("Items") +
  ylab("Item response probabilities") +
  theme(axis.title = element_text(size = 20),
        plot.title = element_text(size = 22),
        legend.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 18))
ggsave(
  filename = paste0(getwd(), "/Plots/item_probability_plots_pain_only_3class.png"),
  plot = plot_3classes,
  units = "in",
  width = 14,
  height = 8,
  dpi = 1000
)


#-----------------------------
# CONDUCT BLRTs
# Note: first load BLRT function from separate BLRT.R file

# calculate BLRT for each candidate model with 2-6 classes
tic()
BLRT_m2 <- BLRT(m1, m2, 100)
beep(5)
BLRT_m3 <- BLRT(m2, m3, 100)
beep(5)
BLRT_m4 <- BLRT(m3, m4, 100)
beep(5)
BLRT_m5 <- BLRT(m4, m5, 100)
beep(5)
BLRT_m6 <- BLRT(m5, m6, 100)
beep(5)
toc()


#------------------------------
# EXAMINE CLASSES

# assign class memberships from 3 class model posterior probabilities
df_cc$class_mem3 <- apply(m3$posterior,
                          MARGIN = 1,
                          FUN = function(x){which(x == max(x))})

### Create a descriptive Table 1 ###

# List variables to be included in the table
myVars <- c("diabetes", "lungDis", "hrtCond", "angina", "stroke", "arthritis",
            "maritalStatus", "foodSecurity", "gender", "householdSize", "numChildren",
            "veteranStatus", "randCESD", "urbanicity", "age", "race4Cats", "edu4Cats",
            "wealthQuarts", "region4Cats", "jobStatus4Cats", "cancerActive",
            "bmi6Cats", "smokeStatus", "insurance")

# List variables which are categorical
catVars <- c("diabetes", "lungDis", "hrtCond", "angina", "stroke", "arthritis",
             "maritalStatus", "foodSecurity", "gender", "veteranStatus", "urbanicity",
             "race4Cats", "edu4Cats", "wealthQuarts", "region4Cats", "jobStatus4Cats",
             "cancerActive", "bmi6Cats", "smokeStatus", "insurance")

# List continuous variables which should be displayed as median (IQR)
medVars <- c("householdSize", "numChildren", "randCESD", "age")

# Create table 1
tab1 <- CreateTableOne(vars = myVars, # set descriptive variables
                       strata = "class_mem3", # define stratifying variable
                       data = df_cc,
                       factorVars = catVars) # define categorical variables

# Print table 1
print(tab1,
      nonnormal = medVars,
      formatOptions = list(big.mark = ","),
      test = FALSE)
