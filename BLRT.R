# Code to carry out the BLRT to compare model fit of candidate LCA models with
# k and k-1 classes.

# Author: Eva Ryan
# Date: July 2024
#------------------------------------------------------------------------------

BLRT <- function(lca_model_k_1, lca_model_k, r){
  # Inputs: lca_model_k_1 = k-1 class model fitted with poLCA function
  #         lca_model_k = k class model fitted with poLCA function
  #         r = number of bootstrapped samples to generate
  # Output: p_value = BLRT p-value

  # load required package
  require(poLCA)

  # verify that lca_model_k_1 has one fewer classes than lca_model_k
  if(length(lca_model_k$P) - length(lca_model_k_1$P) != 1){
    stop("ERROR: lca_model_k_1 must have one fewer classes than lca_model_k")
  }

  # extract G^2 values for the k-1 and k class models
  Gsq_k_1 <- lca_model_k_1$Gsq
  Gsq_k <- lca_model_k$Gsq

  # calculate Gsq difference for the two fitted models
  Gsq_diff <- Gsq_k_1 - Gsq_k

  # extract number of classes k for larger model
  k <- length(lca_model_k$P)

  # create list to store simulate datasets
  sims <- list()

  # generate r "bootstrapped" datasets from the smaller k-1 class model
  set.seed(6049)
  for(i in 1:r){
    sims[[i]] <- poLCA.simdata(N = lca_model_k_1$Nobs,
                               probs = lca_model_k_1$probs,
                               x = NULL, niv = 0, b = NULL,
                               missval = FALSE, pctmiss = NULL)$dat
  }

  # create a dataframe to store the Gsq values and difference for each set of
  # fitted models
  Gsqs_summary <- as.data.frame(matrix(NA, r, 3))
  names(Gsqs_summary) <- c("k_1", "k", "difference")

  # specify LCA model formula
  f = as.formula(cbind(Y1, Y2, Y3, Y4, Y5)~1)

  # fit k-1 class and k class LCA models to the simulated datasets
  set.seed(9406)
  boot_k_1 <- lapply(X = sims, FUN = function(df){
    poLCA(f, data = df, nclass = k-1, maxiter = 1000,
          graphs = FALSE, tol = 1e-8, na.rm = TRUE,
          nrep = 50, verbose = FALSE, calc.se = FALSE)
  })
  set.seed(9406)
  boot_k <- lapply(X = sims, FUN = function(df){
    poLCA(f, data = df, nclass = k, maxiter = 1000,
          graphs = FALSE, tol = 1e-8, na.rm = TRUE,
          nrep = 50, verbose = FALSE, calc.se = FALSE)
  })

  # extract Gsq value for each fitted model
  Gsq_boot_k_1 <- lapply(X = boot_k_1, FUN = function(df){
    df$Gsq
  })
  Gsq_boot_k <- lapply(X = boot_k, FUN = function(df){
    df$Gsq
  })

  # save the Gsq statistics from each fitted model
  Gsqs_summary$k_1 <- unlist(Gsq_boot_k_1)
  Gsqs_summary$k <- unlist(Gsq_boot_k)
  Gsqs_summary$difference <-  Gsqs_summary$k_1 -  Gsqs_summary$k


# find percentage of bootstrapped sample G^2 differences that the observed
# difference is smaller than
p_value <- sum(Gsqs_summary$difference - Gsq_diff > 0)/r

# return p-value of BLRT
return(p_value)
}
