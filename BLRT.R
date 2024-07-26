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
                               missval = FALSE, pctmiss = NULL)
  }

  # create lists to store LCA models fitted to the simulated datasets
  #boot_k_1 <- list()
  #boot_k <- list()

  # create a dataframe to store the Gsq values and difference for each set of
  # fitted models
  Gsqs_summary <- as.data.frame(matrix(NA, r, 3))
  names(Gsqs_summary) <- c("k_1", "k", "difference")

  # specify LCA model formula
  f = as.formula(cbind(Y1, Y2, Y3, Y4, Y5)~1)

  # fit k-1 class and k class LCA models to the simulated datasets and save
  set.seed(9406)
  for(i in 1:r){
    boot_k_1 <- poLCA(f, data = sims[[i]]$dat, nclass = k-1, maxiter = 500,
                           graphs = FALSE, tol = 1e-8, na.rm = TRUE,
                           nrep = 20, verbose = FALSE, calc.se = FALSE)

    boot_k <- poLCA(f, data = sims[[i]]$dat, nclass = k, maxiter = 500,
                         graphs = FALSE, tol = 1e-8, na.rm = TRUE,
                         nrep = 20, verbose = FALSE, calc.se = FALSE)

    # save the Gsq statistics from each fitted model
    Gsqs_summary$k_1[i] <- boot_k_1$Gsq
    Gsqs_summary$k[i] <- boot_k$Gsq
    Gsqs_summary$difference[i] <-  boot_k_1$Gsq - boot_k$Gsq
  }

  # find percentage of bootstrapped sample G^2 differences that the observed
  # difference is bigger than
  p_value <- sum(Gsqs_summary$difference - Gsq_diff < 0)/r

  # return p-value of BLRT
  return(p_value)
}
