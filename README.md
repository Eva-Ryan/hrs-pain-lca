# hrs-pain-lca
Identifying pain subgroups in the Health and Retirement Study using latent class analysis. This analysis forms the basis of Chapter 6 of my Ph.D. thesis.

**Note:** HRS data can be accessed by creating an account on the HRS website: https://hrsdata.isr.umich.edu/. This analysis is carried out using data from the RAND HRS Fatfiles for study waves 1992-2018 inclusive, the RAND HRS Longitudinal File 2020, and the Cross-Wave Census Region/Division and Mobility File.

## Description of files

1.  The file [Data preprocessing.R](https://github.com/Eva-Ryan/hrs-pain-lca/blob/main/Data%20preprocessing.R) contains code to load data from each data source (Fatfiles, Longitudinal File, and Census Region File), select and clean the variables to be used in the analysis, and merge the required variables into one dataframe.
2.  The file [LCA modelling - pain group only.R](https://github.com/Eva-Ryan/hrs-pain-lca/blob/main/LCA%20modelling%20-%20pain%20group%20only.R) contains code to fit a range of candidate LCA models, calculate and plot fit measures for each candidate model, and plot summaries of the latent classes identified for each candidate model.
3.  The file [BLRT.R](https://github.com/Eva-Ryan/hrs-pain-lca/blob/main/BLRT.R) contains code to run the bootstrapped likelihood ratio test to compare two candidate models with k-1 and k classes.
4.  The folder [Plots](https://github.com/Eva-Ryan/hrs-pain-lca/tree/main/Plots) contains the final fit index plots and LCA model summary plots for this analysis.
