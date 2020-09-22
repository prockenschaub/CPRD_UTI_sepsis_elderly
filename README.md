# Antibiotics for lower UTI in the elderly
This is the R code and code lists used to analyse the data for the paper "Antibiotic prescribing for lower UTI in elderly patients in primary care and risk of bloodstream infection: A cohort study using electronic health records in England". https://doi.org/10.1371/journal.pmed.1003336

## Repository structure

This project contains the code used to produce all tables and figure in the above cited paper. The code was used within a secure data safe haven and was extracted after the final analysis was performed.

The main analysis files are prefaced with numbers and are ordered according to the sequence in which they were applied (00-05):

0. *init*: Set up the general analysis environment needed to perform each future step (e.g. loading packages, setting up the database connection, ...). This file does not have to be executed directly but is called individually by each further step.
1. *extract_data*: Extract the subset of all necessary patient information from the database containing the raw CPRD-HES-ONS data for our cohort. 
2. *derive_tables*: Bring the extracted raw data into the format needed for the analysis. In particular, apply our definition of an episode to derive incident episodes of lower UTI from the list of raw diagnosis codes.
3. *base_analysis*: Perform the main analysis presented in the paper, i.e. the descriptive and multivariate analysis of the association between antibiotic prescribing for lower UTI and subsequent diagnosis of sepsis.
4. *ps_analysis*: Perform the sensitivity analysis using matched and inverse-probability weighted propensity score analysis using logistic regression and boosting trees.
5. *describe_sepsis_diagnosis*: Do additional post-analysis on the type of sepsis picked up by the algorithm.

Files prefaces by 99 contain code meant to aid the reproducibility of the results (see section on Reproducibility below).

Additional utitily functions needed to perform common tasks are contained in the folder named `project_root`. This code was shared between multiple projects, and was originally kept within a root folder within which this particular analysis was contained as a subfolder. The paths to those files have been changed within he code after extracting it from the data safe haven, therefore there might have been errors introduced. Please raise an issue within this repository if you detect an error.

The code lists used in this analysis were adopted from Gharbi et al. (2019) [https://doi.org/10.1136/bmj.l525](https://doi.org/10.1136/bmj.l525). These lists will be published with the paper and can also be found in the subfolder `codelists`.   

## Reproducibility

### Data structure
Code to simulate the dataset that would have been created after running `01_extract_data.R` and `02_derive_tables.R` is provided in the `99_simulate_data.R` file. This data can be used to test the code in `03_base_analysis.R(md)` and `04_ps_analysis.R(md)`. The simulation was set up such that marginal distributions of each variable agree with the actual data. No joint distributions and relationships between variables were taken into account when creating this simulation, so results **will** differ from those presented in the paper. 

### R version and packages
All analysis was performed using R version 3.6.1 (as mentioned in the paper), although some packages were added later and compiled under R version 3.6.3. The file `99_install_packages.R` downloads all necessary packages via the [checkpoint](https://cran.r-project.org/web/packages/checkpoint/index.html) package. We recommend that the user installs R version 3.6.3 to do so (and not a later version > 3, since package functionality might have changed). 

**Note:** We experienced some problems when using the checkpoint package in RStudio. It appears that R studio sometimes is loading certain packages relating to tidyverse or knitr in the background on start-up. This results in an error that states that a package has been already loaded and can't be unloaded. If you experience an issue like this, we recommend that you clear your environment and restart the R session. This usually resolved the issue.
