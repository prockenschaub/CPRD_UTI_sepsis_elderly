# Antibiotics for lower UTI in the elderly
This is the R code and code lists used to analyse the data for paper "Antibiotic prescribing for lower UTI in elderly patients in primary care and risk of bloodstream infection: a cohort study using electronic health records". TODO: add DOI

## Repository structure

This project contains the code used to produce all tables and figure in the above cited paper. The code was used within a secure data safe haven and was extracted after the final analysis was performed.

The main analysis files are prefaced with numbers and are ordered according to the sequence in which they were applied (00-05):

0. *init*: Set up the general analysis environment needed to perform each future step (e.g. loading packages, setting up the database connection, ...). This file does not have to be executed directly but is called individually by each further step.
1. *extract_data*: Extract the subset of all necessary patient information from the database containing the raw CPRD-HES-ONS data for our cohort. 
2. *derive_tables*: Bring the extracted raw data into the format needed for the analysis. In particular, apply our definition of an episode to derive incident episodes of lower UTI from the list of raw diagnosis codes.
3. *base_analysis*: Perform the main analysis presented in the paper, i.e. the descriptive and multivariate analysis of the association between antibiotic prescribing for lower UTI and subsequent diagnosis of sepsis.
4. *ps_analysis*: Perform the sensitivity analysis using matched and inverse-probability weighted propensity score analysis using logistic regression and boosting trees.
5. *describe_sepsis_diagnosis*: Do additional post-analysis on the type of sepsis picked up by the algorithm.

Additional utitily functions needed to access the database and performing common tasks are contained in the folder named `project_root`. This code was shared between multiple projects, and was originally kept within a root folder within which this particular analysis was contained as a subfolder. The paths to those files have been changed within he code after extracting it from the data safe haven, therefore there might have been errors introduced. Please raise an issue within this repository if you detect an error.

The code lists used in this analysis were adopted from Gharbi et al. (2019) [https://doi.org/10.1136/bmj.l525](https://doi.org/10.1136/bmj.l525). These lists will be published with the paper and can also be found in the subfolder `codelists`.   

## Data structure
 Unfortunately, no actual data can be provided with this repository, as we do not own the data. Ethical approval to CPRD is required to obtain data for the same or similar time frames.
 
 TODO: add a description of the SQL table structure
