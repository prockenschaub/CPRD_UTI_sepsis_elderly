###########################################################################
# Author:   Patrick Rockenschaub
# Project:  Preserve Antibiotics through Safe Stewardship (PASS)
#           Primary Care work package 1
#           COPD analysis
#
# File:     00_init.R
# Date:     07/11/2018
# Task:     Initialise the workspace for a clean new analysis of antibiotic
#           prescribing in COPD patients (i.e. remove previous objects, 
#           load libraries, etc.)
#
###########################################################################

.libPaths("../00_packages_3.4.3")

# Load the general environment (only if not called before)
if(exists(".conn")){
   disconnect_db()
}

# Load the base settings and functions
suppressMessages({
  source("00_init.R")
  source("00_basic_tables.R")
  source("00_code_lists.R")
})

# Load local functions
subfolder <- "00_replication/gharbi_2019"
source(file.path(subfolder, "00_functions.R"))

# Set path to store the derived datasets
der_dir <- "01_derived"
mat_dir <- "02_matched"


study_start <- ymd("2007-04-01")
study_end <- ymd("2015-03-31")