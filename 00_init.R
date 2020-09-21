###########################################################################
# Author:   Patrick Rockenschaub
# Project:  Preserve Antibiotics through Safe Stewardship (PASS)
#           Primary Care work package 1
#           Gharbi et al. re-analysis
#
# File:     00_init.R
# Date:     07/11/2018
# Task:     Initialise the workspace for a clean new analysis of antibiotic
#           prescribing in COPD patients (i.e. remove previous objects, 
#           load libraries, etc.)
#
###########################################################################

loc_lib <- "../00_packages"
.libPaths(loc_lib)


# Set the programme directories
root_dir <- file.path("S:/CALIBER_17_048")
db_dir <- file.path(root_dir, "00_database")
lu_dir <- file.path(root_dir, "00_caliber", "CPRD", "Lookups")

# Set path to store the derived datasets
der_dir <- "01_derived"
mat_dir <- "02_matched"

# Load global functions
source(file.path(root_dir, "00_utilities.R"))

# Establish database connection 
if(!exists(".conn")){
  source(file.path(db_dir, "00_database.R"))
  init_db(file.path(db_dir, "00_db_connection.txt"), lib.loc = loc_lib)
  connect_db()
}

# Load the base settings and functions
suppressMessages({
  source("00_basic_tables.R")
  source("00_code_lists.R")
})

# Load local functions
source(file.path(subfolder, "00_functions.R"))

# Set study parameters
study_start <- ymd("2007-04-01")
study_end <- ymd("2015-03-31")