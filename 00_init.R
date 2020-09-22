###########################################################################
# Author:   Patrick Rockenschaub
# Project:  Preserve Antibiotics through Safe Stewardship (PASS)
#           Primary Care work package 1
#           Gharbi et al. re-analysis
#
# File:     00_init.R
# Date:     07/11/2018
# Task:     Initialise the workspace for a clean new analysis of antibiotic
#           prescribing in UTI patients (i.e. remove previous objects, 
#           load libraries, etc.)
#
###########################################################################

# NOTE: the following changes were made to this document to allow the main
#       code to run outside the Data Safe Haven environment it was 
#       originally run in
#
# (1) Reset the library path to the default system library path. Use 
#     checkpoint package instead.
# (2) Set a different path to the files that were originally located in 
#     the project root (because they were shared between multiple analyses)
# (3) Disable calls to the database with the raw data


# (1) Reset library path:
# loc_lib <- "00_packages"
# .libPaths(loc_lib)
library(checkpoint)
checkpoint("2019-08-23", R.version = "3.6.3")

# Set the programme directories
root_dir <- file.path("project_root") # (2) Change paths
db_dir <- file.path(root_dir) # (2) Change paths
lu_dir <- file.path(root_dir) # (2) Change paths

# Set path to store the derived datasets
der_dir <- "01_derived"
mat_dir <- "02_matched"

# Load global functions
source(file.path(root_dir, "00_utilities.R"))

# Establish database connection 

# (3) Disable database calls
# if(!exists(".conn")){
#   source(file.path(root_dir, "00_database.R"))
#   init_db(file.path(root_dir, "00_db_connection.txt"), lib.loc = loc_lib)
#   connect_db()
# }

# Load the base settings and functions
suppressMessages({
  source(file.path(root_dir, "00_basic_tables.R"))
  source(file.path(root_dir, "00_code_lists.R"))
})

# Load local functions
source(file.path("00_functions.R")) # (2) Change paths

# Set study parameters
study_start <- ymd("2007-04-01")
study_end <- ymd("2015-03-31")