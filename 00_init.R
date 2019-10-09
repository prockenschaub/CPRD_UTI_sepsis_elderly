###########################################################################
# Author:   Patrick Rockenschaub
# Project:  Preserve Antibiotics through Safe Stewardship (PASS)
#           Primary Care work package 1
#
# File:     00_init.R
# Date:     07/11/2018
# Task:     Initialise the workspace for a clean new analysis
#
###########################################################################

.libPaths("../00_packages_3.4.3") # Path to the installed R packages

# Load the general environment (only if not called before)
if(exists(".conn")){
   disconnect_db()
}

# Load the base settings and functions
root_folder <- "project_root"
suppressMessages({
  source(file.path(root_folder, "00_init.R"))
  source(file.path(root_folder, "00_basic_tables.R"))
  source(file.path(root_folder, "00_code_lists.R"))
})

# Load local functions
subfolder <- "."
source(file.path(subfolder, "00_functions.R"))

# Set path to store the derived datasets
der_dir <- "01_derived"
mat_dir <- "02_matched"


study_start <- ymd("2007-04-01")
study_end <- ymd("2015-03-31")