#--------------------------------------------------------------------------
#
# Program: 00_init.R
# Author:  Patrick Rockenschaub
# Date:    07/12/2017
#
# Purpose: Initialise all study parameters, call utility functions, etc.
#
#--------------------------------------------------------------------------


# Create a clean slate ----------------------------------------------------
all_objs <- ls(all.names = TRUE)
all_objs <- all_objs[all_objs != "dir_root"]

remove(list = all_objs)


# Set paths ---------------------------------------------------------------

# Set the programme directories
root_dir <- file.path("S:/CALIBER_17_048")
db_dir <- file.path(root_dir, "00_database")
lu_dir <- file.path(root_dir, "00_caliber", "CPRD", "Lookups")

loc_lib <- "../00_packages"



# Establish database connection -------------------------------------------

if(!exists(".conn")){
  source(file.path(db_dir, "00_database.R"))
  init_db(file.path(db_dir, "00_db_connection.txt"), lib.loc = loc_lib)
  connect_db()
}


# Custom functions --------------------------------------------------------
source(file.path(root_dir, "00_utilities.R"))




# Study parameters --------------------------------------------------------

study_start <- ymd("2007-04-01")
study_end   <- ymd("2015-12-31")



# Aesthetics --------------------------------------------------------------

rd_blu <- RColorBrewer::brewer.pal(n = 8, name = "RdBu")
fill_col <- RColorBrewer::brewer.pal(n = 8, name = "Accent")[c(1:3, 5:8, 4)]
line_col <- RColorBrewer::brewer.pal(n = 8, name = "Set1")[c(3:5, 1:2, 7:8, 6)]
grey <- "#888888"
