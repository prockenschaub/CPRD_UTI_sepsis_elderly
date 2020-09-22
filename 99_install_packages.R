
# Use checkpoint to install the same version of each package --------------

# Load (and install) the checkpoint package
if (!require("checkpoint")) {
  install.packages(
    "checkpoint", 
    repos = "https://cran.microsoft.com/snapshot/2019-08-23/", 
  )
}

library(checkpoint)

# Set checkpoint parameters
R_version <- "3.6.3"
snapshot_date <- "2019-08-23"

# Create a CRAN checkpoint
checkpoint(snapshot_date, R.version = R_version)
