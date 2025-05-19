## This Script checks the installs any required packages for the 
#   R scripts used in this project repository 


##############################################
## Set working directory to the repository  ##
##############################################

#list of required R packages
required_pkgs <- c(
  "tidyr",
  "dplyr",
  "janitor",
  "here",
  "rstudioapi"
)

# get packages that aren’t installed 
new_pkgs <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]

# Install any packages 
if (length(new_pkgs) > 0) {
  install.packages(new_pkgs, 
                   repos = "https://cloud.r-project.org")
}

# Load all the packages
invisible(lapply(required_pkgs, library, character.only = TRUE))

#Set working directory to this script's location (on RStudio only)
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  current_path <- rstudioapi::getActiveDocumentContext()$path #get the current path
  setwd(dirname(dirname(current_path))) #goes up two levels and sets wd
  message("Working directory set to: ", getwd())
} else {
  warning("rstudioapi not available. Please set the working directory manually.")
}




