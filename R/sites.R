#' sites
#'
#' @description
#' A function that iterates through each file in the GoPro image folder with the
#' path defined by camdir() and extracts the site numbers.
#'
#' @return A vector of site numbers. This vector is assigned to the Global
#' Environment.

################################################################################

########################
### File Description ###
########################

# Filename: sites.R
# Author: Sam McNeely
# Contributors:
# Date Created: 11/08/2023
# Date Modified: 01/10/2023

################################################################################

###########################
### Function Definition ###
###########################
#
# Notes:
#
# Do NOT modify the function definitions. Simply run through each function to
# create it (Ctrl + Enter).

# * Create a function that takes user input to create a vector of the sites that
# * had recorded videos and have had still images extracted from said videos.
# Name the function sites()
sites <- function() {

  filenames <- list.files(cdir)

  sites <- c()

  for(i in 1:length(filenames)) {
    num <- as.numeric(str_extract(string = filenames[i], pattern = '[0-9]+'))
    sites <- append(x = sites, values = num, after = length(sites))
  }

  assign(x = "sites", value = sites, envir = .GlobalEnv)

}

################################################################################
