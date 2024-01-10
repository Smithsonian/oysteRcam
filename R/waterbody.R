#' waterbody
#'
#' @description
#' A function that converts the site abbreviation identifier into the location's
#' name.
#'
#' @return The location name associated with the site abbreviation identifier.
#' This object is assigned to the Global Environment.

################################################################################

########################
### File Description ###
########################

# Filename: waterbody.R
# Author: Sam McNeely
# Contributors:
# Date Created: 11/28/2023
# Date Modified: 01/09/2024

# Place in the data collection process:
# This R script is meant to be run after images have been extracted from the
# GoPro videos and before the images are qualitatively scored.

################################################################################

###########################
### Function Definition ###
###########################
#
# Notes:
#
# Do NOT modify the function definitions. Simply run through the function to
# create it (Ctrl + Enter).

# * Create a function that defines the water body.
# Name the function waterbody()
waterbody <- function() {

  # * Define each location.
  # Choptank River
  if(locabbrev == 'B') {
    loc <- 'Broad Creek'
  } else if(locabbrev == 'H') {
    loc <- 'Harris Creek'

  # Rappahannock River
  } else if(locabbrev == 'RCC') {
    loc <- 'Rappahannock Carters Creek'
  } else if(locabbrev == 'RB') {
    loc <- 'Rappahannock Belle Isle'

  # South River
  } else if(locabbrev == 'OP') {
    loc <- 'South River'
  }

  # Use assign() to assign loc to the global environment
  assign(x = 'loc', value = loc, envir = .GlobalEnv)

}

################################################################################
