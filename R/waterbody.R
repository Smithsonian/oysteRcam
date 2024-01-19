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
# Date Modified: 01/16/2024

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

  # Great Wicomico River
  } else if(locabbrev == 'GW') {
    loc <- 'Great Wicomico River'

    # Herring Bay
  } else if(locabbrev == 'HB') {
    loc <- 'Herring Bay'

  # Lafayette River
  } else if(locabbrev == 'LF') {
    loc <- 'Lafayette River'

  # Little Choptank River
  } else if(locabbrev == 'LC') {
    loc <- 'Little Choptank River'

  # Lynnhaven River
  } else if(locabbrev == 'LYN') {
    loc <- 'Lynnhaven River'

  # Manokin River
  } else if(locabbrev == 'M') {
    loc <- 'Manokin River'

  # Piankatank River
  } else if(locabbrev == 'PK') {
    loc <- 'Piankatank River'

  # Rappahannock River
  } else if(locabbrev == 'RP') {
    loc <- 'Rappahannock River'
  } else if(locabbrev == 'RCC') {
    loc <- 'Carters Creek'
  } else if(locabbrev == 'RB') {
    loc <- 'Belle Isle'

  # South River
  } else if(locabbrev == 'OP') {
    loc <- 'South River'

  # St. Mary's River
  } else if(locabbrev == 'SM') {
    loc <- 'St. Marys River'

  # Tred Avon River
  } else if(locabbrev == 'T') {
    loc <- 'Tred Avon River'

    # York River
  } else if(locabbrev == 'LYK') {
    loc <- 'Low York River'
  }

  # Use assign() to assign loc to the global environment
  assign(x = 'loc', value = loc, envir = .GlobalEnv)

}

################################################################################
