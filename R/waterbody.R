###################
### waterbody() ###
###################

################################################################################

########################
### File Description ###
########################

# Filename: waterbody.R
# Author: Sam McNeely
# Contributors: 
# Date Created: 11/28/2023
# Date Modified: 01/07/2024

# Purpose: 
# The purpose of this script is to define the body of water in which samples
# were taken.

# Difference from PhotoRandomizer.R:
# This script creates an object containing the name of the water body sampled.

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
    loc <- 'Rappahannock Bell Isle'
  
  # South River
  } else if(locabbrev == 'OP') {
    loc <- 'South River'
  } 
  
  # Use assign() to assign loc to the global environment
  assign(x = 'loc', value = loc, envir = .GlobalEnv)
  
}

################################################################################