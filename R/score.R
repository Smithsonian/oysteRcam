###############
### score() ###
###############

################################################################################

########################
### File Description ###
########################

# Filename: score.R
# Author: Sam McNeely
# Contributors: 
# Date Created: 12/20/2023
# Date Modified: 12/21/2023

# Purpose: 
# The purpose of this script is to score the images in R and then it will output
# a CSV with the scored data.

# Place in the data collection process:
# This R script is meant to be run after images have been extracted from the 
# GoPro videos.

################################################################################

###########################
### Function Definition ###
###########################
#
# Notes:
#
# Do NOT modify the function definitions. Simply run through each function to
# create it (Ctrl + Enter).

# * Create a function that takes user input to define which camera is being 
# * evaluated first to create the spreadsheet variables.
# Name the function score()
score <- function() {
  
  # Use camdir() to establish the path to the folder of images to be scored.
  camdir()
  
  # Use waterbody() to set the location of the samples.
  waterbody()
  
  
  
}
