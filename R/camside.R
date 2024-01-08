#################
### camside() ###
#################

################################################################################

#################
### camside() ###
#################

# Filename: camside.R
# Author: Sam McNeely
# Contributors: 
# Date Created: 11/09/2023
# Date Modified: 11/17/2023

# Purpose: 
# The purpose of this script is to establish which camera side is being
# evaluated.

# Difference from PhotoRandomizer.R:
# This script prompts the user to define the camera side being evaluated.

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
# Do NOT modify the function definitions. Simply run through each function to
# create it (Ctrl + Enter).

# * Create a function that takes user input to define which camera is being 
# * evaluated first to create the spreadsheet variables.
# Name the function camside()
camside_ui <- function() {
  
  # * The stop_1 variable and its corresponding while loop are reassurances that
  # * the user input the correct entry. This variable is the flag that tells the
  # * while loop to either continue or stop running.
  stop_1 <- FALSE
  
  while(stop == FALSE) {
    # * Have the user input which camera side is being entered with the only
    # * options being "a" or "a"
    side <- ''
  
    # use a while loop to prompt for user input until a specific entry "a" or "b"
    # is entered
    while(side != "a" | side != "b") {
      side <- readline(prompt = "Which camera side are you entering? [a/b] ")
    
      # if side equals "a" or "b", break the while loop
      if(side == "a" | side == "b") {
        break
      
        # if side does NOT equal "a" or "b", print an error message and prompt
        # for user input again
      } else {
        cat('Error: invalid entry.\nEnter "a" or "b".')
      }
    }
    
    # Prompt the user to check that their input is correct
    answer <- readline(prompt = cat("\n\nYou are evaluating camera side ", side, ". Is this correct? [Y/N]", sep = ""))
  
    # If user input is correct, break the main while loop
    if(answer == "Y") {
      break
      
      # If user input is incorrect, reprompt the user
    } else if(answer == "N") {
      cat('Camera side is incorrect. Reprompting...\n\n')
    } else {
      cat('Error: invalid entry.\nEnter "Y" or "N".\n\n')
    }
  }
}

################################################################################