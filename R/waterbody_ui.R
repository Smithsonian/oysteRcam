#' waterbody_ui
#'
#' @description
#' A user interface function that takes user input define the location where
#' sampling occurred.
#'
#' @return The sampling location.

################################################################################

########################
### File Description ###
########################

# Filename: waterbody_ui.R
# Author: Sam McNeely
# Contributors:
# Date Created: 11/28/2023
# Date Modified: 01/11/2024

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
# Name the function waterbody_ui().
waterbody_ui <- function() {

  # * while loop 1 [Water body entry and verification]
  # * The stop_1 variable and its corresponding while loop are reassurances that
  # * the user input the correct entry. This variable is the flag that tells the
  # * while loop to either continue or stop running.
  stop_1 <- FALSE

  while(stop_1 == FALSE) {

    # Prompt the user for the name of the water body location.
    location <- readline(prompt = cat("\nWhat is the name of the water body that was sampled? "))

    # * while loop 1.1 [Water body verification]
    # * Have the user input the month in which sampling occurred.
    # stop_1.1 is the flag for when the user entry is valid, exit the current
    # while loop.
    stop_1.1 <- FALSE

    while(stop_1.1 == FALSE) {
      # Prompt the user to check that their input is correct.
      answer <- readline(prompt = cat("\n\nThe water body sampled is ", location, ". Is this correct? [Y/N]", sep = ""))

      # If user input is correct, break the current and main while loops.
      if(answer == "Y") {
        stop_1 <- TRUE
        stop_1.1 <- TRUE

        # Return location.
        return(location)

        # If user input is incorrect, break current loop and reprompt the user.
      } else if(answer == "N") {
        cat('\n\nDate is incorrect. Reprompting...\n')
        stop_1.1 <- TRUE

        # If user input is not a "Y" or "N", output an error and reprompt.
      } else {
        cat('\n\nError: invalid entry.\nEntry must be "Y" for Yes or "N" for No.\n')
      }
    }
  }
}

################################################################################
