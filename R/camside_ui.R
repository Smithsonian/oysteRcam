#' camside_ui
#'
#' @description
#' A user interface function that takes user input to obtain and verify the
#' GoPro camera side being scored.
#'
#' @return The camera side: A or B.

################################################################################

########################
### File Description ###
########################

# Filename: camside_ui.R
# Author: Sam McNeely
# Contributors:
# Date Created: 11/17/2023
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
# Do NOT modify the function definitions. Simply run through each function to
# create it (Ctrl + Enter).

# * Create a function that takes user input to define which camera is being
# * evaluated first to create the spreadsheet variables.
# Name the function camside_ui().
camside_ui <- function() {

  # * while loop 1 [Camera side entry and verification]
  # * The stop_1 variable and its corresponding while loop are reassurances that
  # * the user input the correct entry. This variable is the flag that tells the
  # * while loop to either continue or stop running.
  stop_1 <- FALSE

  while(stop_1 == FALSE) {

    # * while loop 1.1 [Camera side entry]
    # * Have the user input which camera side is being entered with the only
    # * options being "A" or "B".
    # stop_1.1 is the flag for when the user entry is valid, exit the current
    # while loop.
    stop_1.1 <- FALSE

    # Use a while loop to prompt for user input until a specific entry "A" or
    # "B" is entered.
    while(stop_1.1 == FALSE) {
      side <- readline(prompt = cat("\n\nWhich camera side are you entering? [A/B]\n"))

      # If side equals "A" or "B", break the while loop.
      if(side == "A" | side == "B") {
        stop_1.1 <- TRUE

        # If side does NOT equal "A" or "B", print an error message and prompt
        # for user input again.
      } else {
        cat('\n\nError: invalid entry.\nEnter "A" or "B".\n')
      }
    }

    # * while loop 1.2 [Camera side verification]
    # * Have the user check that the camera side entered is correct.
    # stop_1.2 is the flag for when the user entry is valid, exit the current
    # and main while loops.
    stop_1.2 <- FALSE

    while(stop_1.2 == FALSE) {
      # Prompt the user to check that their input is correct
      answer <- readline(prompt = cat("\n\nYou are evaluating camera side ", side, ". Is this correct? [Y/N]\n", sep = ""))

      # If user input is correct, break the current and main while loop.
      if(answer == "Y") {
        stop_1.2 <- TRUE
        stop_1 <- TRUE

        # Return side.
        return(side)

        # If user input is incorrect, reprompt the user.
      } else if(answer == "N") {
        cat('\n\nCamera side is incorrect. Reprompting...\n')
        stop_1.2 <- TRUE

        # If user input is neither "Y" nor "N", display error message and
        # reprompt.
      } else {
        cat('\n\nError: invalid entry.\nEnter "Y" or "N".\n')
      }
    }
  }
}

################################################################################
