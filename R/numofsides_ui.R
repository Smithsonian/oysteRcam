#' numofsides_ui
#'
#' @description
#' A user interface function that takes user input set the iterative counter
#' designating how many camera sides are to be analyzed.
#'
#' @return The number of camera sides to be analyzed.

################################################################################

########################
### File Description ###
########################

# Filename: numofsides_ui.R
# Author: Sam McNeely
# Contributors:
# Date Created: 01/11/2024
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
# Name the function numofsides_ui().
numofsides_ui <- function() {

  # * while loop 1 [Camera side amount and verification]
  # * The stop_1 variable and its corresponding while loop are reassurances that
  # * the user input the correct entry. This variable is the flag that tells the
  # * while loop to either continue or stop running.
  stop_1 <- FALSE

  while(stop_1 == FALSE) {

    # * while loop 1.1 [Camera side amount]
    # * Have the user input how many camera sides are being entered with the
    # * options being 1 or 2.
    # stop_1.1 is the flag for when the user entry is valid, exit the current
    # while loop.
    stop_1.1 <- FALSE

    # Use a while loop to prompt for user input until a specific entry 1 or 2 is
    # entered.
    while(stop_1.1 == FALSE) {
      # Prompt the user for the number of sides to be analyzed.
      numsides <- readline(prompt = cat("\n\nHow many sides are being analyzed? [1/2]\n"))

      # If the entry is a 1 or 2, then exit the current while loop.
      if(numsides == '1' | numsides == '2') {
        stop_1.1 <- TRUE

        # If the entry is not a 1 or 2, then print an error message and reprompt.
        # If the entry is a 1 or 2, then exit the current while loop.
      } else {
        cat('\n\nError: invalid entry.\nEnter "1" or "2".\n')
      }
    }

    # * while loop 1.2 [Camera side amount verification]
    # * Have the user answer if the current amount of sides to be analyzed is
    # * correct.
    # stop_1.2 is the flag for when the user entry is valid, exit the current
    # and main while loop.
    stop_1.2 <- FALSE

    # Use a while loop to prompt for user input until a specific entry "Y" or
    # "N" is entered.
    while(stop_1.2 == FALSE) {
      # Ask the user if the number of camera sides is correct.
      side_answer <- readline(prompt = cat("\n\nYou are analyzing ", numsides, " sides. Is that correct? [Y/N]\n", sep = ""))

      # If the entry is "N", set stop_1.2 = TRUE to exit the current while loop,
      # but by keeping stop_1 = FALSE, the main while loop is rerun.
      if(side_answer == "N") {
        stop_1.2 <- TRUE
        cat('\n\nNumber of sides to be analyzed is incorrect. Reprompting...\n')

        # If the entry is "Y", set stop_1.2 = TRUE to exit the current while loop
        # and stop_1 = TRUE to exit the main while loop.
      } else if(side_answer == "Y") {
        stop_1 <- TRUE
        stop_1.2 <- TRUE

        # Return numsides
        return(numsides)

        # If the entry is not a "Y" or "N", then print an error message and
        # reprompt.
      } else {
        cat('\n\nError: invalid entry.\nEnter "Y" for Yes or "N" for No.\n')
      }
    }
  }
}

################################################################################
