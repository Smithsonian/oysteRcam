#' missing_sites_ui
#'
#' @description
#' A user interface function that takes user input to obtain and verify sites
#' sites that were sampled but videos were not recording, therefore, no images
#' from said sites are available.
#'
#' @return A vector containing the sites where videos, and consequently, images
#' were not recorded.

################################################################################

##########################
### missing_sites_ui() ###
##########################

################################################################################

########################
### File Description ###
########################

# Filename: missing_sites_ui.R
# Author: Sam McNeely
# Contributors:
# Date Created: 11/27/2023
# Date Modified: 12/08/2023

# Purpose:
# The purpose of this script is to create a function, missing_sites_ui(), that
# takes user input through an interface prompted by the function itself. The
# user input will be site numbers for sites in which something went wrong with
# the recording and no video was recorded. This function will create a vector
# containing the numbers entered by the user. It can be used for both side A and
# B camera sites.

# Difference from PhotoRandomizer.R:
# This script takes user input (in this case, site numbers) and stores it in
# a vector that will later be used to create the Excel file for RAP scoring.

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

# * Create a function that takes user input to create a vector of the sites that
# * did NOT have recorded videos and are missing images.
# Name the function missing_sites_ui()
missing_sites_ui <- function() {

  # * Create a nested function that logically tests whether a variable, x, is a
  # * whole number or not.
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
    abs(x - round(x)) < tol
  }

  # * while loop 1 [Site entry and verification]
  # * The stop_1 variable and its corresponding while loop are reassurances that
  # * the user input the correct entry. This variable is the flag that tells the
  # * while loop to either continue or stop running.
  stop_1 <- FALSE

  while(stop_1 == FALSE) {

    # Before the user is prompted, let the user know that the sites entered
    # next are for the sites from which videos were not recorded.
    cat('\n\nUse the following prompt to enter the site numbers from which videos were NOT recorded.')

    # * while loop 1.1 [Site Entry]
    # * Have the user input the sites that were sampled
    # stop_2 is the flag for when the user entry is valid, exit the current
    # while loop
    stop_2 <- FALSE

    # create an empty vector, missing_sites, to store all of the site numbers
    missing_sites <- c()

    while(stop_2 == FALSE) {

      # Prompt the user with 3 options: integers 1-999, the string End, and the
      # string Remove. 1-999 adds a new site number to the sites vector, End
      # terminates the while loop ending site number additions to the sites
      # vector, and Remove removes the most recent user entry from the sites
      # vector.
      input_1 <- readline(prompt = cat('\n\nDESCRIPTION           : [ENTRY]\nSite #s               : [1-999]\nFinished adding sites : [End]\nRemove last entry     : [Remove]\n\nSite # :\n'))

      # if the user entered End, terminate the while loop
      if(input_1 == 'End') {
        stop_2 <- TRUE

        # if the user entered Remove, remove the most recent entry from the
        # sites vector
      } else if(input_1 == 'Remove') {
        missing_sites <- missing_sites[-length(missing_sites)]

        # add a warning message to ensure the user knows the most recent entry
        # was removed in case it was an accident
        cat('\n\nWarning: most recent entry removed.\n')

        # if the user entered any assortment of characters, inform the user of an
        # invalid input and prompt them again
      } else if(is.na(as.numeric(input_1)) | (as.numeric(input_1) >= 1000) | (!is.wholenumber(as.numeric(input_1)))) {
        cat('\n\nError: invalid entry.\nEntry must be a whole number between 1-999, "End", or "Remove".\n')

        # add n (as a numeric, not a character) to the sites vector
      } else {
        missing_sites <- append(missing_sites, as.numeric(input_1))
      }
    }

    # * while loop 1.2 [Site Verification]
    # * Have the user verify that the sites entered are all correct
    # stop_3 is the flag for when the user entry is valid, exit the current
    # while loop
    stop_3 <- FALSE

    while(stop_3 == FALSE) {
      cat("\n\nThe sites entered with missing images are:\n")
      cat(missing_sites, sep = ", ")
      # Prompt the user to check that their input is correct
      input_2 <- readline(prompt = cat("\n\nIs this correct? [Y/N]\n"))

      # If user input is correct (Y), break the current while loop by setting
      # stop_3 = TRUE and the main while loop by setting stop_1 = TRUE
      if(input_2 == "Y") {
        stop_1 <- TRUE
        stop_3 <- TRUE

        # Use assign() to assign missing_sites to the global environment
        assign(x = 'missing_sites', value = missing_sites, envir = .GlobalEnv)

        # If user input is incorrect (N), break the current while loop and
        # reprompt the user
      } else if(input_2 == "N") {
        stop_3 <- TRUE
        cat('\n\nSites are incorrect. Reprompting...\n')

        # If the user entered anything other than "Y" or "N", then print an
        # error invalid entry message and reprompt the user.
      } else {
        cat('\n\nError: invalid entry.\nEnter "Y" for Yes or "N" for No.\n')
      }
    }
  }
}

################################################################################
