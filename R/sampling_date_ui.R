#' sampling_date_ui
#'
#' @description
#' A user interface function that takes user input to define the date on which
#' sampling occurred.
#'
#' @return A date in MM-DD-YYYY format assigned to the global environment.

################################################################################

##########################
### sampling_date_ui() ###
##########################

################################################################################

############################
### Function Description ###
############################

# Filename: sampling_date_ui.R
# Author: Sam McNeely
# Contributors:
# Date Created: 11/09/2023
# Date Modified: 01/08/2024

# Purpose:
# The purpose of this script is to define the date on which sampling occurred.

# Difference from PhotoRandomizer.R:
# This script prompts the user to define the date in a specific format.

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

# * Create a function that takes user input to define the date on which sampling
# * occurred.
# Name the function sampling_date_ui()
sampling_date_ui <- function() {

  # * while loop 1 [Date entry and verification]
  # * The stop_1 variable and its corresponding while loop are reassurances that
  # * the user input the correct entry. This variable is the flag that tells the
  # * while loop to either continue or stop running.
  stop_1 <- FALSE

  while(stop_1 == FALSE) {

    # * Have the user input the date sampling occurred with the format MM/DD/YYYY
    # * Ask the user for month, day, and year

    # * while loop 1.1 [Month Entry]
    # * Have the user input the month in which sampling occurred
    # stop_2 is the flag for when the user entry is valid, exit the current
    # while loop
    stop_2 <- FALSE

    # Create a variable, poss_mm, to use as a check for user input validity
    poss_mm <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10',
                 '11', '12')

    while(stop_2 == FALSE) {
      month_i <- readline(prompt = cat("\n\nWhat month did sampling occur? [MM]\n"))

      # if month is a number between 01 and 12, break the current while loop
      if(month_i %in% poss_mm) {
        stop_2 <- TRUE

        # if month is NOT a number between 01 and 12, print an error message and
        # prompt for user input again
      } else {
        cat('\n\nError: invalid entry.\nEntry must be a number between 01-12 and must include two digits. [MM]\n')
      }
    }

    # * while loop 1.2 [Day Entry]
    # * Have the user input the day in which sampling occurred
    # stop_3 is the flag for when the user entry is valid, exit the current
    # while loop
    stop_3 <- FALSE

    # Create a variable, poss_dd, to use as a check for user input validity
    poss_dd <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10',
                 '11', '12', '13', '14', '15', '16', '17', '18', '19', '20',
                 '21', '22', '23', '24', '25', '26', '27', '28', '29', '30',
                 '31')

    while(stop_3 == FALSE) {
      day_i <- readline(prompt = cat("\n\nWhat day did sampling occur? [DD]\n"))

      # if day is a number between 1 and 31, break the while loop
      if(day_i %in% poss_dd) {
        stop_3 <- TRUE

        # if day is NOT a number between 1 and 31, print an error message and
        # prompt for user input again
      } else {
        cat('\n\nError: invalid entry.\nEntry must be a number between 01-31 and must include two digits. [DD]\n')
      }
    }

    # * while loop 1.3 [Year Entry]
    # * Have the user input the year in which sampling occurred
    # stop_4 is the flag for when the user entry is valid, exit the current
    # while loop
    stop_4 <- FALSE

    while(stop_4 == FALSE) {
      year_i <- readline(prompt = cat("\n\nWhat year did sampling occur? [YYYY]\n"))

      # if year is a number between 2020 and 3000, break the while loop
      if(as.numeric(year_i) %in% c(2020:3000)) {
        stop_4 <- TRUE

        # if year is NOT a number between 2020 and 3000, print an error message
        # and prompt for user input again
      } else {
        cat('\n\nError: invalid entry.\nEntry must be a valid four-digit number. [YYYY]\n')
      }
    }

    # Concatenate the entries into a single string separated by "/"
    date <- paste(month_i, day_i, year_i, sep = "/")

    # * while loop 1.4 [Date verification]
    # * Have the user check that the date entered is correct.
    # stop_5 is the flag for when the user entry is valid, exit the current
    # while loop
    stop_5 <- FALSE

    while(stop_5 == FALSE) {
      # Prompt the user to check that their input is correct
      answer <- readline(prompt = cat("\n\nThe sampling date entered is ", date, ". Is this correct? [Y/N]\n", sep = ""))

      # If user input is correct, break the current while loop and the main loop
      if(answer == "Y") {
        stop_5 <- TRUE
        stop_1 <- TRUE

        # Use assign() to assign date to the global environment
        assign(x = 'date', value = date, envir = .GlobalEnv)

        # If user input is incorrect, break current loop and reprompt the user
      } else if(answer == "N") {
        cat('\n\nDate is incorrect. Reprompting...\n')
        stop_5 <- TRUE

        # If user input is not a "Y" or "N", output an error and reprompt
      } else {
        cat('\nError: invalid entry.\nEntry must be "Y" for Yes or "N" for No.')
      }
    }
  }
}

################################################################################
