#######################
### sampling_date_ui() ###
#######################

################################################################################

#######################
### sampling_date_ui() ###
#######################

# Filename: sampling_date_ui.R
# Author: Sam McNeely
# Contributors: 
# Date Created: 11/09/2023
# Date Modified: 11/23/2023

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
# Name the function camside()
sampling_date_ui <- function() {
  
  # * while loop #1 [Date entry and verification]
  # * The stop_1 variable and its corresponding while loop are reassurances that
  # * the user input the correct entry. This variable is the flag that tells the
  # * while loop to either continue or stop running.
  stop_1 <- FALSE
  
  while(stop_1 == FALSE) {
    
    # * Have the user input the date sampling occurred with the format MM/DD/YYYY
    # * Ask the user for month, day, and year
    
    # * while loop #2: nested while loop #1 [Month Entry]
    # * Have the user input the month in which sampling occurred 
    # stop_2 is the flag for when the user entry is valid, exit the current 
    # while loop
    stop_2 <- FALSE
    
    # Create a variable, poss_mm, to use as a check for user input validity
    poss_mm <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10',
                 '11', '12')
    
    while(stop_2 == FALSE) {
      month <- readline(prompt = "What month did sampling occur? [MM] ")
      
      # if month is a number between 01 and 12, break the current while loop
      if(month %in% poss_mm) {
        stop_2 <- TRUE
        
        # if month is NOT a number between 01 and 12, print an error message and 
        # prompt for user input again
      } else {
        cat('Error: invalid entry.\nEntry must be a number between 01-12 and must include two digits. [MM]')
      }
    }
    
    # * while loop #3: nested while loop #2 [Day Entry]
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
      day <- readline(prompt = "What day did sampling occur? [DD] ")
      
      # if day is a number between 1 and 31, break the while loop
      if(day %in% poss_dd) {
        stop_3 <- TRUE
        
        # if day is NOT a number between 1 and 31, print an error message and 
        # prompt for user input again
      } else {
        cat('Error: invalid entry.\nEntry must be a number between 01-31 and must include two digits. [DD]')
      }
    }
    
    # * while loop #4: nested while loop #3 [Year Entry]
    # * Have the user input the year in which sampling occurred 
    # stop_4 is the flag for when the user entry is valid, exit the current 
    # while loop
    stop_4 <- FALSE
    
    while(stop_4 == FALSE) {
      year <- readline(prompt = "What year did sampling occur? [YYYY] ")
      
      # if year is a number between 2020 and 3000, break the while loop
      if(as.numeric(year) %in% c(2020:3000)) {
        stop_4 <- TRUE
        
        # if year is NOT a number between 2020 and 3000, print an error message 
        # and prompt for user input again
      } else {
        cat('Error: invalid entry.\nEntry must be a valid four-digit number. [YYYY]')
      }
    }
    
    # Concatenate the entries into a single string separated by "/"
    date <- paste(month, day, year, sep = "/")
    
    # * while loop #5: nested while loop #4 [Date verification]
    # * Have the user check that the date entered is correct.
    # stop_5 is the flag for when the user entry is valid, exit the current 
    # while loop
    stop_5 <- FALSE
    
    while(stop_5 == FALSE) {
      # Prompt the user to check that their input is correct
      answer <- readline(prompt = cat("\n\nThe sampling date entered is ", date, ". Is this correct? [Y/N]", sep = ""))
      
      # If user input is correct, break the current while loop and the main loop
      if(answer == "Y") {
        stop_5 <- TRUE
        stop_1 <- TRUE
        
        # If user input is incorrect, break current loop and reprompt the user
      } else if(answer == "N") {
        cat('Date is incorrect. Reprompting...\n\n')
        stop_5 <- TRUE
        
        # If user input is not a "Y" or "N", output an error and reprompt
      } else {
        cat('Error: invalid entry.\nEntry must be "Y" for Yes or "N" for No.')
      }
    }
  }
}

################################################################################