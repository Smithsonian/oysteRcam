#######################
### missing_sites() ###
#######################

################################################################################

########################
### File Description ###
########################

# Filename: missing_sites.R
# Author: Sam McNeely
# Contributors: 
# Date Created: 11/08/2023
# Date Modified: 11/08/2023

# Purpose: 
# The purpose of this script is to create a function, missing_sites(), that 
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
# Name the function missing_sites()
missing_sites <- function() {
  
  # * The answer variable and its corresponding while loop are reassurances that
  # * the user input the correct entry.
  answer <- ''
  
  while(answer != "Y") {
    
    # create an empty vector, missing_sites, to store all of the site numbers
    missing_sites <- c()
    
    # create an empty string, n, that will be assigned to the user input
    input <- ''
    
    # use a while loop so that the user is prompted until a specific string, End,
    # is called to break the loop
    while(input != 'End') {
      
      # Prompt the user with 3 options: integers 1-999, the string End, and the 
      # string Remove. 1-999 adds a new site number to the sites vector, End
      # terminates the while loop ending site number additions to the sites 
      # vector, and Remove removes the most recent user entry from the sites 
      # vector.
      input <- readline(prompt = cat('\n\nDESCRIPTION           : [ENTRY]\nSite number           : [1-999]\nFinished adding sites : [End]\nRemove last entry     : [Remove]\n\nSite # : '))
      
      # if the user entered End, terminate the while loop
      if(input == 'End') {
        break
        
        # if the user entered Remove, remove the most recent entry from the sites
        # vector
      } else if(input == 'Remove') {
        missing_sites <- missing_sites[-length(missing_sites)]
        
        # add a warning message to ensure the user knows the most recent entry
        # was removed in case it was an accident
        cat('Warning: most recent entry removed')
        
        # if the user entered any assortment of characters, inform the user of an
        # invalid input and prompt them again
      } else if(is.na(as.numeric(input)) | (as.numeric(input) < 1000)) {
        cat('Error: invalid entry.\nTry again.')
        
        # add n (as a numeric, not a character) to the sites vector
      } else {
        missing_sites <- append(missing_sites, as.numeric(input))
      }
    }
    
    # Prompt the user to check that their input is correct
    answer <- readline(prompt = cat("\n\nThe sites with missing images are ", missing_sites, ". Is this correct? [Y/N]", sep = ""))
    
    # If user input is correct, break the main while loop
    if(answer == "Y") {
      break
      
      # If user input is incorrect, reprompt the user
    } else if(answer == "N") {
      cat('Missing sites are incorrect. Reprompting...\n\n')
    }
  }
}

################################################################################