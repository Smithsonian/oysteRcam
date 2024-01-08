##################
### sites_ui() ###
##################

################################################################################

########################
### File Description ###
########################

# Filename: sites_ui.R
# Author: Sam McNeely
# Contributors: 
# Date Created: 11/08/2023
# Date Modified: 11/09/2023

# Purpose: 
# The purpose of this script is to create a function, sites(), that takes user
# input through an interface prompted by the function itself. The user input 
# will be site numbers for sites in which a video was recorded and a still image 
# was extracted successfully. This function will create a vector containing the 
# numbers entered by the user. It can be used for both side A and B camera 
# sites.

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
# * had recorded videos and have had still images extracted from said videos.
# Name the function sites()
sites <- function() {
  
  # * The answer variable and its corresponding while loop are reassurances that
  # * the user input the correct entry.
  # This variable is called stop because it flag the while loop in a call later
  # in the script to change it to TRUE, which then halts the while loop because
  # TRUE != FALSE.
  # Initially set stop = FALSE
  stop <- FALSE
  
  while(stop == FALSE) {
    
    # create an empty vector, sites, to store all of the site numbers
    sites <- c()
    
    # create an empty string, n, that will be assigned to the user input
    input_1 <- ''
    
    # * while loop label: site input_1
    # use a while loop so that the user is prompted until a specific string, 
    # End, is called to break the loop
    while(input_1 != 'End') {
      
      # Prompt the user with 3 options: integers 1-999, the string End, and the 
      # string Remove. 1-999 adds a new site number to the sites vector, End
      # terminates the while loop ending site number additions to the sites 
      # vector, and Remove removes the most recent user entry from the sites 
      # vector.
      input_1 <- readline(prompt = cat('\n\nDESCRIPTION           : [ENTRY]\nSite number           : [1-999]\nFinished adding sites : [End]\nRemove last entry     : [Remove]\n\nSite # : '))
      
      # if the user entered End, terminate the while loop
      if(input_1 == 'End') {
        break
        
        # if the user entered Remove, remove the most recent entry from the 
        # sites vector
      } else if(input_1 == 'Remove') {
        sites <- sites[-length(sites)]
        
        # add a warning message to ensure the user knows the most recent entry
        # was removed in case it was an accident
        cat('Warning: most recent entry removed')
        
        # If the user entered any assortment of characters or a number greater 
        # than or equal to 1000, inform the user of an invalid input and prompt 
        # them again.
      } else if(is.na(as.numeric(input_1)) | (as.numeric(input_1) >= 1000)) {
        cat('Error: invalid entry.\nTry again.')
        
        # add n (as a numeric, not a character) to the sites vector
      } else {
        sites <- append(sites, as.numeric(input_1))
      }
    }
    
    # * Ask the user if the sites are correctly entered.
    # create an empty string to be able to enter the while loop
    input_2 <- ''
    
    # The user should enter either "Y" or "N", so if they enter anything else,
    # then they will continue to be prompted for a correct entry until they
    # successfully do so.
    while(input_2 != "Y" | input_2 != "N") {
      
      # Prompt the user to check that their input is correct
      input_2 <- readline(prompt = cat("\n\nThe sites entered are ", sites, ". Is this correct? [Y/N]", sep = ""))
      
      # If user input is correct (Y), break the main while loop by setting stop
      # = TRUE
      if(input_2 == "Y") {
        stop <- TRUE
        
        # If user input_1 is incorrect, reprompt the user
      } else if(input_2 == "N") {
        cat('\n\nSites are incorrect. Reprompting...\n\n')
        
        # If the user entered anything other than "Y" or "N", then print an 
        # error invalid entry message and reprompt the user.
      } else {
        cat('\nError: invalid entry.\nEnter "Y" for Yes or "N" for No.')
      }
    }
  }
}

################################################################################