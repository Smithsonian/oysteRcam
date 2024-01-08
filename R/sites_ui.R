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
# Date Created: 11/25/2023
# Date Modified: 12/08/2023

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
# Name the function sites_ui()
sites_ui <- function() {
  
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
    # next are for the sites from which videos were taken.
    cat('\n\nUse the following prompt to enter the site numbers from which videos were recorded.')
    
    # * while loop 1.1 [Site Entry]
    # * Have the user input the sites that were sampled
    # stop_1.1 is the flag for when the user entry is valid, exit the current 
    # while loop
    stop_1.1 <- FALSE
    
    # create an empty vector, sites, to store all of the site numbers
    sites <- c()
    
    while(stop_1.1 == FALSE) {
      
      
      # Prompt the user with 3 options: integers 1-999, the string End, and the 
      # string Remove. 1-999 adds a new site number to the sites vector, End
      # terminates the while loop ending site number additions to the sites 
      # vector, and Remove removes the most recent user entry from the sites 
      # vector.
      input_1 <- readline(prompt = cat('\n\nDESCRIPTION           : [ENTRY]\nSite #s               : [1-999]\nConsecutive Site #s   : [Consecutive]\nFinished adding sites : [End]\nRemove last entry     : [Remove]\n\nSite # :\n'))
      
      # if the user entered End, terminate the while loop
      if(input_1 == 'End') {
        stop_1.1 <- TRUE
        # if the user entered Consecutive, append the sites vector with the
        # consecutive numbers entered by the user 
      } else if(input_1 == 'Consecutive') {  
        
        # * while loop 1.1.1 [Consecutive Site Entry and Verification]
        # * Have the user input the consecutive sites that were sampled
        # stop_1.1.1 is the flag for when the user entry is valid, exit the 
        # current while loop
        stop_1.1.1 <- FALSE
        
        while(stop_1.1.1 == FALSE) {
          
          # * while loop 1.1.1.1 [Consecutive Site Entry: First Site]
          # * Have the user input the consecutive sites that were sampled
          # stop_1.1.1.1 is the flag for when the user entry is valid, exit the 
          # current while loop
          stop_1.1.1.1 <- FALSE
          
          while(stop_1.1.1.1 == FALSE) {
            
            # Prompt the user for the first site number in the consecutive 
            # sequence.
            first <- readline(prompt = cat('\n\nWhat is the first site number in the consecutive sequence? [1-999]\n'))
            
            # If the user entered a whole number between 1 and 999, exit the 
            # current while loop.
            if((!is.na(as.numeric(first))) & (is.wholenumber(as.numeric(first))) & (as.numeric(first) >= 1) & (as.numeric(first) <= 999)) {
              stop_1.1.1.1 <- TRUE
              
              # Any other entry outputs an invalid entry error message.
            } else {
              cat('\n\nError: invalid entry.\nEntry must be a whole number between 1-999.\n')
            }
          }
          
          # * while loop 1.1.1.2 [Consecutive Site Entry: Last Site]
          # * Have the user input the consecutive sites that were sampled
          # stop_1.1.1.2 is the flag for when the user entry is valid, exit the current 
          # while loop
          stop_1.1.1.2 <- FALSE
          
          while(stop_1.1.1.2 == FALSE) {
            
            # Prompt the user for the last site number in the consecutive 
            # sequence.
            last <- readline(prompt = cat('\n\nWhat is the last site number in the consecutive sequence? [1-999]\n'))
            
            # If the user entered a whole number between 1 and 999, exit the 
            # current while loop.
            if((!is.na(as.numeric(last))) & (is.wholenumber(as.numeric(last))) & (as.numeric(last) >= 1) & (as.numeric(last) <= 999)) {
              stop_1.1.1.2 <- TRUE
              
              # Any other entry outputs an invalid entry error message.
            } else {
              cat('\n\nError: invalid entry.\nEntry must be a whole number between 1-999.\n')
            }
          }
          
          # * while loop 1.1.1.3 [Site Verification]
          # * Have the user verify that the sites entered are all correct
          # stop_1.1.1.3 is the flag for when the user entry is valid, exit the current 
          # while loop
          stop_1.1.1.3 <- FALSE
          
          while(stop_1.1.1.3 == FALSE) {
            
            # If the first site number is less than the last site number, then
            # proceed with verification.
            if(as.numeric(first) < as.numeric(last)) {
              cat("\n\nThe first site in the consecutive sequence is ", first, " and the last site is ", last, ".", sep = "")
              
              # Prompt the user to check that their input is correct
              input_2 <- readline(prompt = cat("\n\nIs this correct? [Y/N]\n"))
              
              # If user input is correct (Y), break the current and main while 
              # loops
              if(input_2 == "Y") {
                stop_1.1.1 <- TRUE
                stop_1.1.1.3 <- TRUE
                
                # append the consecutive sites to the sites vector
                sites <- append(sites, as.numeric(first):as.numeric(last))
                
                # If user input is incorrect (N), break the current while loop and 
                # reprompt the user
              } else if(input_2 == "N") {
                stop_1.1.1.3 <- TRUE
                cat('\n\nSites are incorrect. Reprompting...\n')
                
                # If the user entered anything other than "Y" or "N", then print an 
                # error invalid entry message and reprompt the user.
              } else {
                cat('\n\nError: invalid entry.\nEnter "Y" for Yes or "N" for No.\n')
              }
              
              # If the first site number is greater than the last site number, 
              # then prompt an error message that informs the user that the 
              # first site number must be lower than the last site number.
            } else {
              cat('\n\nError: first site is greater than the last site in the consecutive sequence.\nFirst site must be a number lower than the last site.\n')
              
              # Exit the current while loop to reprompt the user for consecutive
              # site input.
              stop_1.1.1.3 <- TRUE
            }
            
          }
          
        }
        
        # if the user entered Remove, remove the most recent entry from the 
        # sites vector
      } else if(input_1 == 'Remove') {
        sites <- sites[-length(sites)]
        
        # add a warning message to ensure the user knows the most recent entry
        # was removed in case it was an accident
        cat('\n\nWarning: most recent entry removed.\n')
        
        # If the user entered any assortment of characters or a number less than
        # or equal to 0, greater than or equal to 1000, inform the user of an
        # invalid input and prompt them again.
      } else if(is.na(as.numeric(input_1)) | (as.numeric(input_1) <= 0) | (as.numeric(input_1) >= 1000) | (!is.wholenumber(as.numeric(input_1)))) {
        cat('\n\nError: invalid entry.\nEntry must be a whole number between 1-999, "End", or "Remove".\n')
        
        # add n (as a numeric, not a character) to the sites vector
      } else {
        sites <- append(sites, as.numeric(input_1))
      }
    }
    
    # * while loop 1.2 [Site Verification]
    # * Have the user verify that the sites entered are all correct
    # stop_1.2 is the flag for when the user entry is valid, exit the current 
    # while loop
    stop_1.2 <- FALSE
    
    while(stop_1.2 == FALSE) {
      cat("\n\nThe sites entered are:\n")
      cat(sites, sep = ", ")
      
      # Prompt the user to check that their input is correct
      input_3 <- readline(prompt = cat("\n\nIs this correct? [Y/N]\n"))
      
      # If user input is correct (Y), break the current and main while loops by
      # setting stop_1 = TRUE
      if(input_3 == "Y") {
        stop_1 <- TRUE
        stop_1.2 <- TRUE
        
        # Use assign() to assign sites to the global environment
        assign(x = 'sites', value = sites, envir = .GlobalEnv)
        
        # If user input is incorrect (N), break the current while loop and 
        # reprompt the user
      } else if(input_3 == "N") {
        stop_1.2 <- TRUE
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