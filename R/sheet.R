###############
### sheet() ###
###############

################################################################################

###############
### sheet() ###
###############

# Filename: sheet.R
# Author: Sam McNeely
# Contributors: 
# Date Created: 11/09/2023
# Date Modified: 11/09/2023

# Purpose: 
# The purpose of this script is to create the necessary variables based on user
# inputs to be stored in a dataframe and eventually in a spreadsheet for scoring
# the GoPro images.

# Difference from PhotoRandomizer.R:
# This script uses functions camside(), sites(), and missing_sites() to create 
# the rest of the spreadsheet variables.

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
# Name the function sheet()
sheet <- function() {
  
  # call sampling_date() to set the date sampling occurred
  sampling_date()
  
  # * Both sides need to be evaluated, so camside(), sites(), and the following
  # * if-else statement needs to be run twice (once per camera side).
  
  # get user input for the number of sides that are to be evaluated
  # * The answer variable and its corresponding while loop are reassurances that
  # * the user input the correct entry.
  answer <- ''
  
  while(answer != "Y") {
    
    # Prompt the user for the number of sides to be analyzed
    n <- readline(prompt = "\n\nHow many sides are being analyzed? [1/2] ")
    
    # ask the user if this is correct
    answer <- readline(prompt = cat("\n\nYou are analyzing ", n, " sides. Is that correct? [Y/N] "))
    
    if(answer != 'Y' | answer != 'N') {
      cat('Error: invalid entry.\nEnter "Y" for Yes or "N" for No.')
    } else if(answer == "N") {
      cat('\n\nNumber of sides to be analyzed is incorrect. Reprompting...\n\n')
    } else if(answer == "Y") {
      # leave empty so that the while loop tries to run again but stops because
      # answer != "Y" is FALSE
    }
  }
  
  # create an empty string, n, that will be assigned to the user input
  i <- 0
  
  # use a while loop so that the user is prompted until a specific string, 
  # End, is called to break the loop
  while(i != n) {
    
    # call camside() to determine which side is being evaluated
    camside()
    
    # call sites() to gather the site number information from this specific side
    sites() # the vector needed for the next step is sites
    
    # * Define parameters for this specific camera side
    # Parameters: Date_a/b, Location_a/b, Random_Assignment_a/b, Habscore_a/b, and
    # Notes_a/b
    
    # create an if-statement for if side equals "a" or "b" to define the variable
    # names
    if(side == "a") {
      Date_a <- rep(date, length(sites))
      Location_a <- rep(location, length(sites))
      Random_Assignment_a <- sample(1:length(sites), length(sites))
      Habscore_a <- rep(NA, length(sites))
      Notes_a <- rep("", length(sites))
    } else if(side == "b") {
      Date_b <- rep(date, length(sites))
      Location_b <- rep(location, length(sites))
      Random_Assignment_b <- sample(1:length(sites), length(sites))
      Habscore_b <- rep(NA, length(sites))
      Notes_b <- rep("", length(sites))
    }
    
    # * Create the dataframe for the specific side
    # # create an if-statement for if side equals "a" or "b" to define the
    # dataframe's name
    if(side == "a") {
      df_a <- data.frame(Date_a, Location_a, Site_a, Random_Assignment_a, 
                         Habscore_a, Notes_a)
    } else if(side == "b") {
      df_b <- data.frame(Date_b, Location_b, Site_b, Random_Assignment_b, 
                         Habscore_b, Notes_b)
    }
    
    # * Add in rows for the sites with missing images.
    # call missing_sites() to gather the site number information from this 
    # specific side in which images were not extracted for said site
    missing_sites() # the vector needed for the next step is missing_sites
    
    # append the dataframe with rows for missing images
    if(length(missing_sites) >= 1) {
      
      # append the correct dataframe: df_a or df_b
      if(side == "a") {
        for(i in 1:length(missing_sites)) {
          df_a <- df_a %>%
            add_row(Date_a = Date, Location_a = location, 
                    Site_a = missing_sites[i], Habscore_a = NA, 
                    Notes_a = "Missing image")
        }
      } else if(side == "b") {
        for(i in 1:length(missing_sites)) {
          df_b <- df_b %>%
            add_row(Date_b = Date, Location_b = location, 
                    Site_b = missing_sites[i], Habscore_b = NA, 
                    Notes_b = "Missing image")
        }
      }
    }
    
    # increment i by 1
    i <- i + 1
  }
}

################################################################################