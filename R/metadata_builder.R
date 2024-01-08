##########################
### metadata_builder() ###
##########################

################################################################################

########################
### File Description ###
########################

# Filename: metadata_builder.R
# Author: Sam McNeely
# Contributors: 
# Date Created: 12/04/2023
# Date Modified: 12/08/2023

# Purpose: 
# The purpose of this script is to build the metadata sheet for the excel file
# that is created in sheet_ui(). This function will create it based on the 
# objects created previously.

# Difference from PhotoRandomizer.R:
# This script creates an object containing the name of the water body sampled.

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

# * Create a function that builds the metadata sheet in the excel file.
# Name the function metadata_builder()
metadata_builder <- function() {
  
  # Read in the metadata excel file
  mdat_full <- read.xlsx(xlsxFile = './Scoring Metadata/RAP_DNR_Metadata.xlsx',
                         sheet = 1)
  
  # * Extract only the metadata needed.
  # Mandatory columns to keep
  cols_to_keep <- c('Date_a.b', 'Location_a.b', 'Site_a.b', 
                    'Random_Assignment_a.b', 'Notes_a.b', 'Warning', 
                    'Check_Warnings')
  
  # * Append cols_to_keep with other variables if they exist
  # Add Habscore_a.b and Max_Habscore if needed
  if(hab_answer == 'Y') {
    
    # Add Habscore_a.b after Random_Assignment_a.b
    append(cols_to_keep, values = 'Habscore_a.b', after = 4)
    
    # Add Max_Habscore after Notes_a.b
    append(cols_to_keep, values = 'Max_Habscore', after = 5)
    
    if(pc_answer == 'Y') {
      
      # Add Percent_Cover_a.b after Habscore_a.b
      append(cols_to_keep, values = 'Percent_Cover_a.b', after = 5)
      
      # Add Max_Percent_Cover after Notes_a.b
      append(cols_to_keep, values = 'Max_Percent_Cover', after = 6)
    }
    
    # Add Percent_Cover_a.b and Max_Percent_Cover if needed
  } else if(pc_answer == 'Y') {
    
    # Add Percent_Cover_a.b after Habscore_a.b
    append(cols_to_keep, values = 'Percent_Cover_a.b', after = 4)
    
    # Add Max_Percent_Cover after Notes_a.b
    append(cols_to_keep, values = 'Max_Percent_Cover', after = 5)
  }

  # Assign the cols_to_keep to mdat
  mdat <- mdat_full[, cols_to_keep]
  
  # Assign mdat to the Global Environment.
  assign(x = 'mdat', value = mdat, envir = .GlobalEnv)
}

################################################################################