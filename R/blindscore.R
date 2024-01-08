#####################
### blindscore() ###
#####################

################################################################################

########################
### File Description ###
########################

# Filename: blindscore.R
# Author: Sam McNeely
# Contributors: 
# Date Created: 12/11/2023
# Date Modified: 12/11/2023

# Purpose: 
# The purpose of this script is to copy the folder of GoPro images from one side
# and then rename each image according to its corresponding random assignment
# number.

# Place in the data collection process:
# This R script is meant to be run after the excel file has been created and 
# before the images are qualitatively scored.

################################################################################

###########################
### Function Definition ###
###########################
#
# Notes:
#
# Do NOT modify the function definitions. Simply run through the function to
# create it (Ctrl + Enter).

# * Create a function that copies the folder of GoPro images and renames each 
# * image to its corresponding randomly assigned number.
# Name the function blindscore()
blindscore <- function(copypath, sheet) {
  
  ##############
  # Parameters #
  ##############
  # copypath:  The full path to the directory that contains the existing GoPro 
  #            images that were previously extracted from the videos. This path
  #            should end with either '/Side A' or '/Side B'. You may use either
  #            '/' or '\\' to distinguish subdirectories.
  # sheet:     The filename of the .xlsx file created using sheet_ui(). Example:
  #            'BroadCreek_2023308_NoMaxScores.xlsx'
  
  # * Copy and paste the image folder.
  # Create the string that will be the new copied folder.
  pastepath <- paste0(copypath, ' (Blind Scoring)')
  
  # The folder that will be copy and pasted to must be created before it is copy
  # and pasted.
  dir.create(pastepath)
  
  # Copy the folder over to the new folder.
  file.copy(from = list.files(path = copypath, full.names = TRUE),
            to = pastepath, 
            recursive = TRUE)
  
  # * Pattern building for renaming the files
  # Extract the name of the first file in copypath that will be used as the
  # template.
  firstfile <- list.files(copypath)[1]
  
  # Extract the site abbreviation for the files from firstfile.
  siteab <- str_split_i(string = firstfile, pattern = '[0-9]+_', 1)
  
  # Build the pattern for recognition.
  patt <- paste0(siteab, '[0-9]+.*')
  
  # * Rename the files within the pastepath folder.
  # Determine which side is being evaluated to appropriately choose the sheet.
  if(grepl(pattern = '\\Side A', x = copypath) | grepl(pattern = '/Side A', x = copypath)) {
    
    # * Data loading
    # Read in the excel file that contains the random assignments from side A.
    data <- read.xlsx(xlsxFile = paste0('./Data/NoMaxScores/RAP/', sheet), sheet = 'Side A')
    
    # * Data Cleaning
    # Assign the Random_Assigntment_a column to an object
    randassign_a <- data$Random_Assignment_a
    
    # Rename the images within the new folder, pastepath, by using randassign_a
    for(i in 1:length(list.files(pastepath))) {
      file.rename(from = paste0(pastepath, '/', list.files(path = pastepath)[i]),
                  to = paste0(pastepath, '/', randassign_a[i], '.png'))
    }
    
  } else if(grepl(pattern = '\\Side B', x = copypath) | grepl(pattern = '/Side B', x = copypath)) {
    
    # Read in the excel file that contains the random assignments from side B.
    data <- read.xlsx(xlsxFile = paste0('./Data/NoMaxScores/RAP/', sheet), sheet = 'Side B')
    
    # Assign the Random_Assigntment_b column to an object
    randassign_b <- data$Random_Assignment_b
    
    # Rename the images within the new folder, pastepath, by using randassign_b
    for(i in 1:length(list.files(pastepath))) {
      file.rename(from = paste0(pastepath, '/', list.files(path = pastepath)[i]),
                  to = paste0(pastepath, '/', randassign_b[i], '.png'))
    }
  }
}

################################################################################